;;; clocktable.el --- Stepwise clocktable formatter
;; Add #+FILETAGS: writing at the top of any Org file where you want word count tracking active.
;; Clock in and out on headings in that file as usual.
;; The properties START_WORDS, END_WORDS, and WORDS_DELTA will be automatically updated on clock in/out.
;; Other Org files without that filetag will behave normally with no extra properties added.


;;; my-org-clocktable.el --- Custom Org clocktable with word counts -*- lexical-binding: t; -*-

(require 'org)
(require 'cl-lib) ;; for cl-push-tail

(setq org-clock-report-include-clocking-children t)

(defun my/org-get-subtree-word-count ()
  "Return the word count for the current Org heading subtree."
  (save-restriction
    (org-narrow-to-subtree)
    (count-words (point-min) (point-max))))

(defun my/org-file-has-writing-tag-p ()
  "Return t if current Org file has the 'writing' tag in #+FILETAGS."
  (when (buffer-file-name)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+FILETAGS:.*\\bwriting\\b" nil t))))

(defun my/org-clock-in-update-start-words ()
  "On clock-in, store current subtree word count as START_WORDS."
  (when (my/org-file-has-writing-tag-p)
    (let ((wc (my/org-get-subtree-word-count)))
      (org-entry-put nil "START_WORDS" (number-to-string wc)))))

(defun my/org-clock-out-update-end-words ()
  "On clock-out, update END_WORDS and WORDS_DELTA based on subtree word count."
  (when (my/org-file-has-writing-tag-p)
    (let* ((start-wc (string-to-number (or (org-entry-get nil "START_WORDS") "0")))
           (end-wc (my/org-get-subtree-word-count))
           (delta (- end-wc start-wc)))
      (org-entry-put nil "END_WORDS" (number-to-string end-wc))
      (org-entry-put nil "WORDS_DELTA" (format "%+d" delta)))))

(add-hook 'org-clock-in-hook #'my/org-clock-in-update-start-words)
(add-hook 'org-clock-out-hook #'my/org-clock-out-update-end-words)

(defun my/org-clocktable-with-words (ipos tables params)
  "Custom clocktable formatter including START_WORDS, END_WORDS, WORDS_DELTA properties.
Safely handles entries with nil markers to avoid errors."
  (message "Formatter params: %S" params)
  (let* ((data (org-clock-get-table-data nil params)))
    (message "Fetched clock data length: %d" (length data))
    (if (not (listp data))
        ;; No clocked data to show yet
        "| No clocked data available yet. |\n"
      ;; Else build the clocktable output
      (let ((result-table
             (list '("| Heading" "Time" "START_WORDS" "END_WORDS" "WORDS_DELTA" "|")))
            (total-seconds 0))
        (dolist (entry data)
          (let* ((level (nth 0 entry))
                 (heading (nth 1 entry))
                 (time (nth 2 entry))
                 (marker (nth 3 entry))
                 (indent (make-string (* 2 (1- level)) ?\s))
                 (time-str (org-duration-from-minutes (/ time 60.0)))
                 start-words end-words words-delta)
            (message "marker is: %S" marker)
            (if (and marker (markerp marker))
                (progn
                  (setq start-words (org-with-point-at marker (org-entry-get nil "START_WORDS")))
                  (setq end-words   (org-with-point-at marker (org-entry-get nil "END_WORDS")))
                  (setq words-delta (org-with-point-at marker (org-entry-get nil "WORDS_DELTA"))))
              (setq start-words "")
              (setq end-words "")
              (setq words-delta ""))
            (push (list
                   (concat indent heading)
                   time-str
                   (or start-words "")
                   (or end-words "")
                   (or words-delta ""))
                  result-table)
            (setq total-seconds (+ total-seconds time))))
        (setq result-table (reverse result-table))
        (push (list "Total" (org-duration-from-minutes (/ total-seconds 60.0)) "" "" "") result-table)
        (mapconcat (lambda (row)
                     (concat "| " (mapconcat 'identity row " | ") " |"))
                   result-table
                   "\n")))))



;; Example usage:
;; #+BEGIN: clocktable :maxlevel 3 :scope file :formatter my/org-clocktable-with-words
;; #+END:



(provide 'my-org-clocktable)
;;; my-org-clocktable.el ends here
