(message "Loading jess-org.el")

(setq org-directory "~/Documents/org")

;;;;;;;;;; trying to stop files opening in another window by default
(setq org-link-frame-setup '((file . find-file)))

;;;;;;;;;; I was getting 'ghosting' of leading stars with bullets
(setq org-hide-leading-stars t)

;;;;;;;;;; cleaning up any missing org agenda files on emacs startup
(with-eval-after-load 'org
  (setq org-agenda-files
        (cl-remove-if-not #'file-exists-p org-agenda-files)))

;;;;;;;;;; cleaning up some errors
(with-eval-after-load 'org-agenda
  ;; Prevent Spacemacs from trying to remap these specific keys
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "C-n") 'org-agenda-next-line
    (kbd "G") 'org-agenda-toggle-time-grid
    (kbd "|") 'org-agenda-filter-remove-all
    (kbd "\\") 'org-agenda-filter-by-tag))

;;;;;;;;;; headline sizes in org mode
(with-eval-after-load 'org
  ;; Top-level heading (* Heading): slightly larger and bold
  ;; edit: this was :height 1.1 but I am changing it back to 1.0
  (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'bold)

  ;; All other heading levels: normal size and bold
  (dolist (face '(org-level-2 org-level-3 org-level-4
                              org-level-5 org-level-6 org-level-7 org-level-8))
    (set-face-attribute face nil :height 1.0 :weight 'bold)))

;;;;;;;;;; setting details for tags
(setq org-tags-column 0) ; this hopefully puts tags inline with headlines

;;;;;configuration for org and org-roam, etc.
(with-eval-after-load 'org
  ;;  (org-superstar-mode -1) ;; superstar mode was overriding my bullets!

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "☉" "◎" "○" "►" "✲")))
  )
;;;;;;;;;; superstar mode
(setq org-superstar-remove-leading-stars t
      org-superstar-leading-bullet " "
      org-hide-leading-stars t)

;;;;;;;;;; setting up org capture, agenda, and other stuff
(with-eval-after-load 'org
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %? :inbox:\n")
          ("w" "writing todo" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %? :writing:\n")
          ("l" "life todo" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %? :life:\n")
          ("t" "todo" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %? :todo:\n")
          ))
  (setq org-todo-keywords
        '((sequence "TODO" "ZERO DRAFT" "FIRST DRAFT" "SECOND DRAFT" "DONE"))
        )
  )


;;;;;;;;;;;WRITING CLOCKTABLE
;; Run M-x my-writing-tracking-toggle to turn tracking on or off. Clock in/out as usual.
;; When on, clock-out will add the extra word count lines in the LOGBOOK after the clock entry.

;; each org file that is tracking writing will need the following set up as the clocktable:

;; #+BEGIN: writing-clocktable :maxlevel 2 :block thisweek
;; #+END:

;; Workflow:
;; Open a writing file.
;; M-x my-writing-tracking-toggle → ON
;; Clock in to a heading → write/edit.
;; Clock out → LOGBOOK gets:
;; CLOCK: [2025-08-08 Fri 10:00]--[2025-08-08 Fri 11:30] =>  1:30
;; - Words total: 3240
;; - Words change: +450
;; Refresh your #+BEGIN: writing-clocktable … → table shows time, total words, and change.


(defvar my-writing-tracking-mode nil
  "If non-nil, record word count and change on clock-out.")

(defun my-writing-tracking-toggle ()
  "Toggle writing tracking mode for clock-out."
  (interactive)
  (setq my-writing-tracking-mode (not my-writing-tracking-mode))
  (message "Writing tracking mode %s"
           (if my-writing-tracking-mode "ON" "OFF")))

(defun my-writing-word-count ()
  "Count words in the current org heading's subtree."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point))
          (end (org-end-of-subtree t)))
      (count-words start end))))

(defun my-writing-last-total-in-logbook ()
  "Get the last recorded 'Words total' in this heading's LOGBOOK."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (org-end-of-subtree t)))
      (when (re-search-forward "- Words total: \\([0-9]+\\)" end t)
        (string-to-number (match-string 1))))))

(defun my-writing-log-word-data ()
  "If `my-writing-tracking-mode' is on, log total and change in LOGBOOK with timestamp."
  (when my-writing-tracking-mode
    (let* ((total (my-writing-word-count))
           (prev-total (or (my-writing-last-total-in-logbook) total))
           (change (- total prev-total))
           (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (save-excursion
        (org-back-to-heading t)
        ;; Find the existing LOGBOOK drawer, if any
        (if (re-search-forward "^:LOGBOOK:" (save-excursion (org-end-of-subtree t) (point)) t)
            (progn
              ;; Move inside the LOGBOOK drawer
              (forward-line)
              ;; Insert under the last entry but before :END:
              (if (re-search-forward "^:END:" (save-excursion (org-end-of-subtree t) (point)) t)
                  (progn
                    (beginning-of-line)
                    (insert (format "- Words total: %d %s\n- Words change: %+d %s\n"
                                    total timestamp change timestamp)))
                ;; If :END: not found, just insert
                (insert (format "- Words total: %d %s\n- Words change: %+d %s\n"
                                total timestamp change timestamp))))
          ;; If no LOGBOOK found, create one after the clock entry
          (progn
            (org-end-of-subtree t)
            (insert "\n:LOGBOOK:\n")
            (insert (format "- Words total: %d %s\n- Words change: %+d %s\n"
                            total timestamp change timestamp))
            (insert ":END:\n")))))))


;;;;;;;;;;THE WRITING CLOCKTABLE BLOCK
(defun org-dblock-write:writing-clocktable (params)
  "Generate a clocktable with word counts and changes."
  (let* ((data (org-clock-get-table-data (current-buffer) params))
         (total-time (nth 1 data))
         (entries (nth 2 data)))
    (insert "| Headline | Time | Words total | Words change |\n|-\n")
    (dolist (entry entries)
      (let* ((headline (nth 1 entry))
             (time-min (nth 3 entry))
             (marker (nth 5 entry))
             (words-total (save-excursion
                            (goto-char marker)
                            (or (my-writing-last-total-in-logbook) "")))
             (words-change (save-excursion
                             (goto-char marker)
                             (when (re-search-forward "- Words change: \\([+-]?[0-9]+\\)"
                                                      (org-end-of-subtree t) t)
                               (match-string 1)))))
        (insert (format "| %s | %d:%02d | %s | %s |\n"
                        headline (/ time-min 60) (mod time-min 60)
                        words-total words-change))))
    (insert "|-\n")
    (insert (format "| *Total* | %d:%02d |   |   |\n"
                    (/ total-time 60) (mod total-time 60)))))

(add-hook 'org-clock-out-hook #'my-writing-log-word-data)

;;;;;;;;;; ORG ROAM
(setq org-roam-directory "~/Documents/org/org-roam")
(setq org-roam-v2-ack t)
(setq org-roam-completion-everywhere t)
(org-roam-db-autosync-mode)
;;;;;;;;;;; setting up capture templates
(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
        '(("c" "concept" plain "%?"
           :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: * %^{Title}\n#+filetags: concept\n\n")
           :unnarrowed t)
          ("r" "course" plain "%?"
           :target (file+head "courses/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: # %^{Title}\n#+filetags: course\n\n")
           :unnarrowed t)
          ("n" "note" plain "%?"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: @ %^{Title}\n#+filetags: note\n\n")
           :unnarrowed t)
          ("t" "tool" plain "%?"
           :target (file+head "tools/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: $ %^{Title}\n#+filetags: tool\n\n")
           :unnarrowed t)
          ("x" "technique" plain "%?"
           :target (file+head "techniques/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: + %^{Title}\n#+filetags: technique\n\n")
           :unnarrowed t)
          ("i" "inspiration" plain "%?"
           :target (file+head "inspirations/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: % %^{Title}\n#+filetags: inspiration\n\n")
           :unnarrowed t)
          ("e" "experiment" plain "%?"
           :target (file+head "experiments/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ! %^{Title}\n#+filetags: experiment\n\n")

           :unnarrowed t)
          ("p" "project" plain "%?"
           :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ^ %^{Title}\n#+filetags: project\n\n")

           :unnarrowed t)
          ("l" "log" plain "%?"
           :target (file+head "logs/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ~ %^{Title}\n#+filetags: log\n\n")
           :unnarrowed t)))
  )

;;;;;;;;;;using org-side-tree
;;;;;;;;;;see https://www.youtube.com/watch?v=c3QLfl9_D5Y for demo!
(setq org-side-tree-timer-interval 2.0)

(with-eval-after-load 'org
  (require 'org-side-tree)

  ;; Wrap the timer function to ignore the "Invalid search bound" error
  (defun my/org-side-tree-timer-function-safe ()
    (ignore-errors
      (org-side-tree-timer-function)))

  ;; Cancel any existing timers on the original function
  (cancel-function-timers 'org-side-tree-timer-function)
  ;; Restart the timer with the safe wrapper
  (run-with-timer 0 org-side-tree-timer-interval #'my/org-side-tree-timer-function-safe)

  (add-hook 'org-side-tree-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") #'org-side-tree-visit)))
  (setq org-side-tree-persistent t)
  (setq outline-minor-mode-cycle t)
  (spacemacs/set-leader-keys "a s" #'org-side-tree))
;; can also
(setq org-side-tree-narrow-on-jump nil)


;; ;; this is the indent sorting
(setq org-list-indent-offset 0)

(electric-indent-mode -1) ;; disable electric indent globally for org


;;;;;;;; my/org-narrow-to-subtree action
;;;;;;;; currently am commenting this out; seeing if org-side-tree will do
;;;;;;;; everything I need it to.
;; (defun my/org-narrow-to-subtree ()
;;   "Open the current Org subtree in a right-hand window using an indirect buffer, fully expanded."
;;   (interactive)
;;   (unless (derived-mode-p 'org-mode)
;;     (user-error "Not in an Org buffer"))

;;   (let* ((base-buf (current-buffer))
;;          (base-name (buffer-name))
;;          (heading (nth 4 (org-heading-components)))
;;          (indirect-name (format "*%s: %s*" base-name heading))
;;          (existing-window (get-buffer-window indirect-name t))
;;          (indirect-buf (or (get-buffer indirect-name)
;;                            (make-indirect-buffer base-buf indirect-name t))))

;;     (with-current-buffer base-buf
;;       (org-mode))  ; Ensure org-mode overlays are restored

;;     (with-current-buffer indirect-buf
;;       (org-mode)
;;       (widen)
;;       (org-narrow-to-subtree)
;;       (org-show-all))  ; show all (as opposed to org-show-subtree)


;;     ;; Create or reuse right window
;;     (let ((right-win
;;            (or (window-in-direction 'right)
;;                (split-window-right))))
;;       (set-window-buffer right-win indirect-buf)
;;       (select-window right-win))))

;; ;;;; Bind the function to a custom shortcut key, e.g., C-c n
;; (global-set-key (kbd "C-c n") 'my/org-narrow-to-subtree)

(provide 'jess-org)
