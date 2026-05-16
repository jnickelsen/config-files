(message "Loading jess-org.el")

(setq org-directory "~/Documents/org")

(require 'cl-lib)

;; making dired open selected in the next window
(setq dired-dwim-target t)
;;;;;;;; making sure text wraps by default
(add-hook 'org-mode-hook #'visual-line-mode)

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


;;;;;;;;; tweaking clocktable
(setq org-clock-report-include-clocking-task t)


;;;;;;;;;; setting details for tags
(setq org-tags-column 0) ; this hopefully puts tags inline with headlines

;;;;;configuration for bullets using org-superstar.
(with-eval-after-load 'org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "☉" "◎" "○" "►" "✲")))

(add-hook 'org-mode-hook #'org-superstar-mode)

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
        '((sequence "TODO" "DRAFT" "EDIT" "|" "FINAL")))

  )

;;;;;;;;;; ORG-REFILE

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(setq org-refile-use-outline-path 'file)              ; HOPEFULLY lets me refile to top level


;;;;;;;;;;ORG SORT BY NUMBER NO PREFIX
;; for work.org - sorting but excluding the client prefix for jira codes

(defun org-sort-by-leading-number-suffix ()
  "Sort Org entries numerically by the number in the prefix at the start of the heading (e.g., MOJ-45)."
  (interactive)
  (org-sort-entries t ?f
                    (lambda ()
                      (let ((heading (org-get-heading t t t t)))
                        ;; Extract the number after the dash in the first word
                        (if (string-match "^[^ ]*-\\([0-9]+\\)" heading)
                            (string-to-number (match-string 1 heading))
                          0)))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s B") #'org-sort-by-leading-number-suffix))

;;;;;;;; setting up export from org to a word document
(defun jess/convert-org-to-docx ()
  "Use Pandoc to convert the current .org buffer to .docx."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input)
                         (format-time-string "-%Y-%m-%d-%H%M%S")
                         ".docx"))
         (cmd (concat "pandoc --from org '" input "' -o '" output "' 2>&1"))
         (result (shell-command-to-string cmd)))
    (if (string-empty-p result)
        (message "Exported to %s" (file-name-nondirectory output))
      (message "Pandoc error: %s" result))))


(with-eval-after-load 'org
  (spacemacs/declare-prefix-for-mode 'org-mode "mP" "pandoc")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "P d" #'jess/convert-org-to-docx))

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
