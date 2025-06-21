(message "Loading jess-org.el")


(setq org-directory "~/Documents/org")

;;;;;;;;;; cleaning up some errors
(with-eval-after-load 'org-agenda
  ;; Prevent Spacemacs from trying to remap these specific keys
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "C-n") 'org-agenda-next-line
    (kbd "G") 'org-agenda-toggle-time-grid
    (kbd "|") 'org-agenda-filter-remove-all
    (kbd "\\") 'org-agenda-filter-by-tag))



;;;;;configuration for org and org-roam, etc.
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☉" "◎" "○" "►" "✲")))

;;;;;;;;;; setting up org capture, agenda, and other stuff
(with-eval-after-load 'org
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %?")
          ("m" "meeting notes" entry (file+olp+datetree ,(concat org-directory "/meeting-notes.org"))
           "* %?")
          ("w" "writing todo" entry (file ,(concat org-directory "/writing-tasks.org"))
           "* TODO %? :writing:\n")
          ("l" "life todo" entry (file ,(concat org-directory "/inbox.org"))
           "* TODO %? :life:\n")
          ("t" "todo" entry (file+headline "/notes.org" "tasks")
           "* TODO %?\n  %i\n  %a")
          ))
  (setq org-todo-keywords
        '((sequence "TODO" "ZERO DRAFT" "FIRST DRAFT" "SECOND DRAFT" "DONE"))
        )
  )


;;;;;;;;;; ORG ROAM
(setq org-roam-directory "~/Documents/org/ctl+ink")
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
