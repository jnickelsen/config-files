;;; org-jump.el --- Indirect buffer jumping for Org headlines

(defvar ab/org-origin-marker nil
  "Marker pointing to the original heading in the base buffer.")

(defun ab/org-unfold-headline-and-narrow ()
  "Open the current Org headline in an indirect buffer and unfold its contents."
  (interactive)
  (when (org-at-heading-p)
    (let ((origin (point-marker)))
      (org-tree-to-indirect-buffer)
      (setq ab/org-origin-marker origin)
      (outline-show-entry)
      (org-cycle-hide-drawers 'children))))

(defun ab/org-return-from-indirect-buffer ()
  "Return to the base buffer from an indirect buffer and restore point."
  (interactive)
  (when (buffer-base-buffer)
    (let ((origin-marker ab/org-origin-marker)
          (base (buffer-base-buffer)))
      (kill-buffer)
      (switch-to-buffer base)
      (widen)
      (when (and origin-marker
                 (marker-buffer origin-marker)
                 (buffer-live-p (marker-buffer origin-marker)))
        (goto-char origin-marker)
        (recenter))
      (setq ab/org-origin-marker nil))))

(defun ab/org-smart-return ()
  "If on a heading, open in indirect buffer; if in indirect buffer, return; else default RET."
  (interactive)
  (if (org-at-heading-p)
      ;; If we're at a headline, open in indirect buffer or return
      (cond
       ((and (buffer-base-buffer) ab/org-origin-marker)
        (ab/org-return-from-indirect-buffer))
       ((org-at-heading-p)
        (ab/org-unfold-headline-and-narrow)))
    ;; Else just do the default RET behavior when we're not on a heading
    (org-return)))



(define-minor-mode org-jump-mode
  "Minor mode for jumping into and out of Org subtrees."
  :lighter " ↪"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Set RET in evil normal and insert states
            (with-eval-after-load 'evil
              (evil-define-key 'normal map (kbd "RET") #'ab/org-smart-return)
              (evil-define-key 'insert map (kbd "RET") #'ab/org-smart-return))
            map))




(provide 'org-jump)
;;; org-jump.el ends here
