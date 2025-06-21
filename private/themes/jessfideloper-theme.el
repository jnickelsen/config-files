(deftheme jessfideloper-theme "A custom theme based on the Fideloper macOS Terminal theme.")

(let ((class '((class color) (min-colors 89)))
      (bg "#292F32") ;; Replace with your Terminal background color
      (fg "#DBDADF") ;; Replace with your Terminal foreground color
      (black "#292F32") ;; Example ANSI colors
      (red "#CC1D2C")
      (green "#EDB8AC")
      (yellow "#B7AB9B")
      (blue "#2E79C2")
      (magenta "#C1226F")
      (cyan "#319286")
      (white "#E9E3CE"))

  (custom-theme-set-faces
   'jessfideloper
   `(default ((,class (:background ,bg :foreground ,fg))))
   ;; `(cursor ((,class (:background ,fg))))
   `(cursor ((t (:foreground "green"))))
   `(region ((,class (:background ,black :foreground ,white))))
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:foreground ,green))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-string-face ((,class (:foreground ,red))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))
   `(mode-line ((,class (:background ,black :foreground ,white))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jessfideloper)
