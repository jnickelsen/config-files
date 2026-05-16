;;; jess-theming.el --- Theme system backed by theme-registry.org -*- lexical-binding: t; -*-

(message "Loading jess-theming.el")

;; ─────────────────────────────────────────────
;; CONFIGURATION
;; ─────────────────────────────────────────────

(defvar jess/theme-registry-file
  (expand-file-name "theme-registry.org" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the org file listing themes with tags.")

;; Evening = 18:00–06:59
(defun jess/evening-p ()
  (let ((h (string-to-number (format-time-string "%H"))))
    (or (>= h 18) (< h 7))))


;; ─────────────────────────────────────────────
;; ORG REGISTRY PARSING
;; Simple regexp approach — no org-element needed.
;; Headlines look like: * theme-name :tag1:tag2:
;; ─────────────────────────────────────────────

(defun jess/registry-all-tags ()
  "Return a sorted list of all unique tags in the theme registry."
  (let (tags)
    (with-temp-buffer
      (insert-file-contents jess/theme-registry-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\* .+?\\(\\(:[a-zA-Z0-9_]+\\)+:\\)" nil t)
        (let ((tag-string (match-string 1)))
          (dolist (tag (split-string (string-trim tag-string ":") ":"))
            (unless (string-empty-p tag)
              (cl-pushnew tag tags :test #'string=))))))
    (sort tags #'string<)))

(defun jess/registry-themes-with-tag (tag)
  "Return a list of theme symbols from the registry that have TAG."
  (let (themes
        (pattern (format "^\\* \\([^ \t]+\\).*:%s:" (regexp-quote tag))))
    (with-temp-buffer
      (insert-file-contents jess/theme-registry-file)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (push (intern (match-string 1)) themes)))
    (nreverse themes)))


;; ─────────────────────────────────────────────
;; THEME LOADING
;; ─────────────────────────────────────────────

(defun jess/load-theme-safely (theme)
  "Disable all active themes, load THEME, then apply face tweaks."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (jess/apply-face-tweaks)
  (message "Theme: %s" theme))

(defun jess/pick-random (list)
  "Return a random element from LIST, or nil if empty."
  (when list
    (nth (random (length list)) list)))


;; ─────────────────────────────────────────────
;; DAILY THEME  (time-based, random from :light or :dark)
;; ─────────────────────────────────────────────

(defun jess/apply-daily-theme ()
  "Load a random light or dark theme depending on time of day."
  (interactive)
  (let* ((tag (if (jess/evening-p) "dark" "light"))
         (candidates (jess/registry-themes-with-tag tag))
         (theme (jess/pick-random candidates)))
    (if theme
        (jess/load-theme-safely theme)
      (message "No themes found for tag: %s" tag))))


;; ─────────────────────────────────────────────
;; TAG-BASED RANDOM  (interactive, SPC T r)
;; ─────────────────────────────────────────────

(defun jess/random-theme-by-tag (tag)
  "Pick a random theme from the registry matching TAG.
With prefix arg, prompt from known tags; otherwise free-type."
  (interactive
   (list (completing-read "Tag: " (jess/registry-all-tags) nil nil)))
  (let* ((candidates (jess/registry-themes-with-tag tag))
         (theme (jess/pick-random candidates)))
    (if theme
        (progn
          (jess/load-theme-safely theme)
          (message "🎲 %s  (tag: %s, %d candidates)" theme tag (length candidates)))
      (message "No themes found for tag: %s" tag))))


;; ─────────────────────────────────────────────
;; FACE TWEAKS  (applied after every theme load)
;; Keeps headings modest — max 1.1× default height.
;; ─────────────────────────────────────────────

(defun jess/apply-face-tweaks ()
  "Enforce sane heading sizes and weights after a theme is loaded."
  ;; Flatten outline faces — themes sometimes scale these wildly
  (dolist (face '(outline-1 outline-2 outline-3 outline-4
                  outline-5 outline-6 outline-7 outline-8))
    (when (facep face)
      (set-face-attribute face nil :height 1.0 :weight 'normal :inherit 'default)))
  ;; Org headings: level 1 slightly larger, rest normal
  (when (facep 'org-level-1)
    (set-face-attribute 'org-level-1 nil :height 1.1 :weight 'bold :inherit 'default))
  (dolist (face '(org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8))
    (when (facep face)
      (set-face-attribute face nil :height 1.0 :weight 'bold :inherit 'default))))


;; ─────────────────────────────────────────────
;; CATPPUCCIN FLAVOURS
;; ─────────────────────────────────────────────

(defun jess/catppuccin (flavour)
  "Load catppuccin with FLAVOUR (latte, mocha, macchiato, frappe)."
  (interactive (list (completing-read "Flavour: " '("latte" "mocha" "macchiato" "frappe"))))
  (setq catppuccin-flavor (intern flavour))
  (jess/load-theme-safely 'catppuccin))


;; ─────────────────────────────────────────────
;; ORG-MODE PRETTINESS
;; ─────────────────────────────────────────────

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑") prettify-symbols-alist)
            (push '("[-]" . "❍") prettify-symbols-alist)
            (prettify-symbols-mode 1)
            (when (fboundp 'org-bullets-mode)
              (org-bullets-mode 1))))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A")))
  "Face for the text part of a checked org-mode checkbox.")


;; ─────────────────────────────────────────────
;; KEYBINDINGS
;; ─────────────────────────────────────────────

(spacemacs/set-leader-keys
  "T d"   #'jess/apply-daily-theme       ;; today's random theme
  "T r"   #'jess/random-theme-by-tag     ;; random by tag
  "T c"   #'jess/catppuccin)             ;; catppuccin flavour picker


;; ─────────────────────────────────────────────
;; STARTUP + AUTO-REFRESH
;; ─────────────────────────────────────────────

;; Switch at midnight and re-check every hour
(run-at-time "00:00" 86400 #'jess/apply-daily-theme)
(run-at-time nil 3600 #'jess/apply-daily-theme)

;; Apply on startup
(jess/apply-daily-theme)


(provide 'jess-theming)
;;; jess-theming.el ends here
