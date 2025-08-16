;;; jess-theming.el --- Custom theming and palette management -*- lexical-binding: t; -*-

(message "Loading jess-theming.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Favorite Themes + Helm interface
(defvar my/favorite-themes
  '(spacemacs-light organic-green zenburn toxi ef-trio-light ef-trio-dark
                    ef-summer ef-spring ef-rosa ef-reverie ef-elea-light
                    ef-elea-dark ef-dream doom-solarized-dark doom-rouge
                    doom-oksolar-dark doom-monokai-octagon doom-miramare
                    doom-laserwave doom-henna doom-flatwhite doom-solarized-light
                    doom-ayu-light leuven modus-operandi wombat)
  "Themes I actually use and want to see in Helm.")

(defun my/helm-themes-curated ()
  "Helm interface limited to themes I like."
  (interactive)
  (helm :sources
        (helm-build-sync-source "My Themes"
          :candidates (mapcar #'symbol-name my/favorite-themes)
          :action (lambda (theme) (load-theme (intern theme) t))
          :persistent-action (lambda (theme) (load-theme (intern theme) t)))
        :buffer "*helm my themes*"))

(spacemacs/set-leader-keys "a T" #'my/helm-themes-curated)

(defun my/add-theme-to-favorites (theme)
  "Add THEME to `my/favorite-themes`."
  (interactive
   (list (intern (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))))))
  (add-to-list 'my/favorite-themes theme)
  (message "Added %s to favorites." theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan mode tweaks
(when (fboundp 'nyan-mode)
  (nyan-mode 1)
  (setq nyan-bar-length (if (member (format-time-string "%A") '("Friday" "Saturday"))
                            24  ; longer bar on weekends
                          16))
  (setq nyan-wavy-trail (member (format-time-string "%A") '("Thursday" "Sunday"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight Changes faces
(custom-set-faces
 '(highlight-changes ((t (:foreground "#AE0073" :background "#FCDAE9"))))
 '(highlight-changes-delete ((t (:underline t :foreground "#AE0073" :background "#FCDAE9")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode prettify checkboxes
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑") prettify-symbols-alist)
            (push '("[-]" . "❍") prettify-symbols-alist)
            (prettify-symbols-mode 1)
            (org-bullets-mode 1)))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A")))
  "Face for the text part of a checked org-mode checkbox.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org heading palettes (7+ colors each)
(defun col-strip (col-str)
  "Convert hyphen-separated hex colors string COL-STR into list of #rrggbb colors."
  (mapcar (lambda (x) (concat "#" x))
          (split-string col-str "-")))

(defvar my-org-heading-palettes
  (mapcar (lambda (pair)
            (cons (car pair) (col-strip (cadr pair))))
          '(("Signal Path" "3D0B0A-7D1614-D52425-2D9693-26C9CB-94B7B3-5ABABC")
            ("Jess theme" "2E936C-5FAD41-75B9BE-391463-3B0942-C64291-EFA8B8")
            ("Volcanic Prism" "1F0602-3D0B0A-5E1119-7D1614-D12725-D52425-9CB5B4")
            ("Thermal Bloom" "D12725-5E1119-94B7B3-2D9693-5ABABC-26C9CB-9CB5B4")
            ("Muted Pastels" "6D6875-B5838D-E5989B-FFCDB2-FFB4A2-F4A261-E76F51-2A9D8F")
            ("Tokyo Dusk" "1A1B26-414868-7AA2F7-9ECE6A-E0AF68-F7768E-B48EAD-86BBD8")
            ("Dracula Pop" "282A36-6272A4-FF79C6-8BE9FD-50FA7B-F1FA8C-BD93F9-FFB86C")
            ("Lavender & Purples" "6A1B9A-8E24AA-AB47BC-BA68C8-CE93D8-E1BEE7-9C27B0-7B1FA2")
            ("Seaside Pebbles (Contrast)" "2F4F4F-4B5D67-5F738A-708090-8899A6-AAB9C9-DDE5EC-F0F8FF")
            ("Warm Cinnamon" "7B3F00-A0522D-CD853F-DEB887-F4A460-DAA520-B8860B-8B4513")
            ("Forest Lanterns" "1B2E2B-3E5641-7D8F69-A0C18F-FFD275-FFB347-F79824-F57F17")
            ("Frosted Petals" "F8E8EE-E0C9D9-D5A6BD-C085B3-9B89B3-7C90A0-5F7A84-455B66")
            ("Mars Static" "2D0605-591B1B-7F2A2A-AA3939-CC4E4E-E27C7C-FFB9A3-FFF3E0")
            ("Mars Static edit" "2D0605-742020-AA3939-DD665A-F7977A-FBBFA1-FFE6C5-FFF9F3")
            ("Mars Static (Cooler)" "2B0A0E-4F1C1F-804C49-AB7E6B-C3A799-DBCDC3-E4E7E5-FFFFFF")
            ("Mars Static (Duskier)" "1A0B1C-3B1C2F-6B324F-914F65-BF6E5F-DA9878-F1C9B0-FDF4E9")
            ("Mars Static (Violet-Gold)" "3B0D3F-6A1D6E-9B3E9C-D3678F-EC9A7F-F5C979-FCEBB3-FFFFEA")
            ("Ink and Ether" "0D0D0D-1C1C1C-3A3A3A-4C5C4C-6C8C6C-9ACAA0-C2E8C2-EFFFEF")
            ("Citrine to Coal" "FDF6B2-FAD643-FACA15-F0B500-DE9E1A-B07826-624F29-3A3A3A")
            ("Deep Tide" "003366-005577-007788-339999-66CCCC-99CCFF-B0C4DE-DDEEFF")))
  "Alist of named org heading palettes with list of colors.")

(defvar my-org-current-palette-index 0
  "Current index into `my-org-heading-palettes'.")


;; ;; Custom face for =highlighted= Org text (DISABLED to use theme default)
;; (defface my/org-highlighted-text
;;   '((t (:box (:line-width -1 :color "#FFAEB9")
;;              :background "#FFD6C9"
;;              :inherit nil
;;              :extend nil)))
;;   "Custom face for =highlighted= Org text.")

;; (defun my/update-org-highlighted-text-face (palette)
;;   "Update =highlighted= org face colors using PALETTE."
;;   (let ((bg (or (nth 5 palette) "#FFD6C9"))   ;; 6th color for background
;;         (box (or (nth 2 palette) "#FFAEB9"))) ;; 3rd color for box
;;     (set-face-attribute 'my/org-highlighted-text nil
;;                         :background bg
;;                         :box `(:line-width -1 :color ,box)
;;                         :foreground 'unspecified
;;                         :weight 'normal)))

(defun my/apply-org-heading-palette (index-or-name)
  "Apply the Org heading color palette by INDEX (number) or NAME (string)."
  (interactive
   (list (completing-read "Choose palette: "
                          (mapcar #'car my-org-heading-palettes)
                          nil t
                          (nth my-org-current-palette-index (mapcar #'car my-org-heading-palettes)))))
  (let* ((names (mapcar #'car my-org-heading-palettes))
         (pos (cond
               ((stringp index-or-name)
                (or (cl-position index-or-name names :test #'string=)
                    0))
               ((integerp index-or-name)
                (if (and (>= index-or-name 0)
                         (< index-or-name (length my-org-heading-palettes)))
                    index-or-name
                  0))
               (t 0)))
         (color-theme (cdr (nth pos my-org-heading-palettes))))
    (if color-theme
        (progn
          (setq my-org-current-palette-index pos)
          (dotimes (i (min (length color-theme) 8))
            (let ((face (intern (format "org-level-%d" (1+ i)))))
              (set-face-attribute face nil
                                  :foreground (nth i color-theme))))
          ;; (my/update-org-highlighted-text-face color-theme)
          (message "Applied palette: %s" (nth pos names)))
      (error "No palette found at index %s" index-or-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown palette application (safe, deferred)
(defun my/get-org-palette-colors (palette-name)
  "Return list of hex colors for PALETTE-NAME from `my-org-heading-palettes`."
  (cdr (assoc palette-name my-org-heading-palettes)))

(defun my/apply-markdown-palette-by-name (palette-name)
  "Apply colors from PALETTE-NAME to markdown faces using a clean mapping."
  (when (featurep 'markdown-mode)
    (let ((colors (my/get-org-palette-colors palette-name))
          (mapping '((markdown-header-face-1 . 1)
                     (markdown-header-face-2 . 2)
                     (markdown-header-face-3 . 3)
                     (markdown-link-face     . 4)
                     (markdown-code-face     . 5)
                     (markdown-bold-face     . 6)
                     (markdown-italic-face   . 6)
                     (markdown-markup-face   . 2)
                     (font-lock-keyword-face . 2)
                     (markdown-blockquote-face . 3))))
      (when (and colors (>= (length colors) 7))
        (dolist (pair mapping)
          (let* ((face (car pair))
                 (color-idx (cdr pair))
                 (color (nth (1- color-idx) colors))) ;; 1-based to 0-based index
            (when color
              (set-face-attribute face nil
                                  :foreground (if (member face
                                                          '(markdown-code-face))
                                                  nil
                                                color)
                                  :background (when (eq face 'markdown-code-face) color)
                                  :weight (if (member face '(markdown-header-face-1 markdown-bold-face))
                                              'bold
                                            'normal)
                                  :slant (if (eq face 'markdown-italic-face)
                                             'italic
                                           'normal)
                                  :underline (if (eq face 'markdown-link-face)
                                                 t
                                               nil)))))))))

;; This defers applying markdown palette if markdown-mode not loaded yet
(defun my/apply-all-palettes (palette-name)
  "Apply org heading and markdown palettes for PALETTE-NAME safely."
  (let ((palette (cdr (assoc palette-name my-org-heading-palettes))))
    (when palette
      (my/apply-org-heading-palette palette-name)
      ;;(my/update-org-highlighted-text-face palette)
      (if (featurep 'markdown-mode)
          (my/apply-markdown-palette-by-name palette-name)
        (add-hook 'markdown-mode-hook
                  (lambda ()
                    (my/apply-markdown-palette-by-name palette-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Palette cycling
(defun my/cycle-org-heading-palettes ()
  "Cycle to the next Org heading color palette and apply markdown palette too."
  (interactive)
  (setq my-org-current-palette-index
        (mod (1+ my-org-current-palette-index) (length my-org-heading-palettes)))
  (my/apply-all-palettes (nth my-org-current-palette-index (mapcar #'car my-org-heading-palettes))))

(with-eval-after-load 'org
  (my/apply-all-palettes (nth my-org-current-palette-index (mapcar #'car my-org-heading-palettes)))
  (spacemacs/set-leader-keys "o p" #'my/cycle-org-heading-palettes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Daily themes + palettes
(defvar jess/day-theme-palette-alist
  '(("Monday"    . (:theme doom-flatwhite        :palette "Mars Static Edit)"))
    ("Tuesday"   . (:theme doom-solarized-light  :palette "Muted Pastels"))
    ("Wednesday" . (:theme spacemacs-light       :palette "Lavender & Purples"))
    ("Thursday"  . (:theme ef-reverie            :palette "Lavender & Purples"))
    ("Friday"    . (:theme ef-maris-light        :palette "Frosted Pastels"))
    ("Saturday"  . (:theme organic-green         :palette "Deep Tide")) ; <3
    ("Sunday"    . (:theme ef-summer             :palette "Mars Static (Violet-Gold)")) ; <3
    (_           . (:theme organic-green         :palette "Jess theme"))))

(defvar jess/night-theme-palette-alist
  '(("Monday"    . (:theme doom-solarized-dark   :palette "Muted Pastels")) ; <3 this one
    ("Tuesday"   . (:theme doom-miramare         :palette "Thermal Bloom"))
    ("Wednesday" . (:theme ef-elea-dark          :palette "Frosted Pastels"))
    ("Thursday"  . (:theme ef-dream              :palette "Thermal Bloom"))
    ("Friday"    . (:theme doom-henna            :palette "Citrine to Coal"))
    ("Saturday"  . (:theme wombat                :palette "Citrine to Coal"))
    ("Sunday"    . (:theme doom-solarized-dark   :palette "Lavender & Purples")) ; <3
    (_           . (:theme wombat                :palette "Jess theme"))))

(defun evening-hours-p ()
  "Return t if current time is between 6pm and 7am."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 18) (< hour 7))))

(defun jess/apply-daily-theme-and-palette ()
  "Load theme and org heading + markdown palettes based on day and time."
  (interactive)
  (let* ((day (format-time-string "%A"))
         (alist (if (evening-hours-p)
                    jess/night-theme-palette-alist
                  jess/day-theme-palette-alist))
         (entry (or (assoc day alist)
                    (assoc '_' alist)))
         (theme (plist-get (cdr entry) :theme))
         (palette-name (plist-get (cdr entry) :palette))
         (palette (cdr (assoc palette-name my-org-heading-palettes))))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (load-theme theme t)
      (message "🖌 Loaded theme: %s" theme))
    (when palette
      (my/apply-all-palettes palette-name)
      (message "🎨 Applied org heading & markdown palettes: %s" palette-name))))

(spacemacs/set-leader-keys "t d" #'jess/apply-daily-theme-and-palette)

;; Schedule theme + palette update every hour and at midnight
(run-at-time nil 3600 #'jess/apply-daily-theme-and-palette)
(run-at-time "00:00" 86400 #'jess/apply-daily-theme-and-palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org emphasis + prettify tweaks
(setq org-hide-emphasis-markers t)
(setq org-emphasis-alist
      '(("*" bold nil)
        ("/" italic nil)
        ("_" underline nil)
        ;;        ("=" my/org-highlighted-text nil) ;; COMMENTED OUT to use theme default
        ("=" org-verbatim verbatim) ;; this is the default
        ("~" org-verbatim verbatim)
        ("+" (:strike-through t) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak the modeline if there are problems distinguishing between windows
;; (set-face-foreground 'modeline "white")
;; (set-face-background 'modeline "purple")
;; (set-face-background 'modeline-inactive "light blue")


(provide 'jess-theming)
;;; jess-theming.el ends here
