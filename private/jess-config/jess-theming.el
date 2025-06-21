(message "Loading jess-theming.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; custom helm list of themes
(defvar my/favorite-themes
  '(spacemacs-light organic-green zenburn toxi ef-trio-light ef-trio-dark ef-summer ef-spring ef-rosa ef-reverie ef-elea-light ef-elea-dark ef-dream doom-solarized-dark doom-rouge doom-oksolar-dark doom-monokai-octagon doom-miramare doom-laserwave doom-henna doom-flatwhite doom-solarized-light doom-ayu-light leuven modus-operandi wombat)

  "Themes I actually use and want to see in Helm.")

(defun my/helm-themes-curated ()
  "Helm interface limited to themes I like."
  (interactive)
  (helm :sources
        (helm-build-sync-source "My Themes"
          :candidates (mapcar #'symbol-name my/favorite-themes)
          :action (lambda (theme)
                    (load-theme (intern theme) t))
          :persistent-action (lambda (theme)
                               (load-theme (intern theme) t)))
        :buffer "*helm my themes*"))

(spacemacs/set-leader-keys "a T" #'my/helm-themes-curated)

;;; and to add them to favourites when you come across them:
(defun my/add-theme-to-favorites (theme)
  "Add THEME to `my/favorite-themes`."
  (interactive
   (list (intern (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))))))
  (add-to-list 'my/favorite-themes theme)
  (message "Added %s to favorites." theme))
;;
;; to add the favourites list to my config, (as it will just be in session memory):
;; run M-: eval-expression, and enter:
;; (with-output-to-temp-buffer "*My Favorite Themes*"
;;   (princ my/favorite-themes))
;; then copy and paste the result into the function above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;NYAN GOODNESS
(when (fboundp 'nyan-mode)
  (nyan-mode 1)
  )
;; (setq nyan-bar-length 20) ;; optional: makes the cat’s trail longer
;; (setq nyan-wavy-trail nil) ;; optional: no wave
;; (setq nyan-animate-nyancat t) ;; optional: keep animation
;; (setq nyan-cat-face 'nyan-cat-face) ;; set explicitly
;;(setq nyan-rainbow-bar 'off) ;; <-- THIS turns off rainbow mode!

(setq nyan-bar-length (if (member (format-time-string "%A") '("Friday" "Saturday"))
                          24  ; longer bar for weekend flex
                        16))

(setq nyan-wavy-trail (member (format-time-string "%A") '("Thursday" "Sunday")))


  ;;;;;;;;;;HIGHLIGHT CHANGES
(custom-set-faces
 '(highlight-changes
   ((t (:foreground "#AE0073" :background "#FCDAE9"))))
 '(highlight-changes-delete
   ((t (:underline t :foreground "#AE0073" :background "#FCDAE9")))))

;;;;;;;;;; making pretty checkboxes
  ;;; see https://symbl.cc/en/collections/list-bullets/ for more

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode t)
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (prettify-symbols-mode)
                           (org-bullets-mode 1)
                           ))


(defface org-checkbox-done-text
  '((t (:foreground "#71696A")))
  "Face for the text part of a checked org-mode checkbox.")


;;;;;;;;;;  headline colours
;;;;;;;;;;  https://www.reddit.com/r/emacs/comments/rg9ojl/a_workflow_to_quickly_change_orgmode_section/
;;;;instructions:
;; 1 First, generate a palette you like from this color palette generator: https://coolors.co
;; 2 Copy the RGB color codes, separated by hyphens, from the url
;; 3 Paste it into the list in the same format as I have in the below code snippet
;; 4 Copy the code into your init file somewhere
;; 5 Essentially, the code splits the string into a list of codes, then the face attributes set the org header colors to the corresponding list colors.
;; 6. in the =setq pick-color= selection add the number of your color scheme (starting with 0)

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
            ("Deep Tide" "003366-005577-007788-339999-66CCCC-99CCFF-B0C4DE-DDEEFF")))
  "Alist of named org heading palettes with list of colors.")

(defvar my-org-current-palette-index 0
  "Current index into `my-org-heading-palettes'.")

(defface my/org-highlighted-text
  '((t (:box (:line-width -1 :color "#FFAEB9")
             :background "#FFD6C9"
             :inherit nil
             :extend nil)))
  "Custom face for =highlighted= Org text.")

(defun my/update-org-highlighted-text-face (palette)
  "Update =highlighted= org face colors using PALETTE."
  ;; Choose colors from palette with fallback defaults
  (let ((bg (or (nth 5 palette) "#FFD6C9"))   ;; 6th color for background
        (box (or (nth 2 palette) "#FFAEB9"))) ;; 3rd color for box
    (set-face-attribute 'my/org-highlighted-text nil
                        :background bg
                        :box `(:line-width -1 :color ,box)
                        :foreground 'unspecified
                        :weight 'normal)))

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
            ("Lavender & Purples"   "6A1B9A-8E24AA-AB47BC-BA68C8-CE93D8-E1BEE7-9C27B0-7B1FA2")
            ("Seaside Pebbles (Contrast)" "2F4F4F-4B5D67-5F738A-708090-8899A6-AAB9C9-DDE5EC-F0F8FF")
            ("Warm Cinnamon" "7B3F00-A0522D-CD853F-DEB887-F4A460-DAA520-B8860B-8B4513")
            ("Deep Tide" "003366-005577-007788-339999-66CCCC-99CCFF-B0C4DE-DDEEFF"))))

(defvar my-org-current-palette-index 0
  "Current index into `my-org-heading-palettes'.")

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
          ;; update highlight face dynamically
          (my/update-org-highlighted-text-face color-theme)
          (message "Applied palette: %s" (nth pos names)))
      (error "No palette found at index %s" index-or-name))))


(defun my/cycle-org-heading-palettes ()
  "Cycle to the next Org heading color palette."
  (interactive)
  (setq my-org-current-palette-index
        (mod (1+ my-org-current-palette-index) (length my-org-heading-palettes)))
  (my/apply-org-heading-palette my-org-current-palette-index))

(with-eval-after-load 'org
  (my/apply-org-heading-palette my-org-current-palette-index)
  ;; Bind cycling command in org-mode
  (spacemacs/set-leader-keys "o p" #'my/cycle-org-heading-palettes))


;; Daily theme + palette combo

(defvar jess/daily-theme-palette-alist
  '(("Monday"    . (:theme ef-trio-light          :palette "Jess theme"))
    ("Tuesday"   . (:theme doom-solarized-light   :palette "Muted Pastels"))
    ("Wednesday" . (:theme spacemacs-light        :palette "Seaside Pebbles (Contrast)"))
    ("Thursday"  . (:theme ef-reverie             :palette "Lavender & Purples"))
    ("Friday"    . (:theme zenburn                :palette "Muted Pastels"))
    ("Saturday"  . (:theme organic-green          :palette "Deep Tide"))
    ("Sunday"    . (:theme doom-one-light         :palette "Dracula Pop"))
    (_           . (:theme organic-green          :palette "Jess theme")))

  "Alist of weekday strings to theme and org headline palette.")

(defvar jess/day-theme-palette-alist
  '(("Monday"    . (:theme ef-trio-light          :palette "Jess theme"))
    ("Tuesday"   . (:theme doom-solarized-light   :palette "Muted Pastels"))
    ("Wednesday" . (:theme spacemacs-light        :palette "Seaside Pebbles (Contrast)"))
    ("Thursday"  . (:theme ef-reverie             :palette "Lavender & Purples"))
    ("Friday"    . (:theme zenburn                :palette "Muted Pastels"))
    ("Saturday"  . (:theme organic-green          :palette "Deep Tide"))
    ("Sunday"    . (:theme doom-one-light         :palette "Thermal Bloom"))
    (_           . (:theme organic-green          :palette "Jess theme"))))

(defvar jess/night-theme-palette-alist
  '(("Monday"    . (:theme doom-solarized-dark    :palette "Tokyo Dusk"))
    ("Tuesday"   . (:theme doom-miramare          :palette "Dracula Pop"))
    ("Wednesday" . (:theme ef-elea-dark           :palette "Warm Cinnamon"))
    ("Thursday"  . (:theme ef-dream               :palette "Thermal Bloom"))
    ("Friday"    . (:theme doom-henna             :palette "Volcanic Prism"))
    ("Saturday"  . (:theme wombat                 :palette "Deep Tide"))
    ("Sunday"    . (:theme doom-solarized-dark    :palette "Dracula Pop"))
    (_           . (:theme wombat                 :palette "Jess theme"))))


  ;;;;;;;;;; creating a helper so that I can set light and dark themes during the day and night :)

(defun evening-hours-p ()
  "Return t if current time is between 6pm and 7am."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 18) (< hour 7))))


(defun jess/apply-daily-theme-and-palette ()
  "Load theme and org headline palette based on weekday and time (day/night)."
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
    ;; Clean theme slate
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (load-theme theme t)
      (message "🖌 Loaded theme: %s" theme))
    (when palette
      (my/apply-org-heading-palette palette-name)
      (my/update-org-highlighted-text-face palette)
      (message "🎨 Applied org headline palette: %s" palette-name))))



;; (defun jess/apply-daily-theme-and-palette ()
;;   "Load theme and org headline palette based on the current weekday."
;;   (interactive)
;;   (let* ((day (format-time-string "%A"))
;;          (entry (or (assoc day jess/daily-theme-palette-alist)
;;                     (assoc '_' jess/daily-theme-palette-alist)))
;;          (theme (plist-get (cdr entry) :theme))
;;          (palette-name (plist-get (cdr entry) :palette))
;;          (palette (cdr (assoc palette-name my-org-heading-palettes))))
;;     ;; Disable all themes first
;;     (mapc #'disable-theme custom-enabled-themes)
;;     ;; Load daily theme
;;     (when theme
;;       (load-theme theme t)
;;       (message "🖌 Loaded theme: %s" theme))
;;     ;; Apply org headline palette
;;     (when palette
;;       (my/apply-org-heading-palette palette-name)
;;       (my/update-org-highlighted-text-face palette)
;;       (message "🎨 Applied org headline palette: %s" palette-name))))



(spacemacs/set-leader-keys "t d" #'jess/apply-daily-theme-and-palette)

;;;;;;;;;; Schedule automatic theme/palette change at midnight every day
(run-at-time nil 3600 #'jess/apply-daily-theme-and-palette)
(run-at-time "00:00" 86400 #'jess/apply-daily-theme-and-palette)


  ;;;;;;;;;; highlighting using extra programmable symbols
  ;;;;;;;;;; it had to be done as a 'box' rather than a straight highlight, because the hl-line mode kept overwriting it.
(defface my/org-highlighted-text
  '((t (:box (:line-width -1 :color "#FFAEB9")
             :background "#FFD6C9"
             ;;:foreground "black"
             ;;:weight bold
             :inherit nil
             :extend nil)))
  "Custom face for =highlighted= Org text.")

(setq org-hide-emphasis-markers t)
(setq org-emphasis-alist
      '(("*" bold nil)
        ("/" italic nil)
        ("_" underline nil)
        ("=" my/org-highlighted-text nil)
        ("~" org-verbatim verbatim)
        ("+" (:strike-through t) nil)))

;;;;;;;;;; another example of what you can do
;; (setq org-emphasis-alist
;;       '(("*" (bold :foreground "Orange" ))
;;         ("/" italic)
;;         ("_" underline)
;;         ("=" (:background "maroon" :foreground "white"))
;;         ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
;;         ("+" (:strike-through t))))







(provide 'jess-theming)
