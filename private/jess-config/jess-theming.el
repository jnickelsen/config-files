;;; jess-theming.el --- Simple daily + night theme loader -*- lexical-binding: t; -*-

(message "Loading jess-theming.el")


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



;; -----------------------------
;; Utility: safely load a theme
(defun my/load-theme-safely (theme)
  "Disable all other themes and load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Loaded theme: %s" theme))

;; -----------------------------
;; Utility: check if it’s evening
(defun evening-hours-p ()
  "Return t if current time is between 6pm and 7am."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 18) (< hour 7))))

;; -----------------------------
;; Day and night theme schedules
(defvar jess/day-theme-alist
  '(("Monday"    . doom-dracula) ;; in the mood for dark today
    ("Tuesday"   . ef-day)
    ("Wednesday" . ef-reverie)
    ("Thursday"  . doom-ayu-light)
    ("Friday"    . ef-maris-light)
    ("Saturday"  . organic-green)
    ("Sunday"    . ef-summer)
    (_           . doom-flatwhite))
  "Alist mapping weekday names to day themes.")

(defvar jess/night-theme-alist
  '(("Monday"    . doom-solarized-dark)
    ("Tuesday"   . doom-gruvbox)
    ("Wednesday" . ef-elea-dark)
    ("Thursday"  . ef-dream) ;; love this!
    ("Friday"    . doom-henna)
    ("Saturday"  . wombat)
    ("Sunday"    . doom-solarized-dark)
    (_           . wombat))
  "Alist mapping weekday names to night themes.")

;; -----------------------------
;; Apply theme based on day + time
(defun jess/apply-daily-theme ()
  "Load theme for today, using day or night version."
  (interactive)
  (let* ((day (format-time-string "%A"))
         (alist (if (evening-hours-p)
                    jess/night-theme-alist
                  jess/day-theme-alist))
         (theme (or (cdr (assoc day alist))
                    (cdr (assoc '_' alist)))))
    (my/load-theme-safely theme)))

;; -----------------------------
;; Optional: keybinding for manual refresh
(spacemacs/set-leader-keys "t d" #'jess/apply-daily-theme)

;; -----------------------------
;; Midnight timer to automatically switch day/night
(run-at-time "00:00" 86400 #'jess/apply-daily-theme)
;; daytime timer
(run-at-time nil 3600 #'jess/apply-daily-theme)


;; make sure the scale isn't wonky; I like smaller headlines
(add-hook 'after-load-theme-hook
          (lambda ()
            ;; 1. Ground the universe
            (set-face-attribute 'default nil :height 100)

            ;; 2. Neutralise outline faces
            (dolist (face '(outline-1 outline-2 outline-3 outline-4
                                      outline-5 outline-6 outline-7 outline-8))
              (set-face-attribute face nil
                                  :inherit 'default
                                  :height 1.0
                                  :weight 'normal))

            ;; 3. Apply *intentional* Org hierarchy
            (set-face-attribute 'org-level-1 nil
                                :inherit 'default
                                :weight 'bold
                                :height 1.05)
            (set-face-attribute 'org-level-2 nil
                                :inherit 'default
                                :weight 'bold
                                :height 1.0)))


;; -----------------------------
;; favourites

(defvar jess/theme-clusters
  '((:name "Cosy Dark"
           :themes (doom-dracula doom-gruvbox doom-solarized-dark wombat
                                 doom-henna doom-rouge doom-miramare doom-laserwave))
    (:name "Soft Light"
           :themes (ef-day ef-maris-light doom-ayu-light doom-flatwhite
                           doom-solarized-light leuven modus-operandi spacemacs-light))
    (:name "Ef Themes"
           :themes (ef-day ef-reverie ef-maris-light ef-summer ef-elea-dark
                           ef-dream ef-trio-light ef-trio-dark ef-spring ef-rosa))
    (:name "Dreamy & Moody"
           :themes (ef-dream ef-reverie doom-laserwave doom-miramare
                             doom-rouge toxi doom-monokai-octagon))
    (:name "Nature & Calm"
           :themes (organic-green zenburn wombat ef-summer ef-spring
                                  doom-oksolar-dark modus-operandi leuven))
    (:name "All Favs"
           :themes (doom-dracula doom-gruvbox doom-solarized-dark wombat doom-henna
                                 doom-rouge doom-miramare doom-laserwave ef-day ef-maris-light
                                 doom-ayu-light doom-flatwhite doom-solarized-light leuven
                                 modus-operandi spacemacs-light ef-reverie ef-summer ef-elea-dark
                                 ef-dream ef-trio-light ef-trio-dark ef-spring ef-rosa organic-green
                                 zenburn toxi doom-oksolar-dark doom-monokai-octagon))))

;; and then the function to randomly pick from these
(defun jess/random-theme-from-cluster (cluster-name)
  "Load a random theme from the named cluster."
  (interactive
   (list (completing-read "Cluster: "
                          (mapcar (lambda (c) (plist-get c :name))
                                  jess/theme-clusters))))
  (let* ((cluster (seq-find (lambda (c) (string= (plist-get c :name) cluster-name))
                            jess/theme-clusters))
         (themes (plist-get cluster :themes))
         (pick (nth (random (length themes)) themes)))
    (my/load-theme-safely pick)
    (message "🎲 Random theme: %s (from %s)" pick cluster-name)))

(spacemacs/set-leader-keys "t r" #'jess/random-theme-from-cluster)


;; -----------------------------
;; Apply immediately on load
(jess/apply-daily-theme)


(provide 'jess-theming)
