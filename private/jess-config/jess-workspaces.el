;;; jess-workspaces.el --- Named workspace layouts -*- lexical-binding: t; -*-

(message "Loading jess-workspaces.el")

;; ─────────────────────────────────────────────
;; CONFIGURATION
;; ─────────────────────────────────────────────

(defvar jess/reference-file
  "~/Documents/GitHub/org/emacs reference.org"
  "Path to the Emacs reference file, shown in the right sidebar.")

;; dired opens files in the next window rather than replacing itself
(setq dired-dwim-target t)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") (lambda ()
                  (interactive)
                  (if (file-directory-p (dired-get-file-for-visit))
                      (dired-find-file)
                    (dired-find-file-other-window)))))

(defvar jess/editing-window nil
  "The designated editing window in the current workspace layout.")

;; ─────────────────────────────────────────────
;; CORE LAYOUT
;;
;; Three columns:
;;   left  ~1/4  — dired navigation
;;   middle ~1/2 — main editing buffer
;;   right  ~1/4 — reference sidebar
;; ─────────────────────────────────────────────

(defun jess/three-column-layout (dired-dir &optional main-file)
  "Set up a three-column layout.
DIRED-DIR is opened on the left.
MAIN-FILE (optional) is opened in the centre.
Reference file is opened on the right."
  (delete-other-windows)
  (let* ((total-width (frame-width))
         (left-width  (/ total-width 4))
         (right-width (/ total-width 4)))

    ;; Left column — dired
    (dired (expand-file-name dired-dir))
    (visual-line-mode 1)

    ;; Middle column — main file or scratch buffer
    (let ((mid-win (split-window-right left-width)))
      (select-window mid-win)
      (if main-file
          (progn
            (find-file (expand-file-name main-file))
            (visual-line-mode 1))
        ;; No main file — leave a clean buffer ready to receive
        (switch-to-buffer (get-buffer-create "*writing*")))

      ;; Right column — reference
      (let ((ref-win (split-window-right (- (window-width) right-width))))
        (select-window ref-win)
        (find-file (expand-file-name jess/reference-file))
        (visual-line-mode 1)))
    (setq jess/editing-window (selected-window))
    ;; Leave cursor in the middle (editing) window
    (select-window (window-at (/ total-width 2) 5))))


;; ─────────────────────────────────────────────
;; WORKSPACE SWITCHER
;; Names the current eyebrowse slot and runs the layout.
;; ─────────────────────────────────────────────

(defun jess/switch-workspace (name layout-fn)
  "Name the current eyebrowse slot NAME and run LAYOUT-FN."
  (when (fboundp 'eyebrowse-rename-window-config)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) name))
  (funcall layout-fn))


;; ─────────────────────────────────────────────
;; WORKSPACES
;; ─────────────────────────────────────────────

(defun jess/workspace-trash-planet ()
  "Switch to the Trash Planet workspace."
  (interactive)
  (jess/switch-workspace
   "Trash Planet"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/trash-planet-redux/"
      "~/Documents/GitHub/trash-planet-redux/outline2.1.org"))))

(defun jess/workspace-felathia ()
  "Switch to the Felathia workspace."
  (interactive)
  (jess/switch-workspace
   "Felathia"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/felathia/"
      "~/Documents/GitHub/felathia/new structure.org"))))

(defun jess/workspace-fiction ()
  "Switch to the short fiction workspace."
  (interactive)
  (jess/switch-workspace
   "Fiction"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/fiction/80 fiction"))))

(defun jess/workspace-work ()
  "Switch to the Work workspace."
  (interactive)
  (jess/switch-workspace
   "Work"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/work/"
      "~/Documents/GitHub/work/work.org"))))

(defun jess/workspace-blog ()
  "Switch to the Blog workspace."
  (interactive)
  (jess/switch-workspace
   "Blog"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/obsidian/40 Authoring/46 Blog/2026/drafts"))))

(defun jess/workspace-cicada ()
  "Switch to the Cicada Ink workspace."
  (interactive)
  (jess/switch-workspace
   "Cicada"
   (lambda ()
     (jess/three-column-layout
      "~/Documents/GitHub/obsidian/60 cicada ink"))))


;; ─────────────────────────────────────────────
;; KEYBINDINGS  SPC W <key>
;; ─────────────────────────────────────────────
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") (lambda ()
                  (interactive)
                  (if (file-directory-p (dired-get-file-for-visit))
                      (dired-find-file)
                    (let ((file (dired-get-file-for-visit)))
                      (if (and jess/editing-window
                               (window-live-p jess/editing-window))
                          (progn
                            (select-window jess/editing-window)
                            (find-file file))
                        (dired-find-file-other-window)))))))
(spacemacs/declare-prefix "W" "workspaces")
(spacemacs/set-leader-keys
  "Wt" #'jess/workspace-trash-planet   ;; Trash Planet
  "Wf" #'jess/workspace-felathia       ;; Felathia
  "Ws" #'jess/workspace-fiction        ;; Short fiction
  "Ww" #'jess/workspace-work           ;; Work
  "Wb" #'jess/workspace-blog           ;; Blog
  "Wc" #'jess/workspace-cicada)        ;; Cicada Ink


(provide 'jess-workspaces)
;;; jess-workspaces.el ends here
