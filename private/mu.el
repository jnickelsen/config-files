;;; mu.el --- Play on a MUSH or MUD within Emacs

;; Copyright (C) 2001, 2004  Alex Schroeder
;; Updated 2025 for Discworld connection

(require 'cl-lib)
(require 'ansi-color)

(defgroup mu nil
  "A MUSH or MUD client."
  :group 'processes)

(defcustom mu-worlds nil
  "List of worlds you play in.
Each element is a vector: [NAME HOST PORT CHARACTER PASSWORD]"
  :type '(repeat
          (vector :tag "World"
                  (string :tag "Name")
                  (string :tag "Host")
                  (integer :tag "Port")
                  (string :tag "Char" :value "guest")
                  (string :tag "Pwd" :value "guest")))
  :group 'mu)

;;; World accessors

(defsubst mu-world-name (world) (concat (aref world 3) "@" (aref world 0)))
(defsubst mu-world-host (world) (aref world 1))
(defsubst mu-world-port (world) (aref world 2))
(defsubst mu-world-char (world) (aref world 3))
(defsubst mu-world-pass (world) (aref world 4))

;;; Connection state
(defvar mu-connection nil
  "The current MU process.")

(defvar mu-world-current nil
  "The world corresponding to `mu-connection'.")

;;; Network filter

(defun mu-network-filter (proc string)
  "Insert STRING received from PROC into its buffer, applying ANSI colors."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (when (fboundp 'ansi-color-apply-on-region)
            (ansi-color-apply-on-region (process-mark proc) (point)))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;;; Open connection

(defun mu-get-world ()
  "Prompt the user to choose a world from `mu-worlds'."
  (let* ((choices (mapcar (lambda (w) (cons (mu-world-name w) w))
                          mu-worlds))
         (sel (completing-read "World: " choices nil t)))
    (cdr (assoc sel choices))))


(defun mu-open (world)
  "Create a new mu connection in the current window without splitting."
  (interactive (list (mu-get-world)))
  (when world
    (message "Opening connection...")
    ;; Create the comint buffer
    (let ((buf (make-comint (mu-world-name world) (mu-world-network world))))
      ;; Switch to it in the current window
      (switch-to-buffer buf)
      ;; Login if password is provided
      (when (and (mu-world-password world)
                 (> (length (mu-world-password world)) 0))
        (mu-login world))
      ;; Set the major mode
      (mu-connection-mode (mu-world-name world))
      ;; Create the input buffer for this connection
      (mu-input-buffer buf)
      (message "Opening connection...done"))))
(defun mu-input-buffer (conn-buffer)
  "Create a mu input buffer for connection CONN-BUFFER in the same window.
The current buffer should be a mu connection buffer."
  (let ((input-name (concat "*Input for " mu-name "*")))
    ;; Create or switch to input buffer in the current window
    (switch-to-buffer (get-buffer-create input-name))
    ;; Set up mu-input-mode
    (mu-input-mode conn-buffer)))

;;; Login

(defun mu-login (world)
  "Send login string for WORLD to the current MU process."
  (when (process-live-p mu-connection)
    (process-send-string
     mu-connection
     (format "%s %s\n"
             (mu-world-char world)
             (mu-world-pass world)))))

;;; Major mode

(defvar mu-connection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mu-send-line)
    map)
  "Keymap for `mu-connection-mode'.")

(define-derived-mode mu-connection-mode comint-mode "MU* Conn"
  "Major mode for MU connections."
  (setq comint-process-echoes nil)
  (setq comint-input-sender 'mu-input-sender)
  (use-local-map mu-connection-mode-map))

(defun mu-input-sender (proc input)
  "Send INPUT to PROC, adding newline."
  (when (process-live-p proc)
    (process-send-string proc (concat input "\n"))))

(defun mu-send-line ()
  "Send the current line to the MU server."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (mu-input-sender mu-connection line)
    (forward-line 1)))

(provide 'mu)
;;; mu.el ends here
