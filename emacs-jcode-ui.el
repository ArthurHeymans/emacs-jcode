;;; emacs-jcode-ui.el --- UI primitives for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Two-buffer chat/input UI for jcode.

;;; Code:

(require 'cl-lib)
(require 'project)

(declare-function emacs-jcode-send "emacs-jcode-input")
(declare-function emacs-jcode-cancel "emacs-jcode-input")
(declare-function emacs-jcode-disconnect "emacs-jcode-input")
(declare-function emacs-jcode-previous-input "emacs-jcode-input")
(declare-function emacs-jcode-next-input "emacs-jcode-input")

(defgroup emacs-jcode nil
  "Emacs frontend for jcode."
  :group 'tools
  :prefix "emacs-jcode-")

(defcustom emacs-jcode-input-window-height 10
  "Height of the jcode input window."
  :type '(choice (natnum :tag "Lines") (float :tag "Fraction"))
  :group 'emacs-jcode)

(defface emacs-jcode-user-face '((t :inherit font-lock-keyword-face :weight bold)) "User heading face." :group 'emacs-jcode)
(defface emacs-jcode-assistant-face '((t :inherit font-lock-function-name-face :weight bold)) "Assistant heading face." :group 'emacs-jcode)
(defface emacs-jcode-tool-face '((t :inherit font-lock-comment-face)) "Tool block face." :group 'emacs-jcode)
(defface emacs-jcode-error-face '((t :inherit error)) "Error face." :group 'emacs-jcode)
(defface emacs-jcode-dim-face '((t :inherit shadow)) "Dim face." :group 'emacs-jcode)

(defvar-local emacs-jcode--session nil)
(defvar-local emacs-jcode--chat-buffer nil)
(defvar-local emacs-jcode--input-buffer nil)
(defvar-local emacs-jcode--display-session-id nil)
(defvar-local emacs-jcode--display-title nil)
(defvar-local emacs-jcode--display-status nil)
(defvar-local emacs-jcode--display-model nil)

(defun emacs-jcode--header-line ()
  "Return Pi-like header line text for current jcode buffer."
  (let* ((session (or emacs-jcode--display-title
                      emacs-jcode--display-session-id
                      "new"))
         (status (or emacs-jcode--display-status "starting"))
         (model (or emacs-jcode--display-model "model unknown"))
         (dir (abbreviate-file-name default-directory)))
    (concat
     (propertize " Jcode " 'face 'mode-line-emphasis)
     (propertize (format " %s " session) 'face 'emacs-jcode-assistant-face)
     (propertize (format " %s " status) 'face 'emacs-jcode-dim-face)
     (propertize (format " %s " model) 'face 'emacs-jcode-dim-face)
     (propertize (format " %s" dir) 'face 'emacs-jcode-dim-face))))

(cl-defun emacs-jcode--set-display-metadata (buffer &key session-id title status model)
  "Set display metadata in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when session-id (setq emacs-jcode--display-session-id session-id))
      (when title (setq emacs-jcode--display-title title))
      (when status (setq emacs-jcode--display-status status))
      (when model (setq emacs-jcode--display-model model))
      (setq header-line-format '(:eval (emacs-jcode--header-line))))))

(defvar emacs-jcode-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-k") #'emacs-jcode-cancel)
    (define-key map (kbd "C-c C-d") #'emacs-jcode-disconnect)
    map)
  "Keymap for `emacs-jcode-chat-mode'.")

(define-derived-mode emacs-jcode-chat-mode special-mode "Jcode-Chat"
  "Major mode for jcode chat buffers."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local header-line-format '(:eval (emacs-jcode--header-line))))

(defvar emacs-jcode-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emacs-jcode-send)
    (define-key map (kbd "C-c C-k") #'emacs-jcode-cancel)
    (define-key map (kbd "C-c C-d") #'emacs-jcode-disconnect)
    (define-key map (kbd "M-p") #'emacs-jcode-previous-input)
    (define-key map (kbd "M-n") #'emacs-jcode-next-input)
    map)
  "Keymap for `emacs-jcode-input-mode'.")

(define-derived-mode emacs-jcode-input-mode text-mode "Jcode-Input"
  "Major mode for composing jcode prompts."
  (setq-local header-line-format '(:eval (emacs-jcode--header-line))))

(defun emacs-jcode--project-directory ()
  "Return the current project directory, falling back to `default-directory'."
  (file-name-as-directory
   (or (when-let ((project (project-current nil)))
         (project-root project))
       default-directory)))

(defun emacs-jcode--buffer-name (kind dir &optional session-id)
  "Return a buffer name for KIND, DIR, and optional SESSION-ID."
  (format "*jcode-%s: %s%s*"
          kind
          (file-name-nondirectory (directory-file-name dir))
          (if session-id (format "[%s]" session-id) "")))

(defun emacs-jcode--make-buffers (dir &optional session-id)
  "Create chat and input buffers for DIR and optional SESSION-ID."
  (let ((chat (get-buffer-create (emacs-jcode--buffer-name "chat" dir session-id)))
        (input (get-buffer-create (emacs-jcode--buffer-name "input" dir session-id))))
    (with-current-buffer chat
      (emacs-jcode-chat-mode)
      (setq default-directory dir)
      (setq emacs-jcode--input-buffer input))
    (with-current-buffer input
      (unless (derived-mode-p 'emacs-jcode-input-mode)
        (emacs-jcode-input-mode))
      (setq default-directory dir)
      (setq emacs-jcode--chat-buffer chat))
    (cons chat input)))

(defun emacs-jcode--display-buffers (chat input)
  "Display CHAT above INPUT and focus INPUT."
  (let ((chat-window (display-buffer chat '(display-buffer-pop-up-window))))
    (select-window chat-window)
    (let ((input-window (split-window chat-window (- emacs-jcode-input-window-height) 'below)))
      (set-window-buffer input-window input)
      (select-window input-window))))

(defun emacs-jcode--append (buffer text &optional face)
  "Append TEXT to BUFFER with optional FACE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (move (= (point) (point-max))))
        (goto-char (point-max))
        (insert (if face (propertize text 'face face) text))
        (when move (goto-char (point-max)))))))

(defun emacs-jcode--section (buffer title face)
  "Append a section TITLE to BUFFER using FACE."
  (emacs-jcode--append buffer (format "\n%s\n%s\n" title (make-string (length title) ?=)) face))

(provide 'emacs-jcode-ui)
;;; emacs-jcode-ui.el ends here
