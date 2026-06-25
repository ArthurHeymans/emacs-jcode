;;; jcode-ui.el --- UI primitives for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Two-buffer chat/input UI for jcode.

;;; Code:

(require 'cl-lib)
(require 'project)

(declare-function jcode-send "jcode-input")
(declare-function jcode-cancel "jcode-input")
(declare-function jcode-disconnect "jcode-input")
(declare-function jcode-previous-input "jcode-input")
(declare-function jcode-next-input "jcode-input")

(defgroup jcode nil
  "Emacs frontend for jcode."
  :group 'tools
  :prefix "jcode-")

(defcustom jcode-input-window-height 10
  "Height of the jcode input window."
  :type '(choice (natnum :tag "Lines") (float :tag "Fraction"))
  :group 'jcode)

(defface jcode-user-face '((t :inherit font-lock-keyword-face :weight bold)) "User heading face." :group 'jcode)
(defface jcode-assistant-face '((t :inherit font-lock-function-name-face :weight bold)) "Assistant heading face." :group 'jcode)
(defface jcode-tool-face '((t :inherit font-lock-comment-face)) "Tool block face." :group 'jcode)
(defface jcode-error-face '((t :inherit error)) "Error face." :group 'jcode)
(defface jcode-dim-face '((t :inherit shadow)) "Dim face." :group 'jcode)

(defvar-local jcode--session nil)
(defvar-local jcode--chat-buffer nil)
(defvar-local jcode--input-buffer nil)
(defvar-local jcode--display-session-id nil)
(defvar-local jcode--display-title nil)
(defvar-local jcode--display-status nil)
(defvar-local jcode--display-model nil)
(defvar-local jcode--killing-linked-buffer nil)

(defun jcode--kill-linked-buffer ()
  "Kill the chat/input buffer paired with the current jcode buffer."
  (unless jcode--killing-linked-buffer
    (let ((linked (cond
                   ((derived-mode-p 'jcode-chat-mode)
                    jcode--input-buffer)
                   ((derived-mode-p 'jcode-input-mode)
                    jcode--chat-buffer))))
      (when (buffer-live-p linked)
        (with-current-buffer linked
          (setq jcode--killing-linked-buffer t))
        (kill-buffer linked)))))

(defun jcode--header-line ()
  "Return Pi-like header line text for current jcode buffer."
  (let* ((session (or jcode--display-title
                      jcode--display-session-id
                      "new"))
         (status (or jcode--display-status "starting"))
         (model (or jcode--display-model "model unknown"))
         (dir (abbreviate-file-name default-directory)))
    (concat
     (propertize " Jcode " 'face 'mode-line-emphasis)
     (propertize (format " %s " session) 'face 'jcode-assistant-face)
     (propertize (format " %s " status) 'face 'jcode-dim-face)
     (propertize (format " %s " model) 'face 'jcode-dim-face)
     (propertize (format " %s" dir) 'face 'jcode-dim-face))))

(cl-defun jcode--set-display-metadata (buffer &key session-id title status model)
  "Set display metadata in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when session-id (setq jcode--display-session-id session-id))
      (when title (setq jcode--display-title title))
      (when status (setq jcode--display-status status))
      (when model (setq jcode--display-model model))
      (setq header-line-format '(:eval (jcode--header-line))))))

(defvar jcode-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-k") #'jcode-cancel)
    (define-key map (kbd "C-c C-d") #'jcode-disconnect)
    map)
  "Keymap for `jcode-chat-mode'.")

(define-derived-mode jcode-chat-mode special-mode "Jcode-Chat"
  "Major mode for jcode chat buffers."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local header-line-format '(:eval (jcode--header-line)))
  (add-hook 'kill-buffer-hook #'jcode--kill-linked-buffer nil t))

(defvar jcode-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'jcode-send)
    (define-key map (kbd "C-c C-k") #'jcode-cancel)
    (define-key map (kbd "C-c C-d") #'jcode-disconnect)
    (define-key map (kbd "M-p") #'jcode-previous-input)
    (define-key map (kbd "M-n") #'jcode-next-input)
    map)
  "Keymap for `jcode-input-mode'.")

(define-derived-mode jcode-input-mode text-mode "Jcode-Input"
  "Major mode for composing jcode prompts."
  (setq-local header-line-format '(:eval (jcode--header-line)))
  (add-hook 'kill-buffer-hook #'jcode--kill-linked-buffer nil t))

(defun jcode--clear-chat-buffer (chat)
  "Erase CHAT buffer contents."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun jcode--project-directory ()
  "Return the current project directory, falling back to `default-directory'."
  (file-name-as-directory
   (or (when-let ((project (project-current nil)))
         (project-root project))
       default-directory)))

(defun jcode--buffer-name (kind dir &optional session-id)
  "Return a buffer name for KIND, DIR, and optional SESSION-ID."
  (format "*jcode-%s: %s%s*"
          kind
          (file-name-nondirectory (directory-file-name dir))
          (if session-id (format "[%s]" session-id) "")))

(defun jcode--make-buffers (dir &optional session-id)
  "Create chat and input buffers for DIR and optional SESSION-ID."
  (let ((chat (get-buffer-create (jcode--buffer-name "chat" dir session-id)))
        (input (get-buffer-create (jcode--buffer-name "input" dir session-id))))
    (with-current-buffer chat
      (jcode-chat-mode)
      (setq default-directory dir)
      (setq jcode--input-buffer input))
    (with-current-buffer input
      (unless (derived-mode-p 'jcode-input-mode)
        (jcode-input-mode))
      (setq default-directory dir)
      (setq jcode--chat-buffer chat))
    (cons chat input)))

(defun jcode--display-buffers (chat input)
  "Display CHAT above INPUT and focus INPUT."
  (let ((chat-window (display-buffer chat '(display-buffer-pop-up-window))))
    (select-window chat-window)
    (let ((input-window (split-window chat-window (- jcode-input-window-height) 'below)))
      (set-window-buffer input-window input)
      (select-window input-window))))

(defun jcode--append (buffer text &optional face)
  "Append TEXT to BUFFER with optional FACE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (move (= (point) (point-max))))
        (goto-char (point-max))
        (insert (if face (propertize text 'face face) text))
        (when move (goto-char (point-max)))))))

(defun jcode--section (buffer title face)
  "Append a section TITLE to BUFFER using FACE."
  (jcode--append buffer (format "\n%s\n%s\n" title (make-string (length title) ?=)) face))

(provide 'emacs-jcode-ui)
;;; jcode-ui.el ends here
