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
(declare-function jcode-complete "jcode-input")
(declare-function jcode-steer "jcode-input")
(declare-function jcode--file-reference-capf "jcode-input")
(declare-function jcode--path-capf "jcode-input")
(declare-function jcode--maybe-complete-at "jcode-input")

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

(defun jcode--normalize-directory (dir)
  "Normalize DIR for project/session comparisons."
  (file-name-as-directory (expand-file-name dir)))

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
    (define-key map (kbd "TAB") #'jcode-complete)
    (define-key map (kbd "C-c C-k") #'jcode-cancel)
    (define-key map (kbd "C-c C-d") #'jcode-disconnect)
    (define-key map (kbd "C-c C-s") #'jcode-steer)
    (define-key map (kbd "M-p") #'jcode-previous-input)
    (define-key map (kbd "M-n") #'jcode-next-input)
    map)
  "Keymap for `jcode-input-mode'.")

;; Keep keymaps current when this package is reloaded during development.
(define-key jcode-input-mode-map (kbd "C-c C-c") #'jcode-send)
(define-key jcode-input-mode-map (kbd "TAB") #'jcode-complete)
(define-key jcode-input-mode-map (kbd "C-c C-k") #'jcode-cancel)
(define-key jcode-input-mode-map (kbd "C-c C-d") #'jcode-disconnect)
(define-key jcode-input-mode-map (kbd "C-c C-s") #'jcode-steer)
(define-key jcode-input-mode-map (kbd "@") nil)
(define-key jcode-input-mode-map (kbd "/") nil)
(define-key jcode-input-mode-map (kbd "M-p") #'jcode-previous-input)
(define-key jcode-input-mode-map (kbd "M-n") #'jcode-next-input)

(define-derived-mode jcode-input-mode text-mode "Jcode-Input"
  "Major mode for composing jcode prompts."
  (setq-local header-line-format '(:eval (jcode--header-line)))
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions #'jcode--file-reference-capf nil t)
  (add-hook 'completion-at-point-functions #'jcode--path-capf nil t)
  (add-hook 'post-self-insert-hook #'jcode--maybe-complete-at nil t)
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
  "Ensure CHAT and INPUT are visible, matching pi-style window behavior."
  (let* ((chat-windows (get-buffer-window-list chat nil))
         (input-windows (get-buffer-window-list input nil)))
    (if (and chat-windows input-windows)
        (jcode--focus-input-window chat input)
      (jcode--display-buffer-pair chat input chat-windows input-windows))))

(defun jcode--window-can-split-for-input-p (window)
  "Return non-nil if WINDOW can be split into chat and input panes."
  (>= (window-total-height window) (* 2 window-min-height)))

(defun jcode--input-height-for-window (window)
  "Return the input pane height for WINDOW."
  (let* ((total (window-total-height window))
         (max-input-height (- total window-min-height))
         (raw (if (floatp jcode-input-window-height)
                  (round (* jcode-input-window-height total))
                jcode-input-window-height)))
    (max window-min-height (min raw max-input-height))))

(defun jcode--windows-by-height (&optional windows)
  "Return live WINDOWS sorted by descending height."
  (sort (cl-remove-if-not #'window-live-p
                          (copy-sequence (or windows (window-list nil 'no-mini))))
        (lambda (a b) (> (window-total-height a) (window-total-height b)))))

(defun jcode--window-with-most-height (&optional windows)
  "Return the tallest window from WINDOWS."
  (car (jcode--windows-by-height windows)))

(defun jcode--best-display-window (&optional preferred)
  "Return the best window for displaying chat plus input."
  (or (and preferred
           (window-live-p preferred)
           (jcode--window-can-split-for-input-p preferred)
           preferred)
      (cl-find-if #'jcode--window-can-split-for-input-p
                  (jcode--windows-by-height))
      preferred
      (selected-window)))

(defun jcode--preferred-display-window (chat-windows input-windows selected)
  "Return preferred base window for displaying a jcode pair."
  (cond
   ((and input-windows (not chat-windows)
         (not (memq selected input-windows))
         (jcode--window-can-split-for-input-p selected))
    selected)
   (chat-windows (jcode--window-with-most-height chat-windows))
   (input-windows (jcode--window-with-most-height input-windows))
   (t selected)))

(defun jcode--delete-extra-input-windows (input-windows target)
  "Delete INPUT-WINDOWS except TARGET."
  (dolist (window input-windows)
    (unless (eq window target)
      (ignore-errors (delete-window window)))))

(defun jcode--paired-input-window (chat-window input)
  "Return input window below CHAT-WINDOW showing INPUT, or nil."
  (when (window-live-p chat-window)
    (let ((below (window-in-direction 'below chat-window)))
      (and below (eq (window-buffer below) input) below))))

(defun jcode--best-input-window (chat input)
  "Return best visible input window for CHAT/INPUT in the selected frame."
  (let* ((input-windows (get-buffer-window-list input nil))
         (selected (selected-window))
         (selected-chat-window (and (eq (window-buffer selected) chat) selected)))
    (or (jcode--paired-input-window selected-chat-window input)
        (and (memq selected input-windows) selected)
        (jcode--window-with-most-height input-windows))))

(defun jcode--focus-input-window (chat input)
  "Focus a visible INPUT window for CHAT."
  (when-let* ((window (jcode--best-input-window chat input)))
    (select-window window)))

(defun jcode--display-buffer-pair (chat input chat-windows input-windows)
  "Display CHAT over INPUT using pi-style window selection."
  (let* ((selected (selected-window))
         (preferred (jcode--preferred-display-window chat-windows input-windows selected))
         (target (jcode--best-display-window preferred))
         input-window)
    (when (and input-windows (not chat-windows))
      (jcode--delete-extra-input-windows input-windows target))
    (with-selected-window target
      (unless (jcode--window-can-split-for-input-p target)
        (delete-other-windows target))
      (unless (jcode--window-can-split-for-input-p target)
        (user-error "Window too small for chat + input layout"))
      (switch-to-buffer chat)
      (with-current-buffer chat (goto-char (point-max)))
      (setq input-window (split-window nil (- (jcode--input-height-for-window target)) 'below))
      (set-window-buffer input-window input)
      (set-window-dedicated-p input-window 'side))
    (when (window-live-p input-window)
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

(provide 'jcode-ui)
;;; jcode-ui.el ends here
