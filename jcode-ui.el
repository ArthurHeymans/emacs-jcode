;;; jcode-ui.el --- UI primitives for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Two-buffer chat/input UI for jcode.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'md-ts-mode nil t)

(declare-function jcode-send "jcode-input")
(declare-function jcode-cancel "jcode-input")
(declare-function jcode-disconnect "jcode-input")
(declare-function jcode-previous-input "jcode-input")
(declare-function jcode-next-input "jcode-input")
(declare-function jcode-complete "jcode-input")
(declare-function jcode-steer "jcode-input")
(declare-function jcode-select-model "jcode-native")
(declare-function jcode-cycle-reasoning-effort "jcode-native")
(declare-function jcode-toggle-block "jcode-render")
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

(defcustom jcode-chat-markdown-rendering t
  "Whether jcode chat buffers use tree-sitter markdown rendering when available.
When non-nil and `md-ts-mode' is installed, chat buffers derive from
`md-ts-mode' and hide markdown markup for a cleaner pi-like transcript.
When nil or unavailable, chat buffers fall back to `special-mode'."
  :type 'boolean
  :group 'jcode)

(defface jcode-text-face '((t :foreground "#dcdcd7")) "Native jcode assistant/body text face." :group 'jcode)
(defface jcode-user-text-face '((t :foreground "#f5f5ff")) "Native jcode user text face." :group 'jcode)
(defface jcode-user-bg-face '((t :background "#232832" :extend t)) "Native jcode user background face." :group 'jcode)
(defface jcode-strong-face '((t :foreground "#f0f0eb" :weight bold)) "Native jcode strong text face." :group 'jcode)
(defface jcode-heading-1-face '((t :foreground "#ffd764" :weight bold)) "Native jcode level-1 heading face." :group 'jcode)
(defface jcode-heading-2-face '((t :foreground "#f0be5a" :weight bold)) "Native jcode level-2 heading face." :group 'jcode)
(defface jcode-heading-face '((t :foreground "#c89b4b" :weight bold)) "Native jcode heading face." :group 'jcode)
(defface jcode-code-face '((t :foreground "#b4b4b4" :background "#2d2d2d")) "Native jcode code face." :group 'jcode)
(defface jcode-link-face '((t :foreground "#78b4f0" :underline t)) "Native jcode link face." :group 'jcode)
(defface jcode-dim-face '((t :foreground "#646464")) "Native jcode dim face." :group 'jcode)
(defface jcode-user-face '((t :foreground "#8ab4f8" :weight bold)) "Native jcode user marker face." :group 'jcode)
(defface jcode-assistant-face '((t :foreground "#81c784" :weight bold)) "Native jcode assistant marker face." :group 'jcode)
(defface jcode-tool-face '((t :foreground "#787878")) "Native jcode tool name face." :group 'jcode)
(defface jcode-tool-accent-face '((t :foreground "#ba8bff")) "Native jcode tool accent face." :group 'jcode)
(defface jcode-tool-success-face '((t :foreground "#64b464")) "Native jcode tool success face." :group 'jcode)
(defface jcode-tool-warning-face '((t :foreground "#d6b85c")) "Native jcode tool warning face." :group 'jcode)
(defface jcode-tool-error-face '((t :foreground "#dc6464")) "Native jcode tool error face." :group 'jcode)
(defface jcode-tool-running-face '((t :foreground "#50c8dc")) "Native jcode running tool face." :group 'jcode)
(defface jcode-tool-block-face '((t :extend t)) "Tool block overlay face. Intentionally background-free like the TUI transcript." :group 'jcode)
(defface jcode-collapsed-face '((t :inherit jcode-dim-face :slant italic)) "Collapsed content indicator face." :group 'jcode)
(defface jcode-error-face '((t :inherit error)) "Error face." :group 'jcode)

(defvar-local jcode--session nil)
(defvar-local jcode--chat-buffer nil)
(defvar-local jcode--input-buffer nil)
(defvar-local jcode--display-session-id nil)
(defvar-local jcode--display-title nil)
(defvar-local jcode--display-status nil)
(defvar-local jcode--display-model nil)
(defvar-local jcode--display-reasoning-effort nil)
(defvar-local jcode--display-provider nil)
(defvar-local jcode--display-credential nil)
(defvar-local jcode--display-total-tokens nil)
(defvar-local jcode--display-token-usage-totals nil)
(defvar-local jcode--display-activity nil)
(defvar-local jcode--available-models nil)
(defvar-local jcode--killing-linked-buffer nil)

(defvar jcode--header-model-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'jcode-select-model)
    (define-key map [mode-line mouse-1] #'jcode-select-model)
    map)
  "Keymap for clicking the model in the jcode header.")

(defvar jcode--header-reasoning-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'jcode-cycle-reasoning-effort)
    (define-key map [mode-line mouse-1] #'jcode-cycle-reasoning-effort)
    map)
  "Keymap for clicking the reasoning effort in the jcode header.")

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

(defun jcode--shorten-model-name (model)
  "Return a compact display name for MODEL."
  (let ((name (or model "...")))
    (setq name (replace-regexp-in-string "^claude-" "" name))
    (setq name (replace-regexp-in-string "-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" "" name))
    (if (> (length name) 28)
        (concat (substring name 0 25) "...")
      name)))

(defun jcode--format-token-count (tokens)
  "Format TOKENS compactly."
  (cond
   ((not (numberp tokens)) "?")
   ((>= tokens 1000000) (format "%.1fM" (/ tokens 1000000.0)))
   ((>= tokens 1000) (format "%.1fk" (/ tokens 1000.0)))
   (t (number-to-string tokens))))

(defun jcode--header-activity ()
  "Return useful activity text for the header."
  (let ((activity jcode--display-activity))
    (cond
     ((and (listp activity) (alist-get 'current_tool_name activity))
      (format "tool:%s" (alist-get 'current_tool_name activity)))
     ((and (listp activity) (eq (alist-get 'is_processing activity) t)) "streaming")
     (jcode--display-status jcode--display-status)
     (t "idle"))))

(defun jcode--header-usage ()
  "Return explicit token usage text for the header."
  (let* ((totals jcode--display-token-usage-totals)
         (input (or (and (listp totals) (alist-get 'input_tokens totals))
                    (and (vectorp jcode--display-total-tokens)
                         (> (length jcode--display-total-tokens) 0)
                         (aref jcode--display-total-tokens 0))))
         (output (or (and (listp totals) (alist-get 'output_tokens totals))
                     (and (vectorp jcode--display-total-tokens)
                          (> (length jcode--display-total-tokens) 1)
                          (aref jcode--display-total-tokens 1))))
         (cache-read (and (listp totals) (alist-get 'cache_read_input_tokens totals))))
    (concat
     (if (or input output)
         (format " │ usage input %s output %s" (jcode--format-token-count input)
                 (jcode--format-token-count output))
       "")
     (if (and cache-read (> cache-read 0))
         (format " cached %s" (jcode--format-token-count cache-read))
       ""))))

(defun jcode--header-provider ()
  "Return provider and credential label for the header."
  (let ((provider (pcase jcode--display-provider
                    ('nil nil)
                    (:false nil)
                    ((and (pred symbolp) sym) (symbol-name sym))
                    ((and (pred stringp) str) str)
                    (_ (format "%s" jcode--display-provider))))
        (credential (pcase jcode--display-credential
                      ('nil nil)
                      (:false nil)
                      ((and (pred symbolp) sym) (symbol-name sym))
                      ((and (pred stringp) str) str)
                      (_ (format "%s" jcode--display-credential)))))
    (cond
     ((and provider credential) (format "%s (%s)" provider credential))
     (provider provider)
     (credential credential)
     (t "provider ?"))))

(defun jcode--header-line ()
  "Return compact native-style header line for the input buffer."
  (let* ((provider (jcode--header-provider))
         (model (jcode--shorten-model-name jcode--display-model))
         (reasoning (format "think %s" (or jcode--display-reasoning-effort "?")))
         (activity (jcode--header-activity)))
    (concat
     (propertize provider 'face 'jcode-dim-face)
     " │ "
     (propertize model
                 'face 'jcode-assistant-face
                 'mouse-face 'highlight
                 'help-echo "mouse-1: Select model"
                 'local-map jcode--header-model-map)
     " • "
     (propertize reasoning
                 'mouse-face 'highlight
                 'help-echo "mouse-1: Cycle reasoning effort"
                 'local-map jcode--header-reasoning-map)
     " "
     (propertize (format "%-9s" activity) 'face 'jcode-dim-face)
     (jcode--header-usage))))

(cl-defun jcode--set-display-metadata (buffer &key session-id title status model
                                               reasoning-effort provider credential
                                               total-tokens token-usage-totals
                                               activity available-models)
  "Set display metadata in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when session-id (setq jcode--display-session-id session-id))
      (when title (setq jcode--display-title title))
      (when status (setq jcode--display-status status))
      (when model (setq jcode--display-model model))
      (when reasoning-effort (setq jcode--display-reasoning-effort reasoning-effort))
      (when provider (setq jcode--display-provider provider))
      (when credential (setq jcode--display-credential credential))
      (when total-tokens (setq jcode--display-total-tokens total-tokens))
      (when token-usage-totals (setq jcode--display-token-usage-totals token-usage-totals))
      (when activity (setq jcode--display-activity activity))
      (when available-models (setq jcode--available-models available-models))
      (setq header-line-format
            (when (derived-mode-p 'jcode-input-mode)
              '(:eval (jcode--header-line)))))))

(defvar jcode-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-k") #'jcode-cancel)
    (define-key map (kbd "C-c C-d") #'jcode-disconnect)
    (define-key map (kbd "TAB") #'jcode-toggle-block)
    (define-key map (kbd "<tab>") #'jcode-toggle-block)
    map)
  "Keymap for `jcode-chat-mode'.")

(defun jcode--chat-mode-setup ()
  "Shared setup for `jcode-chat-mode'."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local header-line-format nil)
  (setq-local window-point-insertion-type t)
  (setq-local face-remapping-alist
              '((default jcode-text-face default)
                (font-lock-comment-face jcode-dim-face)
                (font-lock-doc-face jcode-dim-face)
                (font-lock-string-face jcode-code-face)
                (font-lock-constant-face jcode-code-face)
                (font-lock-keyword-face jcode-strong-face)
                (font-lock-function-name-face jcode-heading-face)
                (font-lock-type-face jcode-link-face)
                (bold jcode-strong-face bold)
                (italic italic)))
  (when (derived-mode-p 'md-ts-mode)
    ;; Pi-style display: keep canonical markdown in the buffer but hide markup
    ;; such as **, inline backticks, and fences in the visible transcript.
    (setq-local md-ts-hide-markup t)
    (when (fboundp 'md-ts--set-hide-markup)
      (md-ts--set-hide-markup t)))
  (add-hook 'kill-buffer-hook #'jcode--kill-linked-buffer nil t))

(define-derived-mode jcode-chat-mode special-mode "Jcode-Chat"
  "Major mode for jcode chat buffers."
  (jcode--chat-mode-setup))

(when (and jcode-chat-markdown-rendering (fboundp 'md-ts-mode))
  (define-derived-mode jcode-chat-mode md-ts-mode "Jcode-Chat"
    "Major mode for jcode chat buffers.
Derives from `md-ts-mode' when available for tree-sitter markdown rendering."
    (jcode--chat-mode-setup)))

(put 'jcode-chat-mode 'mode-class 'special)

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
