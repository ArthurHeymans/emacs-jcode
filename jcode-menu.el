;;; jcode-menu.el --- Transient menu for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-native)
(require 'jcode-input)

(declare-function jcode "jcode")
(declare-function jcode-list "jcode")
(declare-function jcode-reconnect "jcode")
(declare-function jcode-connect "jcode")
(declare-function jcode-cancel "jcode-input")
(declare-function jcode-disconnect "jcode-input")
(declare-function jcode-select-model "jcode-native")
(declare-function jcode-select-reasoning-effort "jcode-native")
(declare-function jcode-select-fast-mode "jcode-native")

(defconst jcode-slash-commands
  '(("/?" . "Open jcode command menu")
    ("/help" . "Open jcode command menu")
    ("/compact" . "Compact conversation context")
    ("/clear" . "Clear conversation history")
    ("/split" . "Clone this conversation into a new session")
    ("/transfer" . "Create a compacted handoff session")
    ("/memory" . "Select memory feature state")
    ("/extract-memory" . "Trigger memory extraction")
    ("/swarm" . "Select swarm feature state")
    ("/review" . "Select autoreview feature state")
    ("/judge" . "Select autojudge feature state")
    ("/model" . "Select model")
    ("/reasoning" . "Select reasoning effort")
    ("/fast" . "Select fast/service tier")
    ("/transport" . "Select OpenAI transport")
    ("/premium" . "Select Copilot premium conservation mode")
    ("/z" . "Copilot premium normal mode")
    ("/zz" . "Copilot one premium request per session")
    ("/zzz" . "Copilot zero premium requests")
    ("/cancel" . "Cancel current generation")
    ("/reconnect" . "Reconnect and take over the native session")
    ("/sessions" . "Open session list")
    ("/rename" . "Rename session")
    ("/compaction-mode" . "Select compaction mode"))
  "Slash commands supported by Emacs jcode.

This is curated from the native JSON protocol request surface and the TUI
slash commands: compact, clear, split, transfer, trigger_memory_extraction,
set_model, set_reasoning_effort, set_service_tier, set_transport,
set_premium_mode, set_feature, cancel, resume/list helpers, rename_session,
and set_compaction_mode.")

(defconst jcode-compaction-modes '("reactive" "proactive" "semantic")
  "Known jcode compaction modes exposed by the menu.")

(defconst jcode-transport-modes '("auto" "https" "websocket")
  "Known native jcode transport modes.")

(defconst jcode-premium-modes '(("normal" . 0)
                                ("one" . 1)
                                ("zero" . 2))
  "Known native jcode Copilot premium conservation modes.")

(defconst jcode-feature-toggles '("memory" "swarm" "autoreview" "autojudge")
  "Native jcode runtime features exposed by the menu.")

(defun jcode--menu-chat ()
  "Return the current jcode chat buffer."
  (or (and (derived-mode-p 'jcode-chat-mode) (current-buffer))
      (and (boundp 'jcode--chat-buffer) (buffer-live-p jcode--chat-buffer) jcode--chat-buffer)
      (user-error "No jcode session")))

(defun jcode--menu-connection ()
  "Return the current native jcode connection."
  (let ((chat (jcode--menu-chat)))
    (or (buffer-local-value 'jcode--native-connection chat)
        (user-error "No native jcode connection"))))

(defun jcode-native-request-current (type &rest fields)
  "Send native request TYPE with FIELDS for the current session."
  (apply #'jcode-native--request (jcode--menu-connection) type fields))

(defun jcode-compact ()
  "Trigger manual native jcode context compaction."
  (interactive)
  (jcode-native-request-current "compact")
  (message "Jcode: compaction requested"))

(defun jcode-clear ()
  "Clear the current native jcode conversation history."
  (interactive)
  (when (yes-or-no-p "Clear this jcode conversation? ")
    (jcode-native-request-current "clear")
    (when-let ((chat (jcode--menu-chat)))
      (jcode--clear-chat-buffer chat))
    (message "Jcode: conversation cleared")))

(defun jcode-split ()
  "Split the current native jcode session."
  (interactive)
  (jcode-native-request-current "split")
  (message "Jcode: split requested"))

(defun jcode-transfer ()
  "Transfer the current native jcode session into a compact handoff session."
  (interactive)
  (jcode-native-request-current "transfer")
  (message "Jcode: transfer requested"))

(defun jcode-trigger-memory-extraction ()
  "Trigger memory extraction for the current native jcode session."
  (interactive)
  (jcode-native-request-current "trigger_memory_extraction")
  (message "Jcode: memory extraction requested"))

(defun jcode-rename-session (title)
  "Rename the current native jcode session to TITLE.
An empty TITLE clears the custom title."
  (interactive (list (read-string "Session title (empty clears): ")))
  (jcode-native-request-current "rename_session"
                                :title (unless (string-empty-p title) title))
  (message "Jcode: rename requested"))

(defun jcode-select-compaction-mode (&optional mode)
  "Select native jcode compaction MODE."
  (interactive)
  (let ((choice (or mode (completing-read "Compaction mode: " jcode-compaction-modes nil t))))
    (jcode-native-request-current "set_compaction_mode" :mode choice)
    (message "Jcode: compaction mode %s" choice)))

(defun jcode-select-transport (&optional transport)
  "Select native jcode TRANSPORT for providers that support it."
  (interactive)
  (let ((choice (or transport (completing-read "Transport: " jcode-transport-modes nil t))))
    (jcode-native-request-current "set_transport" :transport choice)
    (message "Jcode: transport %s" choice)))

(defun jcode-select-premium-mode (&optional mode)
  "Select native jcode Copilot premium conservation MODE."
  (interactive)
  (let* ((choice (or mode (completing-read "Premium mode: " (mapcar #'car jcode-premium-modes) nil t)))
         (value (cdr (assoc choice jcode-premium-modes))))
    (unless value (user-error "Unknown premium mode: %s" choice))
    (jcode-native-request-current "set_premium_mode" :mode value)
    (message "Jcode: premium mode %s" choice)))

(defun jcode-set-feature (feature enabled)
  "Set native jcode runtime FEATURE to ENABLED."
  (interactive
   (list (completing-read "Feature: " jcode-feature-toggles nil t)
         (y-or-n-p "Enable feature? ")))
  (jcode-native-request-current "set_feature" :feature feature :enabled (and enabled t))
  (message "Jcode: %s %s" feature (if enabled "on" "off")))

(defun jcode-select-feature-state (&optional feature state)
  "Select native jcode FEATURE STATE, where state is on or off."
  (interactive
   (list (completing-read "Feature: " jcode-feature-toggles nil t)
         (completing-read "State: " '("on" "off") nil t)))
  (let ((choice (or state (completing-read "State: " '("on" "off") nil t))))
    (jcode-set-feature feature (string= choice "on"))))

(defun jcode-enable-feature (feature)
  "Enable native jcode runtime FEATURE."
  (interactive (list (completing-read "Enable feature: " jcode-feature-toggles nil t)))
  (jcode-set-feature feature t))

(defun jcode-disable-feature (feature)
  "Disable native jcode runtime FEATURE."
  (interactive (list (completing-read "Disable feature: " jcode-feature-toggles nil t)))
  (jcode-set-feature feature nil))

(defun jcode--slash-command-at-point ()
  "Return slash command bounds at point, or nil."
  (when (and (derived-mode-p 'jcode-input-mode)
             (save-excursion
               (skip-chars-backward "^[:space:]\n")
               (looking-at "/")))
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "^[:space:]\n")
                   (point))))
      (cons start end))))

(defun jcode--slash-command-capf ()
  "Completion-at-point function for jcode slash commands."
  (when-let ((bounds (jcode--slash-command-at-point)))
    (list (car bounds) (cdr bounds)
          (mapcar #'car jcode-slash-commands)
          :exclusive 'no
          :annotation-function
          (lambda (candidate)
            (concat " " (or (cdr (assoc candidate jcode-slash-commands)) ""))))))

(defun jcode--install-slash-command-capf (&optional buffer)
  "Install jcode slash command completion in BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'jcode-input-mode)
      (setq-local completion-at-point-functions
                  (cons #'jcode--slash-command-capf
                        (delq #'jcode--slash-command-capf
                              completion-at-point-functions))))))

(defun jcode--install-slash-command-capf-in-live-buffers ()
  "Install slash completion in already-open jcode input buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'jcode-input-mode)
        (jcode--install-slash-command-capf buffer)))))

(defun jcode-execute-slash-command (command)
  "Execute slash COMMAND from the Emacs frontend."
  (interactive
   (list (completing-read "Jcode command: " (mapcar #'car jcode-slash-commands) nil t)))
  (pcase command
    ((or "/?" "/help") (jcode-menu))
    ("/compact" (jcode-compact))
    ("/clear" (jcode-clear))
    ("/split" (jcode-split))
    ("/transfer" (jcode-transfer))
    ("/memory" (jcode-select-feature-state "memory"))
    ("/extract-memory" (jcode-trigger-memory-extraction))
    ("/swarm" (jcode-select-feature-state "swarm"))
    ("/review" (jcode-select-feature-state "autoreview"))
    ("/judge" (jcode-select-feature-state "autojudge"))
    ("/model" (jcode-select-model))
    ("/reasoning" (jcode-select-reasoning-effort))
    ("/fast" (jcode-select-fast-mode))
    ("/transport" (jcode-select-transport))
    ("/premium" (jcode-select-premium-mode))
    ("/z" (jcode-select-premium-mode "normal"))
    ("/zz" (jcode-select-premium-mode "one"))
    ("/zzz" (jcode-select-premium-mode "zero"))
    ("/cancel" (jcode-cancel))
    ("/reconnect" (jcode-reconnect))
    ("/sessions" (jcode-list))
    ("/rename" (call-interactively #'jcode-rename-session))
    ("/compaction-mode" (jcode-select-compaction-mode))
    (_ (user-error "Unknown jcode slash command: %s" command))))

(defun jcode--menu-description ()
  "Return current jcode menu description."
  (let ((chat (ignore-errors (jcode--menu-chat))))
    (if (buffer-live-p chat)
        (with-current-buffer chat
          (format "%s • %s • %s"
                  (or jcode--display-model "model?")
                  (or jcode--display-reasoning-effort "effort?")
                  (or jcode--display-service-tier "fast?")))
      "No jcode session")))

;;;###autoload (autoload 'jcode-menu "jcode-menu" nil t)
(transient-define-prefix jcode-menu ()
  "Show the jcode command menu."
  [:description jcode--menu-description
   ["Session"
    ("j" "open/current" jcode)
    ("l" "list sessions" jcode-list)
    ("r" "reconnect" jcode-reconnect)
    ("a" "attach" jcode-connect)
    ("n" "rename" jcode-rename-session)]
   ["Run"
    ("c" "cancel" jcode-cancel)
    ("k" "disconnect" jcode-disconnect)
    ("C" "clear" jcode-clear)]
   ["Context"
    ("x" "compact" jcode-compact)
    ("m" "compaction mode" jcode-select-compaction-mode)
    ("t" "transfer handoff" jcode-transfer)
    ("s" "split" jcode-split)
    ("M" "extract memory" jcode-trigger-memory-extraction)]
   ["Model"
    ("o" "model" jcode-select-model)
    ("e" "effort" jcode-select-reasoning-effort)
    ("f" "fast tier" jcode-select-fast-mode)
    ("T" "transport" jcode-select-transport)]
   ["Features"
    ("F" "feature on/off" jcode-select-feature-state)
    ("+" "enable feature" jcode-enable-feature)
    ("-" "disable feature" jcode-disable-feature)
    ("p" "premium mode" jcode-select-premium-mode)]])

(jcode--install-slash-command-capf-in-live-buffers)

(provide 'jcode-menu)
;;; jcode-menu.el ends here
