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
(declare-function jcode-refresh-session-list-buffers "jcode-session")
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

(defun jcode--menu-state-value (variable &optional fallback)
  "Return current chat-local VARIABLE value or FALLBACK."
  (let ((chat (ignore-errors (jcode--menu-chat))))
    (if (buffer-live-p chat)
        (with-current-buffer chat
          (or (and (boundp variable) (symbol-value variable)) fallback "default"))
      (or fallback "default"))))

(defun jcode--menu-feature-value (feature)
  "Return current menu display value for FEATURE."
  (let ((states (jcode--menu-state-value 'jcode--display-feature-states nil)))
    (or (and (listp states) (cdr (assoc feature states))) "default")))

(defun jcode--menu-default-value (variable)
  "Return default-setting VARIABLE display value."
  (let ((value (and (boundp variable) (symbol-value variable))))
    (cond
     ((null value) "daemon")
     ((and (listp value) (null value)) "daemon")
     ((listp value)
      (string-join (mapcar (lambda (entry) (format "%s=%s" (car entry) (cdr entry))) value) ","))
     (t (format "%s" value)))))

(defun jcode--menu-setting-description (label variable &optional fallback)
  "Return transient description for LABEL from chat-local VARIABLE."
  (format "%s: %s" label (jcode--menu-state-value variable fallback)))

(defun jcode--menu-feature-description (label feature)
  "Return transient description for LABEL and FEATURE."
  (format "%s: %s" label (jcode--menu-feature-value feature)))

(defun jcode--menu-desc-effort ()
  "Return reasoning effort transient description."
  (jcode--menu-setting-description "effort" 'jcode--display-reasoning-effort))

(defun jcode--menu-desc-fast ()
  "Return fast/service tier transient description."
  (jcode--menu-setting-description "fast" 'jcode--display-service-tier "off"))

(defun jcode--menu-desc-transport ()
  "Return transport transient description."
  (jcode--menu-setting-description "transport" 'jcode--display-transport))

(defun jcode--menu-desc-premium ()
  "Return premium mode transient description."
  (jcode--menu-setting-description "premium" 'jcode--display-premium-mode))

(defun jcode--menu-desc-compaction ()
  "Return compaction mode transient description."
  (jcode--menu-setting-description "compaction" 'jcode--display-compaction-mode))

(defun jcode--menu-desc-memory ()
  "Return memory transient description."
  (jcode--menu-feature-description "memory" "memory"))

(defun jcode--menu-desc-swarm ()
  "Return swarm transient description."
  (jcode--menu-feature-description "swarm" "swarm"))

(defun jcode--menu-desc-review ()
  "Return autoreview transient description."
  (jcode--menu-feature-description "review" "autoreview"))

(defun jcode--menu-desc-judge ()
  "Return autojudge transient description."
  (jcode--menu-feature-description "judge" "autojudge"))

(defun jcode--menu-desc-default-model ()
  "Return default model transient description."
  (format "default model: %s"
          (or (jcode-native-config-provider-value "default_model") "daemon")))

(defun jcode--menu-desc-default-provider ()
  "Return default provider transient description."
  (format "default provider: %s"
          (or (jcode-native-config-provider-value "default_provider") "daemon")))

(defun jcode--menu-desc-default-effort ()
  "Return default effort transient description."
  (format "default effort: %s"
          (or (jcode-native-config-provider-value "openai_reasoning_effort") "daemon")))

(defun jcode--menu-desc-default-fast ()
  "Return default fast/service tier transient description."
  (format "default fast: %s" (jcode--menu-default-value 'jcode-default-service-tier)))

(defun jcode--menu-desc-default-transport ()
  "Return default transport transient description."
  (format "default transport: %s" (jcode--menu-default-value 'jcode-default-transport)))

(defun jcode--menu-desc-default-premium ()
  "Return default premium transient description."
  (format "default premium: %s" (jcode--menu-default-value 'jcode-default-premium-mode)))

(defun jcode--menu-desc-default-compaction ()
  "Return default compaction transient description."
  (format "default compaction: %s" (jcode--menu-default-value 'jcode-default-compaction-mode)))

(defun jcode--menu-desc-default-features ()
  "Return default feature transient description."
  (format "default features: %s" (jcode--menu-default-value 'jcode-default-feature-states)))

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

(defun jcode--set-current-display-metadata (&rest fields)
  "Set display metadata FIELDS on the current chat/input pair."
  (let* ((chat (jcode--menu-chat))
         (input (and (buffer-live-p chat)
                     (buffer-local-value 'jcode--input-buffer chat))))
    (dolist (buffer (delq nil (list chat input)))
      (apply #'jcode--set-display-metadata buffer fields))))

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
  (message "Jcode: rename requested; waiting for server confirmation"))

(defun jcode-select-compaction-mode (&optional mode)
  "Select native jcode compaction MODE."
  (interactive)
  (let ((choice (or mode (completing-read "Compaction mode: " jcode-compaction-modes nil t))))
    (jcode-native-request-current "set_compaction_mode" :mode choice)
    (jcode--set-current-display-metadata :compaction-mode choice)
    (message "Jcode: compaction mode %s" choice)))

(defun jcode-select-transport (&optional transport)
  "Select native jcode TRANSPORT for providers that support it."
  (interactive)
  (let ((choice (or transport (completing-read "Transport: " jcode-transport-modes nil t))))
    (jcode-native-request-current "set_transport" :transport choice)
    (jcode--set-current-display-metadata :transport choice)
    (message "Jcode: transport %s" choice)))

(defun jcode-select-premium-mode (&optional mode)
  "Select native jcode Copilot premium conservation MODE."
  (interactive)
  (let* ((choice (or mode (completing-read "Premium mode: " (mapcar #'car jcode-premium-modes) nil t)))
         (value (cdr (assoc choice jcode-premium-modes))))
    (unless value (user-error "Unknown premium mode: %s" choice))
    (jcode-native-request-current "set_premium_mode" :mode value)
    (jcode--set-current-display-metadata :premium-mode choice)
    (message "Jcode: premium mode %s" choice)))

(defun jcode-set-feature (feature enabled)
  "Set native jcode runtime FEATURE to ENABLED."
  (interactive
   (list (completing-read "Feature: " jcode-feature-toggles nil t)
         (y-or-n-p "Enable feature? ")))
  (let ((state (if enabled "on" "off"))
        (states (copy-alist (let ((chat (jcode--menu-chat)))
                              (with-current-buffer chat
                                jcode--display-feature-states)))))
    (setf (alist-get feature states nil nil #'equal) state)
    (jcode-native-request-current "set_feature" :feature feature :enabled (and enabled t))
    (jcode--set-current-display-metadata :feature-states states)
    (message "Jcode: %s %s" feature state)))

(defun jcode-select-memory-state ()
  "Select native jcode memory feature state."
  (interactive)
  (jcode-select-feature-state "memory"))

(defun jcode-select-swarm-state ()
  "Select native jcode swarm feature state."
  (interactive)
  (jcode-select-feature-state "swarm"))

(defun jcode-select-review-state ()
  "Select native jcode autoreview feature state."
  (interactive)
  (jcode-select-feature-state "autoreview"))

(defun jcode-select-judge-state ()
  "Select native jcode autojudge feature state."
  (interactive)
  (jcode-select-feature-state "autojudge"))

(defun jcode-select-feature-state (&optional feature state)
  "Select native jcode FEATURE STATE, where state is on or off."
  (interactive
   (list (completing-read "Feature: " jcode-feature-toggles nil t)
         (completing-read "State: " '("on" "off") nil t)))
  (let ((choice (or state (completing-read "State: " '("on" "off") nil t))))
    (jcode-set-feature feature (string= choice "on"))))

(defun jcode--read-default-choice (prompt choices)
  "Read PROMPT from daemon/default plus CHOICES, returning nil for daemon."
  (let ((choice (completing-read prompt (cons "daemon" choices) nil t nil nil "daemon")))
    (unless (equal choice "daemon") choice)))

(defun jcode-set-default-model (&optional model)
  "Set jcode daemon default MODEL for future native sessions."
  (interactive
   (let* ((chat (ignore-errors (jcode--menu-chat)))
          (models (and (buffer-live-p chat)
                       (buffer-local-value 'jcode--available-models chat)))
          (choices (append '("daemon") models nil))
          (current (or (jcode-native-config-provider-value "default_model") "daemon"))
          (choice (if models
                      (completing-read (format "Default model (current: %s): " current)
                                       choices nil nil nil nil current)
                    (read-string "Default model (empty = daemon): " nil nil current))))
     (list choice)))
  (let ((value (unless (or (string-empty-p (or model "")) (equal model "daemon")) model)))
    (jcode-native-config-set-provider-value "default_model" value)
    (message "Jcode config: default model %s" (or value "daemon"))))

(defun jcode-set-default-provider (&optional provider)
  "Set jcode daemon default PROVIDER for future native sessions.
Use `openai-oauth' to force ChatGPT/Codex OAuth instead of OpenAI API key."
  (interactive
   (let* ((choices '("daemon" "openai-oauth" "openai-api" "claude-oauth" "claude-api"
                     "copilot" "gemini" "antigravity" "cursor" "bedrock" "openrouter"))
          (current (or (jcode-native-config-provider-value "default_provider") "daemon"))
          (choice (completing-read (format "Default provider (current: %s): " current)
                                   choices nil nil nil nil current)))
     (list choice)))
  (let ((value (unless (or (string-empty-p (or provider ""))
                           (member provider '("daemon" "auto" "clear")))
                 provider)))
    (jcode-native-config-set-provider-value "default_provider" value)
    (message "Jcode config: default provider %s" (or value "daemon"))))

(defun jcode-set-default-reasoning-effort (&optional effort)
  "Set jcode daemon OpenAI reasoning EFFORT for future native sessions."
  (interactive)
  (let ((value (or effort (jcode--read-default-choice "Default effort: " jcode-native-reasoning-efforts))))
    (jcode-native-config-set-provider-value "openai_reasoning_effort" value)
    (ignore-errors (jcode--set-current-display-metadata :reasoning-effort value))
    (message "Jcode config: default OpenAI effort %s" (or value "daemon"))))

(defun jcode-set-default-fast-mode (&optional tier)
  "Set Emacs default service TIER for future native sessions."
  (interactive)
  (setq jcode-default-service-tier
        (or tier (jcode--read-default-choice "Default fast/service tier: " '("off" "flex" "priority" "fast"))))
  (message "Jcode: default fast %s" (or jcode-default-service-tier "daemon")))

(defun jcode-set-default-transport (&optional transport)
  "Set Emacs default TRANSPORT for future native sessions."
  (interactive)
  (setq jcode-default-transport
        (or transport (jcode--read-default-choice "Default transport: " jcode-transport-modes)))
  (message "Jcode: default transport %s" (or jcode-default-transport "daemon")))

(defun jcode-set-default-premium-mode (&optional mode)
  "Set Emacs default premium MODE for future native sessions."
  (interactive)
  (setq jcode-default-premium-mode
        (or mode (jcode--read-default-choice "Default premium mode: " (mapcar #'car jcode-premium-modes))))
  (message "Jcode: default premium %s" (or jcode-default-premium-mode "daemon")))

(defun jcode-set-default-compaction-mode (&optional mode)
  "Set Emacs default compaction MODE for future native sessions."
  (interactive)
  (setq jcode-default-compaction-mode
        (or mode (jcode--read-default-choice "Default compaction mode: " jcode-compaction-modes)))
  (message "Jcode: default compaction %s" (or jcode-default-compaction-mode "daemon")))

(defun jcode-set-default-feature-state (&optional feature state)
  "Set Emacs default FEATURE STATE for future native sessions."
  (interactive
   (list (completing-read "Default feature: " jcode-feature-toggles nil t)
         (jcode--read-default-choice "Default state: " '("on" "off"))))
  (let ((states (copy-alist jcode-default-feature-states)))
    (if state
        (setf (alist-get feature states nil nil #'equal) state)
      (setq states (assoc-delete-all feature states)))
    (setq jcode-default-feature-states states)
    (message "Jcode: default %s %s" feature (or state "daemon"))))

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
  (when (derived-mode-p 'jcode-input-mode)
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "^ \t\n\r")
                   (point))))
      (when (save-excursion
              (goto-char start)
              (looking-at "/"))
        (cons start end)))))

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
    ((or "/?" "/help") (call-interactively #'jcode-menu))
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
    ("-c" "compaction" jcode-select-compaction-mode
     :description jcode--menu-desc-compaction)
    ("t" "transfer handoff" jcode-transfer)
    ("s" "split" jcode-split)
    ("M" "extract memory" jcode-trigger-memory-extraction)]
   ["Model"
    ("o" "model" jcode-select-model)
    ("-e" "effort" jcode-select-reasoning-effort
     :description jcode--menu-desc-effort)
    ("-f" "fast" jcode-select-fast-mode
     :description jcode--menu-desc-fast)
    ("-T" "transport" jcode-select-transport
     :description jcode--menu-desc-transport)]
   ["Features"
    ("-m" "memory" jcode-select-memory-state
     :description jcode--menu-desc-memory)
    ("-w" "swarm" jcode-select-swarm-state
     :description jcode--menu-desc-swarm)
    ("-r" "review" jcode-select-review-state
     :description jcode--menu-desc-review)]
   ["Defaults (future sessions)"
    ("P" "default provider" jcode-set-default-provider
     :description jcode--menu-desc-default-provider)
    ("O" "default model" jcode-set-default-model
     :description jcode--menu-desc-default-model)
    ("E" "default effort" jcode-set-default-reasoning-effort
     :description jcode--menu-desc-default-effort)
    ("F" "default fast" jcode-set-default-fast-mode
     :description jcode--menu-desc-default-fast)
    ("T" "default transport" jcode-set-default-transport
     :description jcode--menu-desc-default-transport)
    ("G" "default compaction" jcode-set-default-compaction-mode
     :description jcode--menu-desc-default-compaction)
    ("H" "default feature" jcode-set-default-feature-state
     :description jcode--menu-desc-default-features)]])

(jcode--install-slash-command-capf-in-live-buffers)

(provide 'jcode-menu)
;;; jcode-menu.el ends here
