;;; jcode-native.el --- Native socket backend for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-render)
(require 'jcode-session)

(declare-function jcode-seed-input-history "jcode-input" (prompts))

(cl-defstruct (jcode-native-connection (:constructor jcode--make-native-connection))
  process chat input session-id cwd next-id line-buffer poll-timer last-history-size
  busy followup-queue takeover)

(defcustom jcode-compacted-history-load-count 64
  "Number of compacted messages to add when loading older history."
  :type 'natnum
  :group 'jcode)

(defvar-local jcode--native-connection nil)

(defconst jcode-native-reasoning-efforts '("none" "low" "medium" "high" "xhigh")
  "Reasoning effort values cycled from the header.

`max' exists for some providers, but cycling through it can get stuck when a
provider clamps unsupported `max' back to `xhigh'.")

(defcustom jcode-native-poll-interval 1.0
  "Seconds between native history refreshes for passive session views.

The daemon only pushes live deltas to the owning client for ordinary sessions.
Passive viewers therefore subscribe for the initial history and poll
`get_history' as a non-invasive fallback so current-session buffers keep
updating without taking over another UI client."
  :type 'number
  :group 'jcode)

(defcustom jcode-native-take-over-active-session t
  "Whether native resume should take over the target live session.

When non-nil, Emacs asks the daemon to make this connection the live client for
the selected session.  That is required for token-by-token streaming on normal
sessions because the daemon only sends in-progress deltas to the owning client.
When nil, Emacs remains a passive viewer and relies on polling committed
history."
  :type 'boolean
  :group 'jcode)

(defcustom jcode-native-remote-bridge-program "socat"
  "Program used on TRAMP hosts to bridge stdio to the jcode Unix socket."
  :type 'string
  :group 'jcode)

(defcustom jcode-default-model nil
  "Model to select automatically for newly connected native sessions.
Nil leaves the daemon/session default unchanged."
  :type '(choice (const :tag "Daemon default" nil) string)
  :group 'jcode)

(defcustom jcode-default-reasoning-effort nil
  "Reasoning effort to apply automatically to newly connected native sessions."
  :type '(choice (const :tag "Daemon default" nil)
                 (const "none") (const "low") (const "medium") (const "high") (const "xhigh"))
  :group 'jcode)

(defcustom jcode-default-service-tier nil
  "Service tier to apply automatically to newly connected native sessions."
  :type '(choice (const :tag "Daemon default" nil)
                 (const "off") (const "flex") (const "priority") (const "fast"))
  :group 'jcode)

(defcustom jcode-default-transport nil
  "Transport to apply automatically to newly connected native sessions."
  :type '(choice (const :tag "Daemon default" nil)
                 (const "auto") (const "https") (const "websocket"))
  :group 'jcode)

(defcustom jcode-default-premium-mode nil
  "Copilot premium conservation mode for newly connected native sessions."
  :type '(choice (const :tag "Daemon default" nil)
                 (const "normal") (const "one") (const "zero"))
  :group 'jcode)

(defcustom jcode-default-compaction-mode nil
  "Compaction mode to apply automatically to newly connected native sessions."
  :type '(choice (const :tag "Daemon default" nil)
                 (const "reactive") (const "proactive") (const "semantic"))
  :group 'jcode)

(defcustom jcode-default-feature-states nil
  "Feature states to apply automatically to newly connected native sessions.
Each entry is (FEATURE . STATE), where FEATURE is a string like memory or swarm
and STATE is \"on\" or \"off\"."
  :type '(alist :key-type string :value-type (choice (const "on") (const "off")))
  :group 'jcode)

(defun jcode-native-socket-path (&optional directory)
  "Return best native jcode socket path for DIRECTORY's host."
  (let ((file (jcode--servers-file directory)))
    (or (when (file-readable-p file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents file)
                (let* ((json-object-type 'alist)
                       (json-key-type 'symbol)
                       (data (json-read))
                       (server (cdar data)))
                  (alist-get 'socket server)))
            (error nil)))
        (concat (or (file-remote-p (or directory default-directory)) "")
                "/run/user/" (jcode-native--uid directory) "/jcode.sock"))))

(defun jcode-native--uid (&optional directory)
  "Return the numeric UID string for DIRECTORY's host."
  (if (file-remote-p (or directory default-directory))
      (let* ((directory (file-name-as-directory (or directory default-directory)))
             (uid (ignore-errors
                    (file-attribute-user-id (file-attributes directory 'integer)))))
        (if (integerp uid)
            (number-to-string uid)
          (number-to-string (user-uid))))
    (number-to-string (user-uid))))

(defun jcode-native--host-local-socket-path (socket directory)
  "Return SOCKET as a path local to DIRECTORY's host."
  (if-let ((remote (file-remote-p directory)))
      (if (string-prefix-p remote socket)
          (substring socket (length remote))
        socket)
    socket))

(defun jcode-native--open-process (cwd socket)
  "Open a native protocol process for CWD using SOCKET."
  (if (file-remote-p cwd)
      (let* ((default-directory cwd)
             (remote-socket (jcode-native--host-local-socket-path socket cwd))
             (proc (let ((process-connection-type nil))
                     (start-file-process
                      "jcode-native-remote"
                      (generate-new-buffer " *jcode-native-remote* ")
                      jcode-native-remote-bridge-program
                      "-" (concat "UNIX-CONNECT:" remote-socket)))))
        (set-process-coding-system proc 'utf-8-emacs-unix 'utf-8-emacs-unix)
        (set-process-query-on-exit-flag proc nil)
        proc)
    (make-network-process :name "jcode-native"
                          :buffer (generate-new-buffer " *jcode-native* ")
                          :family 'local
                          :service socket
                          :coding 'utf-8-emacs-unix
                          :noquery t)))

(defun jcode-native--json-read (line)
  "Read native protocol JSON LINE."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string line)))

(defun jcode-native--event-context-window (event)
  "Return context window/max-context tokens from EVENT, if present."
  (or (alist-get 'context_window event)
      (alist-get 'context_window_tokens event)
      (alist-get 'max_context event)
      (alist-get 'max_context_tokens event)
      (alist-get 'context_limit event)
      (alist-get 'context_limit_tokens event)))

(defun jcode-native--send (connection object)
  "Send native protocol OBJECT through CONNECTION."
  (let ((process (and connection (jcode-native-connection-process connection))))
    (unless (and process (process-live-p process))
      (user-error "Native jcode connection is closed; run M-x jcode-reconnect"))
    (condition-case err
        (process-send-string
         process
         (concat (json-serialize object :false-object :false :null-object nil) "\n"))
      (error
       (jcode-native-close connection)
       (user-error "Native jcode connection is closed; run M-x jcode-reconnect (%s)"
                   (error-message-string err))))))

(defun jcode-native--request (connection type &rest fields)
  "Send request TYPE with FIELDS through CONNECTION."
  (let ((id (1+ (jcode-native-connection-next-id connection))))
    (setf (jcode-native-connection-next-id connection) id)
    (jcode-native--send connection (append `(:type ,type :id ,id) fields))
    id))

(defun jcode-native-message (connection content)
  "Send CONTENT as a native message through CONNECTION."
  (setf (jcode-native-connection-busy connection) t)
  (jcode-native--request connection "message" :content content))

(defun jcode-native-steer (connection content)
  "Send CONTENT as a native soft interrupt through CONNECTION."
  (jcode-native--request connection "soft_interrupt" :content content :urgent t))

(defun jcode-native-cancel (connection)
  "Cancel current native generation for CONNECTION."
  (jcode-native--request connection "cancel"))

(defun jcode-native-get-compacted-history (connection visible-messages)
  "Ask CONNECTION to render VISIBLE-MESSAGES from compacted history."
  (jcode-native--request connection "get_compacted_history"
                         :visible_messages visible-messages))

(defun jcode--native-connection-for-command ()
  "Return native connection associated with the current jcode buffer."
  (or (and (bound-and-true-p jcode--native-connection) jcode--native-connection)
      (when (bound-and-true-p jcode--chat-buffer)
        (buffer-local-value 'jcode--native-connection jcode--chat-buffer))))

(defun jcode-load-older-history (&optional count)
  "Load COUNT more compacted history messages into the current chat buffer."
  (interactive "P")
  (let* ((connection (or (jcode--native-connection-for-command)
                         (user-error "No native jcode connection")))
         (chat (jcode-native-connection-chat connection))
         (visible (if (buffer-live-p chat)
                      (with-current-buffer chat (or jcode--compacted-visible 0))
                    0))
         (remaining (and (buffer-live-p chat)
                         (with-current-buffer chat jcode--compacted-remaining)))
         (increment (prefix-numeric-value (or count jcode-compacted-history-load-count)))
         (target (+ visible (max 1 increment))))
    (when (and remaining (<= remaining 0))
      (user-error "No older compacted history remains"))
    (jcode-native-get-compacted-history connection target)
    (message "Jcode: loading older history (%d compacted messages visible)..." target)))

(defun jcode-native-set-model (connection model)
  "Set CONNECTION's active MODEL."
  (jcode-native--request connection "set_model" :model model))

(defun jcode-native--premium-mode-value (mode)
  "Return native premium integer for MODE string."
  (cdr (assoc mode '(("normal" . 0) ("one" . 1) ("zero" . 2)))))

(defun jcode-native-apply-defaults (connection)
  "Apply Emacs-side defaults to newly connected native CONNECTION."
  (when jcode-default-model
    (jcode-native--request connection "set_model" :model jcode-default-model))
  (when jcode-default-reasoning-effort
    (jcode-native--request connection "set_reasoning_effort" :effort jcode-default-reasoning-effort))
  (when jcode-default-service-tier
    (jcode-native--request connection "set_service_tier" :service_tier jcode-default-service-tier))
  (when jcode-default-transport
    (jcode-native--request connection "set_transport" :transport jcode-default-transport))
  (when jcode-default-premium-mode
    (when-let ((value (jcode-native--premium-mode-value jcode-default-premium-mode)))
      (jcode-native--request connection "set_premium_mode" :mode value)))
  (when jcode-default-compaction-mode
    (jcode-native--request connection "set_compaction_mode" :mode jcode-default-compaction-mode))
  (dolist (entry jcode-default-feature-states)
    (let ((feature (car entry))
          (state (cdr entry)))
      (when (member state '("on" "off"))
        (jcode-native--request connection "set_feature"
                               :feature feature
                               :enabled (if (equal state "on") t :false))))))

(defun jcode-native-set-reasoning-effort (connection effort)
  "Set CONNECTION's reasoning EFFORT."
  (jcode-native--request connection "set_reasoning_effort" :effort effort
                         :target_session_id (jcode-native-connection-session-id connection)))

(defun jcode-native-set-service-tier (connection service-tier)
  "Set CONNECTION's SERVICE-TIER."
  (jcode-native--request connection "set_service_tier" :service_tier service-tier))

(defun jcode-native--current-chat ()
  "Return the chat buffer associated with the current jcode buffer."
  (cond
   ((derived-mode-p 'jcode-chat-mode) (current-buffer))
   ((and (boundp 'jcode--chat-buffer) (buffer-live-p jcode--chat-buffer)) jcode--chat-buffer)))

(defun jcode-native--connection-for-command ()
  "Return native connection for a header/menu command."
  (when-let ((chat (jcode-native--current-chat)))
    (buffer-local-value 'jcode--native-connection chat)))

(defun jcode-select-model ()
  "Select the current native jcode model from cached model metadata."
  (interactive)
  (let* ((chat (or (jcode-native--current-chat) (user-error "No jcode session")))
         (connection (or (buffer-local-value 'jcode--native-connection chat)
                         (user-error "No native jcode connection")))
         (models (or (buffer-local-value 'jcode--available-models chat) nil))
         (current (buffer-local-value 'jcode--display-model chat))
         (choice (if models
                     (completing-read (format "Model (current: %s): " (or current "unknown"))
                                      models nil t nil nil current)
                   (read-string (format "Model (current: %s): " (or current "unknown")) nil nil current))))
    (unless (string-empty-p choice)
      (jcode-native-set-model connection choice)
      (dolist (buffer (list chat (jcode-native-connection-input connection)))
        (jcode--set-display-metadata buffer :model choice))
      (message "Jcode: Model set to %s" choice))))

(defun jcode-cycle-reasoning-effort ()
  "Cycle native jcode reasoning effort from the header."
  (interactive)
  (let* ((chat (or (jcode-native--current-chat) (user-error "No jcode session")))
         (connection (or (buffer-local-value 'jcode--native-connection chat)
                         (user-error "No native jcode connection")))
         (current (or (buffer-local-value 'jcode--display-reasoning-effort chat) "none"))
         (tail (member current jcode-native-reasoning-efforts))
         (next (or (cadr tail) (car jcode-native-reasoning-efforts))))
    (jcode-native-set-reasoning-effort connection next)
    (dolist (buffer (list chat (jcode-native-connection-input connection)))
      (jcode--set-display-metadata buffer :reasoning-effort next))
    (message "Jcode: Reasoning effort %s" next)))

(defun jcode-select-reasoning-effort (&optional effort)
  "Select native jcode reasoning EFFORT using completion."
  (interactive)
  (let* ((chat (or (jcode-native--current-chat) (user-error "No jcode session")))
         (connection (or (buffer-local-value 'jcode--native-connection chat)
                         (user-error "No native jcode connection")))
         (current (or (buffer-local-value 'jcode--display-reasoning-effort chat) "none"))
         (choice (or effort
                     (completing-read
                      (format "Reasoning effort (current: %s): " current)
                      jcode-native-reasoning-efforts nil t nil nil current))))
    (unless (string-empty-p choice)
      (jcode-native-set-reasoning-effort connection choice)
      (dolist (buffer (list chat (jcode-native-connection-input connection)))
        (jcode--set-display-metadata buffer :reasoning-effort choice))
      (message "Jcode: Reasoning effort %s" choice))))

(defvar jcode-native-service-tiers '("off" "priority")
  "Native jcode service tiers exposed in the Emacs selector.")

(defun jcode-select-fast-mode (&optional service-tier)
  "Select native jcode fast/service SERVICE-TIER using completion."
  (interactive)
  (let* ((chat (or (jcode-native--current-chat) (user-error "No jcode session")))
         (connection (or (buffer-local-value 'jcode--native-connection chat)
                         (user-error "No native jcode connection")))
         (current (buffer-local-value 'jcode--display-service-tier chat))
         (current-label (or (and current (format "%s" current)) "off"))
         (choice (or service-tier
                     (completing-read
                      (format "Fast/service tier (current: %s): " current-label)
                      jcode-native-service-tiers nil t nil nil current-label))))
    (unless (string-empty-p choice)
      (jcode-native-set-service-tier connection choice)
      (dolist (buffer (list chat (jcode-native-connection-input connection)))
        (jcode--set-display-metadata buffer :service-tier choice))
      (message "Jcode: fast mode %s" (if (member choice '("priority" "fast")) "on" "off")))))

(defun jcode-native--drain-followup (connection)
  "Send next queued follow-up for CONNECTION, if any."
  (when-let ((text (pop (jcode-native-connection-followup-queue connection))))
    (jcode-render-user (jcode-native-connection-chat connection) text)
    (jcode--section (jcode-native-connection-chat connection) "Assistant" 'jcode-assistant-face)
    (jcode-native-message connection text)))

(defun jcode-native--mark-busy (connection)
  "Mark CONNECTION as actively processing a turn."
  (setf (jcode-native-connection-busy connection) t))

(defun jcode-native-close (connection)
  "Close native CONNECTION and cancel its refresh timer."
  (when connection
    (when-let ((timer (jcode-native-connection-poll-timer connection)))
      (cancel-timer timer)
      (setf (jcode-native-connection-poll-timer connection) nil))
    (when-let ((proc (jcode-native-connection-process connection)))
      (when (process-live-p proc)
        (delete-process proc)))))

(defun jcode-native--kill-buffer-hook ()
  "Close native connection associated with the current buffer."
  (when (bound-and-true-p jcode--native-connection)
    (jcode-native-close jcode--native-connection)))

(defun jcode-native--poll (connection)
  "Refresh passive native CONNECTION from daemon history."
  (if (and (jcode-native-connection-process connection)
           (process-live-p (jcode-native-connection-process connection)))
      (unless (jcode-native-connection-busy connection)
        (condition-case nil
            (jcode-native--request connection "get_history")
          (error nil)))
    (jcode-native-close connection)))

(defun jcode-native--render-history-message (chat message)
  "Render native history MESSAGE into CHAT."
  (let ((role (alist-get 'role message))
        (content (or (alist-get 'content message) "")))
    (pcase role
      ("user" (jcode-render-user chat content))
      ("assistant" (jcode-render-assistant-message chat content))
      ("tool" (jcode-render-tool chat `((title . "tool") (status . "done") (content . ,content)) t))
      (_ (jcode-render-info chat (format "%s: %s" role content))))))

(defun jcode-native--render-history (connection event)
  "Render native history EVENT for CONNECTION."
  (let* ((chat (jcode-native-connection-chat connection))
         (input (jcode-native-connection-input connection))
         (messages (append (alist-get 'messages event) nil))
         (history-size (length (prin1-to-string messages))))
    (jcode-native--remember-compacted-history connection event)
    (unless (equal history-size (jcode-native-connection-last-history-size connection))
      (setf (jcode-native-connection-last-history-size connection) history-size)
      (when (and (buffer-live-p input) (fboundp 'jcode-seed-input-history))
        (with-current-buffer input
          (jcode-seed-input-history
           (delq nil
                 (mapcar (lambda (message)
                           (when (equal (alist-get 'role message) "user")
                             (alist-get 'content message)))
                         messages)))))
      (jcode--clear-chat-buffer chat)
      (jcode--session-set-display-native connection event)
      (mapc (lambda (message) (jcode-native--render-history-message chat message))
            messages))))

(defun jcode-native--remember-compacted-history (connection event)
  "Store compacted-history counters from EVENT on CONNECTION and buffers."
  (let ((total (alist-get 'compacted_total event))
        (visible (alist-get 'compacted_visible event))
        (remaining (alist-get 'compacted_remaining event)))
    (dolist (buffer (list (jcode-native-connection-chat connection)
                          (jcode-native-connection-input connection)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when total (setq jcode--compacted-total total))
          (when visible (setq jcode--compacted-visible visible))
          (when remaining (setq jcode--compacted-remaining remaining)))))))

(defun jcode-native--render-compacted-history (connection event)
  "Render expanded compacted-history EVENT for CONNECTION."
  (let* ((chat (jcode-native-connection-chat connection))
         (messages (append (alist-get 'messages event) nil))
         (windows (get-buffer-window-list chat nil t))
         (old-size (with-current-buffer chat (buffer-size)))
         (window-states (mapcar (lambda (window)
                                  (list window
                                        (with-current-buffer chat
                                          (- old-size (window-start window)))
                                        (with-current-buffer chat
                                          (- old-size (window-point window)))))
                                windows)))
    (jcode-native--remember-compacted-history connection event)
    (jcode--clear-chat-buffer chat)
    (jcode--session-set-display-native connection event)
    (mapc (lambda (message) (jcode-native--render-history-message chat message)) messages)
    (let ((new-size (with-current-buffer chat (buffer-size))))
      (dolist (state window-states)
        (pcase-let ((`(,window ,start-from-end ,point-from-end) state))
          (when (window-live-p window)
            (with-current-buffer chat
              (set-window-start window (max (point-min) (- new-size start-from-end)) t)
              (set-window-point window (max (point-min) (- new-size point-from-end))))))))
    (message "Jcode: loaded older history (%s visible, %s remaining)"
             (or (alist-get 'compacted_visible event) "?")
             (or (alist-get 'compacted_remaining event) "?"))))

(defun jcode-native--sentinel (proc _event)
  "Clean native connection state when PROC exits."
  (when-let ((connection (process-get proc 'jcode-native-connection)))
    (jcode-native-close connection)))

(defun jcode--session-set-display-native (connection event)
  "Apply native EVENT metadata to CONNECTION buffers."
  (let ((model (alist-get 'provider_model event))
        (provider (alist-get 'provider_name event))
        (server (alist-get 'server_name event))
        (reasoning (alist-get 'reasoning_effort event))
        (service-tier (alist-get 'service_tier event))
        (transport (alist-get 'transport event))
        (compaction-mode (alist-get 'compaction_mode event))
        (credential (alist-get 'resolved_credential event))
        (total-tokens (alist-get 'total_tokens event))
        (token-usage-totals (alist-get 'token_usage_totals event))
        (context-window (jcode-native--event-context-window event))
        (client-count (alist-get 'client_count event))
        (connection-type (alist-get 'connection_type event))
        (activity (alist-get 'activity event))
        (available-models (append (alist-get 'available_models event) nil)))
    (dolist (buffer (list (jcode-native-connection-chat connection)
                          (jcode-native-connection-input connection)))
      (jcode--set-display-metadata
       buffer
       :session-id (jcode-native-connection-session-id connection)
       :title (or server (jcode-native-connection-session-id connection))
       :status nil
       :model model
       :provider provider
       :reasoning-effort reasoning
       :service-tier service-tier
       :transport transport
       :compaction-mode compaction-mode
       :credential credential
       :total-tokens total-tokens
       :token-usage-totals token-usage-totals
       :context-window context-window
       :client-count client-count
       :connection-type connection-type
       :owner (if (jcode-native-connection-takeover connection) 'owned 'viewing)
       :activity activity
       :available-models available-models))))

(defun jcode-native--handle-event (connection event)
  "Handle native protocol EVENT for CONNECTION."
  (let ((type (alist-get 'type event))
        (chat (jcode-native-connection-chat connection)))
    (when-let ((session-id (or (alist-get 'session_id event)
                               (alist-get 'sessionId event))))
      (unless (equal session-id (jcode-native-connection-session-id connection))
        (setf (jcode-native-connection-session-id connection) session-id)
        (when (fboundp 'jcode-apply-session-info-to-buffers)
          (jcode-apply-session-info-to-buffers
           session-id chat (jcode-native-connection-input connection)))))
    (pcase type
      ("history" (jcode-native--render-history connection event))
      ("compacted_history" (jcode-native--render-compacted-history connection event))
      ("ack" nil)
      ("done"
       (setf (jcode-native-connection-busy connection) nil)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata buffer :activity '((is_processing . :false))))
       (jcode-native--drain-followup connection))
      ("tokens"
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata
         buffer :token-usage-totals
          `((input_tokens . ,(alist-get 'input event))
            (output_tokens . ,(alist-get 'output event))
            (cache_read_input_tokens . ,(or (alist-get 'cache_read_input event) 0))
            (cache_creation_input_tokens . ,(or (alist-get 'cache_creation_input event) 0)))
          :context-window (jcode-native--event-context-window event))))
      ("model_changed"
       (if-let ((error (alist-get 'error event)))
           (jcode-render-error chat error)
         (dolist (buffer (list chat (jcode-native-connection-input connection)))
           (jcode--set-display-metadata buffer :model (alist-get 'model event)
                                        :provider (alist-get 'provider_name event)))))
      ("reasoning_effort_changed"
       (if-let ((error (alist-get 'error event)))
           (jcode-render-error chat error)
	 (dolist (buffer (list chat (jcode-native-connection-input connection)))
	   (jcode--set-display-metadata buffer :reasoning-effort (alist-get 'effort event)))))
      ("service_tier_changed"
       (if-let ((error (alist-get 'error event)))
           (jcode-render-error chat error)
         (dolist (buffer (list chat (jcode-native-connection-input connection)))
           (jcode--set-display-metadata buffer :service-tier (or (alist-get 'service_tier event) "off")))))
      ("transport_changed"
       (if-let ((error (alist-get 'error event)))
           (jcode-render-error chat error)
         (dolist (buffer (list chat (jcode-native-connection-input connection)))
           (jcode--set-display-metadata buffer :transport (alist-get 'transport event)))))
      ("compaction_mode_changed"
       (if-let ((error (alist-get 'error event)))
           (jcode-render-error chat error)
         (dolist (buffer (list chat (jcode-native-connection-input connection)))
           (jcode--set-display-metadata buffer :compaction-mode (alist-get 'mode event)))))
      ("reasoning_delta"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata buffer :activity '((is_processing . t) (phase . "thinking"))))
       (when-let ((text (or (alist-get 'text event)
                            (alist-get 'delta event)
                            (alist-get 'content event))))
         (jcode-render-thinking-delta chat text)))
      ("text_delta"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata buffer :activity '((is_processing . t) (phase . "responding"))))
       (jcode-render-assistant-message chat (alist-get 'text event)))
      ("text_replace"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata buffer :activity '((is_processing . t) (phase . "responding"))))
       (jcode-render-assistant-message chat (alist-get 'text event)))
      ("tool_start"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata
          buffer :activity `((is_processing . t) (current_tool_name . ,(alist-get 'name event)))))
       (jcode-render-tool chat event nil))
      ("tool_input"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata
          buffer :activity `((is_processing . t) (current_tool_name . ,(alist-get 'name event))))))
      ("tool_exec"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata
          buffer :activity `((is_processing . t) (current_tool_name . ,(alist-get 'name event)))))
       (jcode-render-tool chat event t))
      ("tool_done"
       (jcode-native--mark-busy connection)
       (dolist (buffer (list chat (jcode-native-connection-input connection)))
         (jcode--set-display-metadata
          buffer :activity `((is_processing . t) (current_tool_name . ,(alist-get 'name event)))))
       (jcode-render-tool chat event t))
      ("session_renamed"
       (let* ((title (or (alist-get 'display_title event)
                         (alist-get 'title event)))
              (chat (jcode-native-connection-chat connection))
              (input (jcode-native-connection-input connection)))
         (dolist (buffer (list chat input))
           (jcode--set-display-metadata buffer :title title))
         (jcode--rename-display-buffers chat input title)
         (jcode-refresh-session-list-buffers)))
      ("error" (jcode-render-error chat (or (alist-get 'message event) (format "%S" event))))
      (_ nil))))

(defun jcode-native--filter (proc string)
  "Handle native protocol output STRING from PROC."
  (let* ((connection (process-get proc 'jcode-native-connection))
         (buffer (concat (or (jcode-native-connection-line-buffer connection) "") string))
         lines)
    (while (string-match "\n" buffer)
      (push (substring buffer 0 (match-beginning 0)) lines)
      (setq buffer (substring buffer (match-end 0))))
    (setf (jcode-native-connection-line-buffer connection) buffer)
    (dolist (line (nreverse lines))
      (unless (string-empty-p (string-trim line))
        (condition-case err
            (jcode-native--handle-event connection (jcode-native--json-read line))
          (error (jcode-render-error
                  (jcode-native-connection-chat connection)
                  (format "native parse error: %S" err))))))))

(defun jcode-native-open-session (session-id cwd chat input)
  "Open native live SESSION-ID for CWD into CHAT and INPUT."
  (let* ((socket (jcode-native-socket-path cwd))
         (proc (jcode-native--open-process cwd socket))
         (connection (jcode--make-native-connection
                      :process proc :chat chat :input input :session-id session-id
                      :cwd cwd :next-id 0 :line-buffer "")))
    (set-process-filter proc #'jcode-native--filter)
    (set-process-sentinel proc #'jcode-native--sentinel)
    (process-put proc 'jcode-native-connection connection)
    (with-current-buffer chat
      (setq jcode--native-connection connection)
      (add-hook 'kill-buffer-hook #'jcode-native--kill-buffer-hook nil t))
    (with-current-buffer input
      (setq jcode--native-connection connection)
      (add-hook 'kill-buffer-hook #'jcode-native--kill-buffer-hook nil t))
    (jcode-native--request
     connection "subscribe"
     :working_dir cwd
     :target_session_id session-id
     :client_has_local_history (if jcode-native-take-over-active-session t :false)
     :allow_session_takeover (if jcode-native-take-over-active-session t :false))
    (unless jcode-native-take-over-active-session
      (setf (jcode-native-connection-poll-timer connection)
            (run-with-timer jcode-native-poll-interval
                            jcode-native-poll-interval
                            #'jcode-native--poll connection)))
    (setf (jcode-native-connection-takeover connection)
          jcode-native-take-over-active-session)
    (dolist (buffer (list chat input))
      (jcode--set-display-metadata
       buffer :owner (if jcode-native-take-over-active-session 'owned 'viewing)))
    (jcode-native-apply-defaults connection)
    connection))

(provide 'jcode-native)
;;; jcode-native.el ends here
