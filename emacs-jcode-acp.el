;;; emacs-jcode-acp.el --- ACP transport for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'emacs-jcode-ui)
(require 'emacs-jcode-render)

(defcustom emacs-jcode-program "jcode"
  "Program used to launch jcode clients such as `jcode acp'."
  :type 'string
  :group 'emacs-jcode)

(defcustom emacs-jcode-acp-extra-args '("--quiet")
  "Extra arguments appended to `jcode acp'."
  :type '(repeat string)
  :group 'emacs-jcode)

(cl-defstruct (emacs-jcode-session (:constructor emacs-jcode--make-session))
  id cwd chat-buffer input-buffer process connection initialized busy)

(defvar emacs-jcode--sessions nil
  "Live emacs-jcode sessions.")

(defun emacs-jcode--notification-dispatcher (session)
  "Return JSON-RPC notification dispatcher for SESSION."
  (lambda (_conn method params)
    (emacs-jcode-handle-notification session method params)))

(defun emacs-jcode--request-dispatcher (_session)
  "Return JSON-RPC request dispatcher for client-side ACP requests."
  (lambda (_conn method params)
    (pcase method
      ;; Basic permissive defaults.  jcode ACP usually does not need these,
      ;; but returning JSON-ish values keeps the adapter unblocked if it asks.
      ("fs/read_text_file"
       (let ((path (or (alist-get 'path params) (alist-get 'uri params))))
         (if (and path (file-readable-p path))
             `(:content ,(with-temp-buffer (insert-file-contents path) (buffer-string)))
           (jsonrpc-error "File is not readable"))))
      (_ (jsonrpc-error (format "Unsupported client request: %s" method))))))

(defun emacs-jcode--start-acp (session)
  "Start `jcode acp' process and JSON-RPC CONNECTION for SESSION."
  (let* ((default-directory (emacs-jcode-session-cwd session))
         (command (append (list emacs-jcode-program "acp" "--cwd" default-directory)
                          emacs-jcode-acp-extra-args))
         (proc (let ((process-connection-type nil))
                 (apply #'start-file-process
                        "jcode-acp"
                        (generate-new-buffer " *jcode-acp* ")
                        command))))
    (set-process-coding-system proc 'utf-8-emacs-unix 'utf-8-emacs-unix)
    (set-process-query-on-exit-flag proc nil)
    ;; `start-file-process' preserves TRAMP semantics through `default-directory'.
    ;; It cannot attach a separate stderr buffer portably, so stderr is merged
    ;; with stdout by the underlying adapter when supported.
    (setf (emacs-jcode-session-process session) proc)
    (let ((conn (make-instance 'jsonrpc-process-connection
                               :name "jcode-acp"
                               :process proc
                               :notification-dispatcher (emacs-jcode--notification-dispatcher session)
                               :request-dispatcher (emacs-jcode--request-dispatcher session)
                               :on-shutdown (lambda (_conn)
                                              (emacs-jcode-render-info
                                               (emacs-jcode-session-chat-buffer session)
                                               "ACP connection closed")))))
      (setf (emacs-jcode-session-connection session) conn)
      conn)))

(defun emacs-jcode--acp-load-params (session id)
  "Return ACP session/load or session/resume params for SESSION and ID."
  `(:sessionId ,id :cwd ,(emacs-jcode-session-cwd session)))

(defun emacs-jcode--acp-prompt-params (session text)
  "Return ACP session/prompt params for SESSION and TEXT."
  `(:sessionId ,(emacs-jcode-session-id session)
    :prompt [(:type "text" :text ,text)]))

(defun emacs-jcode--request (session method params &optional callback)
  "Send JSON-RPC METHOD with PARAMS for SESSION.  CALLBACK receives result."
  (let ((conn (emacs-jcode-session-connection session)))
    (if callback
        (jsonrpc-async-request conn method params
                               :success-fn callback
                               :error-fn (lambda (err)
                                           (setf (emacs-jcode-session-busy session) nil)
                                           (emacs-jcode-render-error
                                            (emacs-jcode-session-chat-buffer session)
                                            (format "%s failed: %S" method err))))
      (jsonrpc-request conn method params))))

(defun emacs-jcode--session-set-display (session &rest args)
  "Apply display ARGS to both buffers for SESSION."
  (apply #'emacs-jcode--set-display-metadata (emacs-jcode-session-chat-buffer session) args)
  (apply #'emacs-jcode--set-display-metadata (emacs-jcode-session-input-buffer session) args))

(defun emacs-jcode-session-initialize (session callback)
  "Initialize ACP SESSION then call CALLBACK."
  (emacs-jcode--request
   session "initialize"
   '(:protocolVersion 1
     :clientCapabilities (:loadSession t :promptCapabilities (:image t :embeddedContext t)))
   (lambda (_result)
     (setf (emacs-jcode-session-initialized session) t)
     (funcall callback session))))

(defun emacs-jcode-session-new (session)
  "Create a daemon jcode session for SESSION."
  (emacs-jcode--request
   session "session/new" `(:cwd ,(emacs-jcode-session-cwd session))
   (lambda (result)
     (when-let ((id (or (alist-get 'sessionId result) (alist-get 'id result))))
       (setf (emacs-jcode-session-id session) id)
       (emacs-jcode--session-set-display session :session-id id :status "Active"))
     (emacs-jcode-render-info (emacs-jcode-session-chat-buffer session)
                              (format "Connected to jcode%s"
                                      (if (emacs-jcode-session-id session)
                                          (format " session %s" (emacs-jcode-session-id session))
                                        ""))))))

(defun emacs-jcode-session-load (session id &optional resume-only)
  "Load or resume jcode session ID into SESSION.  RESUME-ONLY uses session/resume."
  (setf (emacs-jcode-session-id session) id)
  (setf (emacs-jcode-session-busy session) t)
  (emacs-jcode--session-set-display session :session-id id :status "Loading")
  (emacs-jcode--request session (if resume-only "session/resume" "session/load")
                        (emacs-jcode--acp-load-params session id)
                        (lambda (_result)
                          (setf (emacs-jcode-session-busy session) nil)
                          (emacs-jcode--session-set-display session :session-id id :status "Active")
                          (emacs-jcode-render-info
                           (emacs-jcode-session-chat-buffer session)
                           (format "%s session %s" (if resume-only "Resumed" "Loaded") id)))))

(defun emacs-jcode-session-prompt (session text)
  "Send TEXT as prompt for SESSION."
  (setf (emacs-jcode-session-busy session) t)
  (emacs-jcode--session-set-display session :status "Working")
  (emacs-jcode--request session "session/prompt"
                        (emacs-jcode--acp-prompt-params session text)
                        (lambda (_result)
                          (setf (emacs-jcode-session-busy session) nil)
                          (emacs-jcode--session-set-display session :status "Idle"))))

(defun emacs-jcode-session-cancel (session)
  "Cancel active prompt in SESSION."
  (emacs-jcode--request session "session/cancel"
                        `(:sessionId ,(emacs-jcode-session-id session))
                        (lambda (_result)
                          (emacs-jcode-render-info (emacs-jcode-session-chat-buffer session) "Cancel requested"))))

(defun emacs-jcode-session-close (session)
  "Close/detach SESSION."
  (when (emacs-jcode-session-connection session)
    (emacs-jcode--request session "session/close"
                          `(:sessionId ,(emacs-jcode-session-id session))
                          (lambda (_result) nil))
    (ignore-errors (jsonrpc-shutdown (emacs-jcode-session-connection session))))
  (when-let ((proc (emacs-jcode-session-process session)))
    (when (process-live-p proc) (delete-process proc)))
  (setq emacs-jcode--sessions (delq session emacs-jcode--sessions))
  (emacs-jcode-render-info (emacs-jcode-session-chat-buffer session) "Disconnected"))

(defun emacs-jcode-session-start (cwd chat input &optional session-id resume-only)
  "Start ACP client for CWD and buffers CHAT/INPUT.
When SESSION-ID is non-nil, load or resume it depending on RESUME-ONLY."
  (let ((session (emacs-jcode--make-session :cwd cwd :chat-buffer chat :input-buffer input)))
    (with-current-buffer chat (setq emacs-jcode--session session))
    (with-current-buffer input (setq emacs-jcode--session session))
    (push session emacs-jcode--sessions)
    (emacs-jcode--start-acp session)
    (emacs-jcode-session-initialize
     session
     (lambda (s)
       (if session-id
           (emacs-jcode-session-load s session-id resume-only)
         (emacs-jcode-session-new s))))
    session))

(provide 'emacs-jcode-acp)
;;; emacs-jcode-acp.el ends here
