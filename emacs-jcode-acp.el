;;; emacs-jcode-acp.el --- ACP transport for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
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

(cl-defstruct (emacs-jcode--acp-connection (:constructor emacs-jcode--make-acp-connection))
  process session next-id callbacks line-buffer)

(defvar emacs-jcode--sessions nil
  "Live emacs-jcode sessions.")

(defun emacs-jcode--json-read-line (line)
  "Parse JSON LINE as an alist object."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string line)))

(defun emacs-jcode--json-encode (object)
  "Encode OBJECT as compact JSON."
  (json-serialize object :false-object :false :null-object nil))

(defun emacs-jcode--acp-send (connection object)
  "Send JSON-RPC OBJECT over CONNECTION as one newline-delimited JSON value."
  (let ((proc (emacs-jcode--acp-connection-process connection)))
    (unless (and proc (process-live-p proc))
      (error "jcode ACP process is not live"))
    (process-send-string proc (concat (emacs-jcode--json-encode object) "\n"))))

(defun emacs-jcode--acp-send-error (connection id code message)
  "Send JSON-RPC error response over CONNECTION."
  (emacs-jcode--acp-send
   connection
   `(:jsonrpc "2.0" :id ,id :error (:code ,code :message ,message))))

(defun emacs-jcode--acp-handle-request (connection id method params)
  "Handle client-side JSON-RPC request METHOD/PARAMS with ID."
  (pcase method
    ("fs/read_text_file"
     (let ((path (or (alist-get 'path params) (alist-get 'uri params))))
       (if (and path (file-readable-p path))
           (emacs-jcode--acp-send
            connection
            `(:jsonrpc "2.0" :id ,id
              :result (:content ,(with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string)))))
         (emacs-jcode--acp-send-error connection id -32602 "File is not readable"))))
    (_
     (emacs-jcode--acp-send-error
      connection id -32601 (format "Unsupported client request: %s" method)))))

(defun emacs-jcode--acp-handle-message (connection message)
  "Handle one JSON-RPC MESSAGE from CONNECTION."
  (let* ((session (emacs-jcode--acp-connection-session connection))
         (id (alist-get 'id message))
         (method (alist-get 'method message))
         (params (alist-get 'params message)))
    (cond
     (method
      (if id
          (emacs-jcode--acp-handle-request connection id method params)
        (emacs-jcode-handle-notification session method params)))
     (id
      (let ((callback (gethash id (emacs-jcode--acp-connection-callbacks connection))))
        (remhash id (emacs-jcode--acp-connection-callbacks connection))
        (if-let ((error (alist-get 'error message)))
            (progn
              (setf (emacs-jcode-session-busy session) nil)
              (emacs-jcode-render-error
               (emacs-jcode-session-chat-buffer session)
               (format "%s" (or (alist-get 'message error) error))))
          (when callback
            (funcall callback (alist-get 'result message)))))))))

(defun emacs-jcode--acp-process-filter (proc string)
  "Process newline-delimited JSON STRING from PROC."
  (let* ((connection (process-get proc 'emacs-jcode-acp-connection))
         (buffer (concat (or (emacs-jcode--acp-connection-line-buffer connection) "") string))
         lines)
    (while (string-match "\n" buffer)
      (push (substring buffer 0 (match-beginning 0)) lines)
      (setq buffer (substring buffer (match-end 0))))
    (setf (emacs-jcode--acp-connection-line-buffer connection) buffer)
    (dolist (line (nreverse lines))
      (unless (string-empty-p (string-trim line))
        (condition-case err
            (emacs-jcode--acp-handle-message connection (emacs-jcode--json-read-line line))
          (error
           (emacs-jcode-render-error
            (emacs-jcode-session-chat-buffer (emacs-jcode--acp-connection-session connection))
            (format "Failed to parse ACP message: %S\n%s" err line))))))))

(defun emacs-jcode--acp-process-sentinel (proc event)
  "Render ACP process PROC lifecycle EVENT."
  (when-let* ((connection (process-get proc 'emacs-jcode-acp-connection))
              (session (emacs-jcode--acp-connection-session connection)))
    (unless (process-live-p proc)
      (setf (emacs-jcode-session-busy session) nil)
      (emacs-jcode-render-info
       (emacs-jcode-session-chat-buffer session)
       (format "ACP process %s" (string-trim event))))))

(defun emacs-jcode--start-acp (session)
  "Start `jcode acp' process and NDJSON JSON-RPC connection for SESSION."
  (let* ((default-directory (emacs-jcode-session-cwd session))
         (command (append (list emacs-jcode-program "acp" "--cwd" default-directory)
                          emacs-jcode-acp-extra-args))
         (proc (let ((process-connection-type nil))
                 (apply #'start-file-process
                        "jcode-acp"
                        (generate-new-buffer " *jcode-acp* ")
                        command)))
         (connection (emacs-jcode--make-acp-connection
                      :process proc
                      :session session
                      :next-id 0
                      :callbacks (make-hash-table :test #'equal)
                      :line-buffer "")))
    (set-process-coding-system proc 'utf-8-emacs-unix 'utf-8-emacs-unix)
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc #'emacs-jcode--acp-process-filter)
    (set-process-sentinel proc #'emacs-jcode--acp-process-sentinel)
    (process-put proc 'emacs-jcode-acp-connection connection)
    (setf (emacs-jcode-session-process session) proc)
    (setf (emacs-jcode-session-connection session) connection)
    connection))

(defun emacs-jcode--acp-load-params (session id)
  "Return ACP session/load or session/resume params for SESSION and ID."
  `(:sessionId ,id :cwd ,(emacs-jcode-session-cwd session)))

(defun emacs-jcode--acp-prompt-params (session text)
  "Return ACP session/prompt params for SESSION and TEXT."
  `(:sessionId ,(emacs-jcode-session-id session)
    :prompt [(:type "text" :text ,text)]))

(defun emacs-jcode--request (session method params &optional callback)
  "Send JSON-RPC METHOD with PARAMS for SESSION.  CALLBACK receives result."
  (let* ((connection (emacs-jcode-session-connection session))
         (id (1+ (emacs-jcode--acp-connection-next-id connection))))
    (setf (emacs-jcode--acp-connection-next-id connection) id)
    (when callback
      (puthash id callback (emacs-jcode--acp-connection-callbacks connection)))
    (condition-case err
        (emacs-jcode--acp-send
         connection
         `(:jsonrpc "2.0" :id ,id :method ,method :params ,params))
      (error
       (remhash id (emacs-jcode--acp-connection-callbacks connection))
       (setf (emacs-jcode-session-busy session) nil)
       (emacs-jcode-render-error
        (emacs-jcode-session-chat-buffer session)
        (format "%s failed to send: %S" method err))))))

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
    (ignore-errors
      (emacs-jcode--request session "session/close"
                            `(:sessionId ,(emacs-jcode-session-id session))
                            (lambda (_result) nil))))
  (when-let ((proc (emacs-jcode-session-process session)))
    (when (process-live-p proc) (delete-process proc)))
  (setq emacs-jcode--sessions (delq session emacs-jcode--sessions))
  (emacs-jcode-render-info (emacs-jcode-session-chat-buffer session) "Disconnected"))

(defun emacs-jcode-session-teardown (session)
  "Silently detach SESSION and remove it from live session tracking."
  (when session
    (when-let ((proc (emacs-jcode-session-process session)))
      (when (process-live-p proc)
        (delete-process proc)))
    (setq emacs-jcode--sessions (delq session emacs-jcode--sessions))))

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
