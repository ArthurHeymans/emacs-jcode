;;; jcode-acp.el --- ACP transport for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-render)

(declare-function jcode-apply-session-info-to-buffers "jcode-session"
                  (session-id chat input))

(defcustom jcode-program "jcode"
  "Program used to launch jcode clients such as `jcode acp'."
  :type 'string
  :group 'jcode)

(defcustom jcode-acp-extra-args '("--quiet")
  "Extra arguments appended to `jcode acp'."
  :type '(repeat string)
  :group 'jcode)

(cl-defstruct (jcode-session (:constructor jcode--make-session))
  id cwd chat-buffer input-buffer process connection initialized busy)

(cl-defstruct (jcode--acp-connection (:constructor jcode--make-acp-connection))
  process session next-id callbacks line-buffer)

(defvar jcode--sessions nil
  "Live jcode sessions.")

(defun jcode--json-read-line (line)
  "Parse JSON LINE as an alist object."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string line)))

(defun jcode--json-encode (object)
  "Encode OBJECT as compact JSON."
  (json-serialize object :false-object :false :null-object nil))

(defun jcode--acp-send (connection object)
  "Send JSON-RPC OBJECT over CONNECTION as one newline-delimited JSON value."
  (let ((proc (jcode--acp-connection-process connection)))
    (unless (and proc (process-live-p proc))
      (error "jcode ACP process is not live"))
    (process-send-string proc (concat (jcode--json-encode object) "\n"))))

(defun jcode--acp-send-error (connection id code message)
  "Send JSON-RPC error response over CONNECTION."
  (jcode--acp-send
   connection
   `(:jsonrpc "2.0" :id ,id :error (:code ,code :message ,message))))

(defun jcode--acp-handle-request (connection id method params)
  "Handle client-side JSON-RPC request METHOD/PARAMS with ID."
  (pcase method
    ("fs/read_text_file"
     (let ((path (or (alist-get 'path params) (alist-get 'uri params))))
       (if (and path (file-readable-p path))
           (jcode--acp-send
            connection
            `(:jsonrpc "2.0" :id ,id
              :result (:content ,(with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string)))))
         (jcode--acp-send-error connection id -32602 "File is not readable"))))
    (_
     (jcode--acp-send-error
      connection id -32601 (format "Unsupported client request: %s" method)))))

(defun jcode--acp-handle-message (connection message)
  "Handle one JSON-RPC MESSAGE from CONNECTION."
  (let* ((session (jcode--acp-connection-session connection))
         (id (alist-get 'id message))
         (method (alist-get 'method message))
         (params (alist-get 'params message)))
    (cond
     (method
      (if id
          (jcode--acp-handle-request connection id method params)
        (jcode-handle-notification session method params)))
     (id
      (let ((callback (gethash id (jcode--acp-connection-callbacks connection))))
        (remhash id (jcode--acp-connection-callbacks connection))
        (if-let ((error (alist-get 'error message)))
            (progn
              (setf (jcode-session-busy session) nil)
              (jcode-render-error
               (jcode-session-chat-buffer session)
               (format "%s" (or (alist-get 'message error) error))))
          (when callback
            (funcall callback (alist-get 'result message)))))))))

(defun jcode--acp-process-filter (proc string)
  "Process newline-delimited JSON STRING from PROC."
  (let* ((connection (process-get proc 'jcode-acp-connection))
         (buffer (concat (or (jcode--acp-connection-line-buffer connection) "") string))
         lines)
    (while (string-match "\n" buffer)
      (push (substring buffer 0 (match-beginning 0)) lines)
      (setq buffer (substring buffer (match-end 0))))
    (setf (jcode--acp-connection-line-buffer connection) buffer)
    (dolist (line (nreverse lines))
      (unless (string-empty-p (string-trim line))
        (condition-case err
            (jcode--acp-handle-message connection (jcode--json-read-line line))
          (error
           (jcode-render-error
            (jcode-session-chat-buffer (jcode--acp-connection-session connection))
            (format "Failed to parse ACP message: %S" err))))))))

(defun jcode--acp-process-sentinel (proc event)
  "Render ACP process PROC lifecycle EVENT."
  (when-let* ((connection (process-get proc 'jcode-acp-connection))
              (session (jcode--acp-connection-session connection)))
    (unless (process-live-p proc)
      (setf (jcode-session-busy session) nil)
      (jcode-render-info
       (jcode-session-chat-buffer session)
       (format "ACP process %s" (string-trim event))))))

(defun jcode--start-acp (session)
  "Start `jcode acp' process and NDJSON JSON-RPC connection for SESSION."
  (let* ((default-directory (jcode-session-cwd session))
         (command (append (list jcode-program "acp" "--cwd" default-directory)
                          jcode-acp-extra-args))
         (proc (let ((process-connection-type nil))
                 (apply #'start-file-process
                        "jcode-acp"
                        (generate-new-buffer " *jcode-acp* ")
                        command)))
         (connection (jcode--make-acp-connection
                      :process proc
                      :session session
                      :next-id 0
                      :callbacks (make-hash-table :test #'equal)
                      :line-buffer "")))
    (set-process-coding-system proc 'utf-8-emacs-unix 'utf-8-emacs-unix)
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc #'jcode--acp-process-filter)
    (set-process-sentinel proc #'jcode--acp-process-sentinel)
    (process-put proc 'jcode-acp-connection connection)
    (setf (jcode-session-process session) proc)
    (setf (jcode-session-connection session) connection)
    connection))

(defun jcode--acp-load-params (session id)
  "Return ACP session/load or session/resume params for SESSION and ID."
  `(:sessionId ,id :cwd ,(jcode-session-cwd session)))

(defun jcode--acp-prompt-params (session text)
  "Return ACP session/prompt params for SESSION and TEXT."
  `(:sessionId ,(jcode-session-id session)
    :prompt [(:type "text" :text ,text)]))

(defun jcode--request (session method params &optional callback)
  "Send JSON-RPC METHOD with PARAMS for SESSION.  CALLBACK receives result."
  (let* ((connection (jcode-session-connection session))
         (id (1+ (jcode--acp-connection-next-id connection))))
    (setf (jcode--acp-connection-next-id connection) id)
    (when callback
      (puthash id callback (jcode--acp-connection-callbacks connection)))
    (condition-case err
        (jcode--acp-send
         connection
         `(:jsonrpc "2.0" :id ,id :method ,method :params ,params))
      (error
       (remhash id (jcode--acp-connection-callbacks connection))
       (setf (jcode-session-busy session) nil)
       (jcode-render-error
        (jcode-session-chat-buffer session)
        (format "%s failed to send: %S" method err))))))

(defun jcode--session-set-display (session &rest args)
  "Apply display ARGS to both buffers for SESSION."
  (apply #'jcode--set-display-metadata (jcode-session-chat-buffer session) args)
  (apply #'jcode--set-display-metadata (jcode-session-input-buffer session) args))

(defun jcode-session-initialize (session callback)
  "Initialize ACP SESSION then call CALLBACK."
  (jcode--request
   session "initialize"
   '(:protocolVersion 1
     :clientCapabilities (:loadSession t :promptCapabilities (:image t :embeddedContext t)))
   (lambda (_result)
     (setf (jcode-session-initialized session) t)
     (funcall callback session))))

(defun jcode-session-new (session &optional callback)
  "Create a daemon jcode session for SESSION."
  (jcode--request
   session "session/new" `(:cwd ,(jcode-session-cwd session))
    (lambda (result)
	     (when-let ((id (or (alist-get 'sessionId result) (alist-get 'id result))))
	       (setf (jcode-session-id session) id)
	       (jcode--session-set-display session :session-id id :status "Active")
	       (when (fboundp 'jcode-apply-session-info-to-buffers)
		 (run-at-time
		  0.2 nil
		  (lambda (session-id chat input)
		    (when (and (buffer-live-p chat) (buffer-live-p input))
		      (jcode-apply-session-info-to-buffers session-id chat input)))
		  id
		  (jcode-session-chat-buffer session)
		  (jcode-session-input-buffer session))))
     (jcode-render-info (jcode-session-chat-buffer session)
                              (format "Connected to jcode%s"
                                      (if (jcode-session-id session)
                                          (format " session %s" (jcode-session-id session))
                                        "")))
     (setf (jcode-session-busy session) nil)
     (when callback (funcall callback session)))))

(defun jcode-session-load (session id &optional resume-only callback)
  "Load or resume jcode session ID into SESSION.  RESUME-ONLY uses session/resume."
  (setf (jcode-session-id session) id)
  (setf (jcode-session-busy session) t)
  (jcode--session-set-display session :session-id id :status "Loading")
  (jcode--request session (if resume-only "session/resume" "session/load")
                        (jcode--acp-load-params session id)
                        (lambda (_result)
                          (setf (jcode-session-busy session) nil)
                          (jcode--session-set-display session :session-id id :status "Active")
                          (jcode-render-info
                           (jcode-session-chat-buffer session)
                           (format "%s session %s" (if resume-only "Resumed" "Loaded") id))
                          (when callback (funcall callback session)))))

(defun jcode-session-prompt (session text)
  "Send TEXT as prompt for SESSION."
  (setf (jcode-session-busy session) t)
  (jcode--session-set-display session :status "Working")
  (jcode--request session "session/prompt"
                        (jcode--acp-prompt-params session text)
                        (lambda (_result)
                          (setf (jcode-session-busy session) nil)
                          (jcode--session-set-display session :status "Idle"))))

(defun jcode-session-cancel (session)
  "Cancel active prompt in SESSION."
  (jcode--request session "session/cancel"
                        `(:sessionId ,(jcode-session-id session))
                        (lambda (_result)
                          (jcode-render-info (jcode-session-chat-buffer session) "Cancel requested"))))

(defun jcode-session-close (session)
  "Close/detach SESSION."
  (when (jcode-session-connection session)
    (ignore-errors
      (jcode--request session "session/close"
                            `(:sessionId ,(jcode-session-id session))
                            (lambda (_result) nil))))
  (when-let ((proc (jcode-session-process session)))
    (when (process-live-p proc) (delete-process proc)))
  (setq jcode--sessions (delq session jcode--sessions))
  (jcode-render-info (jcode-session-chat-buffer session) "Disconnected"))

(defun jcode-session-teardown (session)
  "Silently detach SESSION and remove it from live session tracking."
  (when session
    (when-let ((proc (jcode-session-process session)))
      (when (process-live-p proc)
        (delete-process proc)))
    (setq jcode--sessions (delq session jcode--sessions))))

(defun jcode-session-start (cwd chat input &optional session-id resume-only callback)
  "Start ACP client for CWD and buffers CHAT/INPUT.
When SESSION-ID is non-nil, load or resume it depending on RESUME-ONLY.
Call CALLBACK with the started session after session/new, session/load, or
session/resume completes."
  (let ((session (jcode--make-session :cwd cwd :chat-buffer chat :input-buffer input)))
    (setf (jcode-session-busy session) t)
    (with-current-buffer chat (setq jcode--session session))
    (with-current-buffer input (setq jcode--session session))
    (push session jcode--sessions)
    (jcode--start-acp session)
    (jcode-session-initialize
     session
     (lambda (s)
       (if session-id
           (jcode-session-load s session-id resume-only callback)
         (jcode-session-new s callback))))
    session))

(provide 'jcode-acp)
;;; jcode-acp.el ends here
