;;; jcode-native.el --- Native socket backend for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-render)
(require 'jcode-session)

(cl-defstruct (jcode-native-connection (:constructor jcode--make-native-connection))
  process chat input session-id cwd next-id line-buffer poll-timer last-history-size)

(defvar-local jcode--native-connection nil)

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
                "/run/user/" (number-to-string (user-uid)) "/jcode.sock"))))

(defun jcode-native--json-read (line)
  "Read native protocol JSON LINE."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string line)))

(defun jcode-native--send (connection object)
  "Send native protocol OBJECT through CONNECTION."
  (process-send-string
   (jcode-native-connection-process connection)
   (concat (json-serialize object :false-object :false :null-object nil) "\n")))

(defun jcode-native--request (connection type &rest fields)
  "Send request TYPE with FIELDS through CONNECTION."
  (let ((id (1+ (jcode-native-connection-next-id connection))))
    (setf (jcode-native-connection-next-id connection) id)
    (jcode-native--send connection (append `(:type ,type :id ,id) fields))
    id))

(defun jcode-native-message (connection content)
  "Send CONTENT as a native message through CONNECTION."
  (jcode-native--request connection "message" :content content))

(defun jcode-native-cancel (connection)
  "Cancel current native generation for CONNECTION."
  (jcode-native--request connection "cancel"))

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
      (condition-case nil
          (jcode-native--request connection "get_history")
        (error nil))
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
         (messages (append (alist-get 'messages event) nil))
         (history-size (length (prin1-to-string messages))))
    (unless (equal history-size (jcode-native-connection-last-history-size connection))
      (setf (jcode-native-connection-last-history-size connection) history-size)
      (jcode--clear-chat-buffer chat)
      (jcode--session-set-display-native connection event)
      (mapc (lambda (message) (jcode-native--render-history-message chat message))
            messages))))

(defun jcode-native--sentinel (proc _event)
  "Clean native connection state when PROC exits."
  (when-let ((connection (process-get proc 'jcode-native-connection)))
    (jcode-native-close connection)))

(defun jcode--session-set-display-native (connection event)
  "Apply native EVENT metadata to CONNECTION buffers."
  (let ((model (alist-get 'provider_model event))
        (server (alist-get 'server_name event)))
    (dolist (buffer (list (jcode-native-connection-chat connection)
                          (jcode-native-connection-input connection)))
      (jcode--set-display-metadata
       buffer
       :session-id (jcode-native-connection-session-id connection)
       :title (or server (jcode-native-connection-session-id connection))
       :status "Live"
       :model model))))

(defun jcode-native--handle-event (connection event)
  "Handle native protocol EVENT for CONNECTION."
  (let ((type (alist-get 'type event))
        (chat (jcode-native-connection-chat connection)))
    (pcase type
      ("history" (jcode-native--render-history connection event))
      ("text_delta" (jcode-render-assistant-message chat (alist-get 'text event)))
      ("text_replace" (jcode-render-assistant-message chat (alist-get 'text event)))
      ("tool_start" (jcode-render-tool chat event nil))
      ("tool_input" (jcode-render-tool chat event t))
      ("tool_done" (jcode-render-tool chat event t))
      ("session_renamed"
       (dolist (buffer (list (jcode-native-connection-chat connection)
                             (jcode-native-connection-input connection)))
         (jcode--set-display-metadata buffer :title (alist-get 'display_title event))))
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
         (proc (make-network-process :name "jcode-native"
                                     :buffer (generate-new-buffer " *jcode-native* ")
                                     :family 'local
                                     :service socket
                                     :coding 'utf-8-emacs-unix
                                     :noquery t))
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
    (setf (jcode-native-connection-poll-timer connection)
          (run-with-timer jcode-native-poll-interval
                          jcode-native-poll-interval
                          #'jcode-native--poll connection))
    connection))

(provide 'jcode-native)
;;; jcode-native.el ends here
