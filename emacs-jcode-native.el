;;; emacs-jcode-native.el --- Native socket backend for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-jcode-ui)
(require 'emacs-jcode-render)
(require 'emacs-jcode-session)

(cl-defstruct (emacs-jcode-native-connection (:constructor emacs-jcode--make-native-connection))
  process chat input session-id cwd next-id line-buffer poll-timer last-history-size)

(defvar-local emacs-jcode--native-connection nil)

(defcustom emacs-jcode-native-poll-interval 1.0
  "Seconds between native history refreshes for passive session views.

The daemon only pushes live deltas to the owning client for ordinary sessions.
Passive viewers therefore subscribe for the initial history and poll
`get_history' as a non-invasive fallback so current-session buffers keep
updating without taking over another UI client."
  :type 'number
  :group 'emacs-jcode)

(defun emacs-jcode-native-socket-path (&optional directory)
  "Return best native jcode socket path for DIRECTORY's host."
  (let ((file (emacs-jcode--servers-file directory)))
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

(defun emacs-jcode-native--json-read (line)
  "Read native protocol JSON LINE."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string line)))

(defun emacs-jcode-native--send (connection object)
  "Send native protocol OBJECT through CONNECTION."
  (process-send-string
   (emacs-jcode-native-connection-process connection)
   (concat (json-serialize object :false-object :false :null-object nil) "\n")))

(defun emacs-jcode-native--request (connection type &rest fields)
  "Send request TYPE with FIELDS through CONNECTION."
  (let ((id (1+ (emacs-jcode-native-connection-next-id connection))))
    (setf (emacs-jcode-native-connection-next-id connection) id)
    (emacs-jcode-native--send connection (append `(:type ,type :id ,id) fields))
    id))

(defun emacs-jcode-native-close (connection)
  "Close native CONNECTION and cancel its refresh timer."
  (when connection
    (when-let ((timer (emacs-jcode-native-connection-poll-timer connection)))
      (cancel-timer timer)
      (setf (emacs-jcode-native-connection-poll-timer connection) nil))
    (when-let ((proc (emacs-jcode-native-connection-process connection)))
      (when (process-live-p proc)
        (delete-process proc)))))

(defun emacs-jcode-native--kill-buffer-hook ()
  "Close native connection associated with the current buffer."
  (when (bound-and-true-p emacs-jcode--native-connection)
    (emacs-jcode-native-close emacs-jcode--native-connection)))

(defun emacs-jcode-native--poll (connection)
  "Refresh passive native CONNECTION from daemon history."
  (if (and (emacs-jcode-native-connection-process connection)
           (process-live-p (emacs-jcode-native-connection-process connection)))
      (condition-case nil
          (emacs-jcode-native--request connection "get_history")
        (error nil))
    (emacs-jcode-native-close connection)))

(defun emacs-jcode-native--render-history-message (chat message)
  "Render native history MESSAGE into CHAT."
  (let ((role (alist-get 'role message))
        (content (or (alist-get 'content message) "")))
    (pcase role
      ("user" (emacs-jcode-render-user chat content))
      ("assistant" (emacs-jcode-render-assistant-message chat content))
      ("tool" (emacs-jcode-render-tool chat `((title . "tool") (status . "done") (content . ,content)) t))
      (_ (emacs-jcode-render-info chat (format "%s: %s" role content))))))

(defun emacs-jcode-native--render-history (connection event)
  "Render native history EVENT for CONNECTION."
  (let* ((chat (emacs-jcode-native-connection-chat connection))
         (messages (append (alist-get 'messages event) nil))
         (history-size (length (prin1-to-string messages))))
    (unless (equal history-size (emacs-jcode-native-connection-last-history-size connection))
      (setf (emacs-jcode-native-connection-last-history-size connection) history-size)
      (emacs-jcode--clear-chat-buffer chat)
      (emacs-jcode--session-set-display-native connection event)
      (mapc (lambda (message) (emacs-jcode-native--render-history-message chat message))
            messages))))

(defun emacs-jcode-native--sentinel (proc _event)
  "Clean native connection state when PROC exits."
  (when-let ((connection (process-get proc 'emacs-jcode-native-connection)))
    (emacs-jcode-native-close connection)))

(defun emacs-jcode--session-set-display-native (connection event)
  "Apply native EVENT metadata to CONNECTION buffers."
  (let ((model (alist-get 'provider_model event))
        (server (alist-get 'server_name event)))
    (dolist (buffer (list (emacs-jcode-native-connection-chat connection)
                          (emacs-jcode-native-connection-input connection)))
      (emacs-jcode--set-display-metadata
       buffer
       :session-id (emacs-jcode-native-connection-session-id connection)
       :title (or server (emacs-jcode-native-connection-session-id connection))
       :status "Live"
       :model model))))

(defun emacs-jcode-native--handle-event (connection event)
  "Handle native protocol EVENT for CONNECTION."
  (let ((type (alist-get 'type event))
        (chat (emacs-jcode-native-connection-chat connection)))
    (pcase type
      ("history" (emacs-jcode-native--render-history connection event))
      ("text_delta" (emacs-jcode-render-assistant-message chat (alist-get 'text event)))
      ("text_replace" (emacs-jcode-render-assistant-message chat (alist-get 'text event)))
      ("tool_start" (emacs-jcode-render-tool chat event nil))
      ("tool_input" (emacs-jcode-render-tool chat event t))
      ("tool_done" (emacs-jcode-render-tool chat event t))
      ("session_renamed"
       (dolist (buffer (list (emacs-jcode-native-connection-chat connection)
                             (emacs-jcode-native-connection-input connection)))
         (emacs-jcode--set-display-metadata buffer :title (alist-get 'display_title event))))
      ("error" (emacs-jcode-render-error chat (or (alist-get 'message event) (format "%S" event))))
      (_ nil))))

(defun emacs-jcode-native--filter (proc string)
  "Handle native protocol output STRING from PROC."
  (let* ((connection (process-get proc 'emacs-jcode-native-connection))
         (buffer (concat (or (emacs-jcode-native-connection-line-buffer connection) "") string))
         lines)
    (while (string-match "\n" buffer)
      (push (substring buffer 0 (match-beginning 0)) lines)
      (setq buffer (substring buffer (match-end 0))))
    (setf (emacs-jcode-native-connection-line-buffer connection) buffer)
    (dolist (line (nreverse lines))
      (unless (string-empty-p (string-trim line))
        (condition-case err
            (emacs-jcode-native--handle-event connection (emacs-jcode-native--json-read line))
          (error (emacs-jcode-render-error
                  (emacs-jcode-native-connection-chat connection)
                  (format "native parse error: %S" err))))))))

(defun emacs-jcode-native-open-session (session-id cwd chat input)
  "Open native live SESSION-ID for CWD into CHAT and INPUT."
  (let* ((socket (emacs-jcode-native-socket-path cwd))
         (proc (make-network-process :name "jcode-native"
                                     :buffer (generate-new-buffer " *jcode-native* ")
                                     :family 'local
                                     :service socket
                                     :coding 'utf-8-emacs-unix
                                     :noquery t))
         (connection (emacs-jcode--make-native-connection
                      :process proc :chat chat :input input :session-id session-id
                      :cwd cwd :next-id 0 :line-buffer "")))
    (set-process-filter proc #'emacs-jcode-native--filter)
    (set-process-sentinel proc #'emacs-jcode-native--sentinel)
    (process-put proc 'emacs-jcode-native-connection connection)
    (with-current-buffer chat
      (setq emacs-jcode--native-connection connection)
      (add-hook 'kill-buffer-hook #'emacs-jcode-native--kill-buffer-hook nil t))
    (with-current-buffer input
      (setq emacs-jcode--native-connection connection)
      (add-hook 'kill-buffer-hook #'emacs-jcode-native--kill-buffer-hook nil t))
    (emacs-jcode-native--request
     connection "subscribe"
     :working_dir cwd
     :target_session_id session-id
     :client_has_local_history :false
     :allow_session_takeover :false)
    (setf (emacs-jcode-native-connection-poll-timer connection)
          (run-with-timer emacs-jcode-native-poll-interval
                          emacs-jcode-native-poll-interval
                          #'emacs-jcode-native--poll connection))
    connection))

(provide 'emacs-jcode-native)
;;; emacs-jcode-native.el ends here
