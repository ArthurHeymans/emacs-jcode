;;; jcode-session.el --- Session discovery UI for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Session listing/resume helpers backed by jcode's persisted session JSON files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tabulated-list)
(require 'subr-x)
(require 'jcode-ui)

(declare-function jcode-resume "jcode" (session-id &optional full-load))

(defcustom jcode-sessions-directory nil
  "Directory containing jcode session JSON files.
When nil, use ~/.jcode/sessions on the local or TRAMP host of
`default-directory'."
  :type '(choice (const :tag "Default ~/.jcode/sessions" nil) directory)
  :group 'jcode)

(defcustom jcode-hide-empty-sessions t
  "Whether session lists hide sessions with no real conversation.
An empty session is one whose persisted messages are only system/context
messages, which are commonly created by opening jcode and doing nothing."
  :type 'boolean
  :group 'jcode)

(cl-defstruct (jcode-session-info (:constructor jcode--make-session-info))
  id title short-name working-dir status model provider updated-at created-at file
  last-pid server-name message-count conversation-count user-turn-count saved
  location client)

(defvar-local jcode--session-list-directory nil)

(defconst jcode--session-list-format
  [ ("Title" 28 t)
    ("Status" 10 t)
    ("Where" 14 t)
    ("Client" 16 t)
    ("Model" 16 t)
    ("Turns" 7 t)
    ("Updated" 12 t)
    ("Project" 24 t) ]
  "Column format for `jcode-list-mode'.")

(defun jcode--remote-prefix (&optional directory)
  "Return TRAMP prefix for DIRECTORY, or nil for local."
  (file-remote-p (or directory default-directory)))

(defun jcode--sessions-directory (&optional directory)
  "Return jcode sessions directory for DIRECTORY's host."
  (file-name-as-directory
   (or jcode-sessions-directory
       (concat (or (jcode--remote-prefix directory) "") "~/.jcode/sessions"))))

(defun jcode--servers-file (&optional directory)
  "Return jcode servers registry path for DIRECTORY's host."
  (concat (or (jcode--remote-prefix directory) "") "~/.jcode/servers.json"))

(defun jcode--server-name-by-pid (&optional directory)
  "Return alist mapping live jcode server pid to server name."
  (let ((file (jcode--servers-file directory)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((json-object-type 'alist)
                   (json-key-type 'symbol)
                   (data (json-read)))
              (delq nil
                    (mapcar (lambda (entry)
                              (let* ((server (cdr entry))
                                     (pid (alist-get 'pid server))
                                     (name (alist-get 'name server)))
                                (and pid name (cons pid name))))
                            data))))
        (error nil)))))

(defun jcode--safe-alist-get (key alist)
  "Return KEY from ALIST, accepting string or symbol keys."
  (or (alist-get key alist)
      (alist-get (if (symbolp key) (symbol-name key) (intern key)) alist nil nil #'equal)))

(defun jcode--json-true-p (value)
  "Return non-nil only when JSON VALUE represents true."
  (eq value t))

(defun jcode--message-display-role (message)
  "Return display role for persisted MESSAGE."
  (or (jcode--safe-alist-get 'display_role message)
      (jcode--safe-alist-get 'displayRole message)))

(defun jcode--conversation-message-p (message)
  "Return non-nil if persisted MESSAGE is real conversation content."
  (let ((role (jcode--safe-alist-get 'role message))
        (display-role (jcode--message-display-role message)))
    (not (or (equal display-role "system")
             (equal role "system")))))

(defun jcode--user-turn-message-p (message)
  "Return non-nil if persisted MESSAGE is a user-authored text prompt.
jcode stores tool results as `role=user' messages for provider protocol
compatibility; those are not user sends and must not affect the Turns column."
  (and (jcode--conversation-message-p message)
       (not (member (jcode--message-display-role message)
                    '("background_task" "system")))
       (equal (jcode--safe-alist-get 'role message) "user")
       (let ((content (jcode--safe-alist-get 'content message)))
         (or (and (stringp content) (not (string-empty-p (string-trim content))))
             (and (listp content)
                  (cl-some (lambda (part)
                             (and (listp part)
                                  (equal (jcode--safe-alist-get 'type part) "text")
                                  (let ((text (jcode--safe-alist-get 'text part)))
                                    (and (stringp text)
                                         (not (string-empty-p (string-trim text)))))))
                           content))))))

(defun jcode--session-empty-p (info)
  "Return non-nil if INFO has no real conversation messages.
Only classify sessions as empty when the persisted file explicitly included a
message list.  Older/minimal metadata files without messages are kept visible."
  (and (not (jcode-session-info-saved info))
       (numberp (jcode-session-info-message-count info))
       (= (or (jcode-session-info-conversation-count info) 0) 0)))

(defun jcode--session-hidden-as-empty-p (info)
  "Return non-nil when INFO should be hidden as empty in session lists.
Closed empty sessions are usually noise, but active empty sessions are visible
because they can correspond to currently open Emacs/TUI windows."
  (and (jcode--session-empty-p info)
       (not (equal (jcode-session-info-status info) "Active"))))

(defun jcode--read-session-info (file)
  "Read jcode session metadata from FILE."
  (condition-case nil
      (with-temp-buffer
        ;; Session files can be large because they include full transcript content.
        ;; Read the complete JSON so active/long sessions are not silently omitted.
        (insert-file-contents file)
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (data (json-read))
               (id (or (jcode--safe-alist-get 'id data)
                       (file-name-base file)))
               (messages-cell (assq 'messages data))
               (messages (cdr messages-cell))
               (message-count (and messages-cell (listp messages) (length messages)))
               (conversation-count (and messages-cell (listp messages)
                                        (length (cl-remove-if-not
                                                 #'jcode--conversation-message-p messages))))
               (user-turn-count (and messages-cell (listp messages)
                                     (length (cl-remove-if-not
                                              #'jcode--user-turn-message-p messages)))))
          (when id
            (jcode--make-session-info
             :id id
             :title (or (jcode--safe-alist-get 'custom_title data)
                        (jcode--safe-alist-get 'title data))
             :short-name (jcode--safe-alist-get 'short_name data)
             :working-dir (jcode--safe-alist-get 'working_dir data)
             :status (jcode--safe-alist-get 'status data)
             :model (jcode--safe-alist-get 'model data)
             :provider (jcode--safe-alist-get 'provider_key data)
             :updated-at (jcode--safe-alist-get 'updated_at data)
             :created-at (jcode--safe-alist-get 'created_at data)
             :file file
             :last-pid (jcode--safe-alist-get 'last_pid data)
             :message-count message-count
             :conversation-count conversation-count
             :user-turn-count user-turn-count
             :saved (jcode--json-true-p (jcode--safe-alist-get 'saved data))))))
    (error nil)))

(defun jcode--session-status-string (status)
  "Return a human-readable string for jcode session STATUS."
  (cond
   ((null status) "")
   ((equal status "Active") "live")
   ((equal status "Closed") "closed")
   ((stringp status) status)
   ((symbolp status) (symbol-name status))
   ;; Ex: ((Crashed (message . "Terminal or window closed (SIGHUP)")))
   ((and (consp status) (consp (car status)) (symbolp (caar status)))
    (let* ((kind (symbol-name (caar status)))
           (payload (cdar status))
           (message (and (listp payload)
                         (or (alist-get 'message payload)
                             (alist-get "message" payload nil nil #'equal)))))
      (if (and (stringp message) (not (string-empty-p message)))
          (format "%s: %s" kind message)
        kind)))
   ((consp status) (string-join (mapcar #'jcode--session-status-string status) " "))
   (t (format "%s" status))))

(defun jcode--annotate-session-server-names (sessions directory)
  "Annotate SESSIONS with server names from DIRECTORY's server registry."
  (let ((servers (jcode--server-name-by-pid directory)))
    (dolist (info sessions)
      (when-let* ((pid (jcode-session-info-last-pid info))
                  (server-name (cdr (assq pid servers))))
        (setf (jcode-session-info-server-name info) server-name)))
    sessions))

(defun jcode--live-session-user-turn-count (session-id)
  "Return rendered user-message count for live SESSION-ID buffers, or nil."
  (catch 'count
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp 'jcode--display-session-id)
                   (equal jcode--display-session-id session-id)
                   (derived-mode-p 'jcode-chat-mode))
          (save-excursion
            (save-restriction
              (widen)
              (throw 'count (how-many "^You\n=+\n" (point-min) (point-max))))))))))

(defun jcode--annotate-session-live-counts (sessions)
  "Annotate SESSIONS with counts from open chat buffers when available."
  (dolist (info sessions)
    (when-let* ((id (jcode-session-info-id info))
                (count (jcode--live-session-user-turn-count id)))
      (setf (jcode-session-info-user-turn-count info) count)))
  sessions)

(defun jcode--session-location-label (directory)
  "Return a compact local/TRAMP location label for DIRECTORY."
  (if-let ((remote (jcode--remote-prefix directory)))
      (format "remote %s" (string-remove-suffix ":" (string-remove-prefix "/" remote)))
    "local"))

(defun jcode--session-live-client-label (session-id)
  "Return connected client label for live SESSION-ID buffers, or nil."
  (catch 'label
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp 'jcode--display-session-id)
                   (equal jcode--display-session-id session-id)
                   (derived-mode-p 'jcode-chat-mode))
          (let* ((owner (pcase jcode--display-owner
                          ('owned "owned")
                          ('viewing "viewing")
                          ((and (pred stringp) s) s)
                          (_ nil)))
                 (count (and (numberp jcode--display-client-count)
                             (format "x%d" jcode--display-client-count)))
                 (transport (and (stringp jcode--display-connection-type)
                                 (not (string-empty-p jcode--display-connection-type))
                                 jcode--display-connection-type)))
            (throw 'label
                   (string-join (delq nil (list "Emacs" owner count transport)) " "))))))))

(defun jcode--annotate-session-runtime (sessions directory)
  "Annotate SESSIONS with runtime location and live client metadata."
  (let ((location (jcode--session-location-label directory)))
    (dolist (info sessions)
      (setf (jcode-session-info-location info) location)
      (when-let ((client (jcode--session-live-client-label (jcode-session-info-id info))))
        (setf (jcode-session-info-client info) client)))
    sessions))

(defun jcode-list-sessions-data (&optional directory)
  "Return discovered jcode session metadata for DIRECTORY's host."
  (let ((dir (jcode--sessions-directory directory)))
    (when (file-directory-p dir)
      (sort (jcode--annotate-session-runtime
             (jcode--annotate-session-live-counts
              (jcode--annotate-session-server-names
               (cl-remove-if (lambda (info)
                              (and jcode-hide-empty-sessions
                                   (jcode--session-hidden-as-empty-p info)))
                            (delq nil (mapcar #'jcode--read-session-info
                                              (directory-files dir t "\\.json\\'" t))))
               directory))
             directory)
            (lambda (a b)
              (string> (or (jcode-session-info-updated-at a) "")
                       (or (jcode-session-info-updated-at b) "")))))))

(defun jcode--session-display-title (info)
  "Return display title for session INFO."
  (or (jcode-session-info-title info)
      (when (and (equal (jcode-session-info-status info) "Active")
                 (jcode-session-info-server-name info)
                 (jcode-session-info-short-name info))
        (format "%s %s"
                (jcode-session-info-server-name info)
                (jcode-session-info-short-name info)))
      (jcode-session-info-short-name info)
      (jcode-session-info-id info)))

(defun jcode--session-candidate (info)
  "Return completion candidate for session INFO."
  (let ((id (jcode-session-info-id info)))
    (cons (format "%s  %s  %s  %s"
                  (jcode--session-display-title info)
                  (jcode--session-status-string
                   (jcode-session-info-status info))
                  (or (jcode-session-info-updated-at info) "")
                  (or (jcode-session-info-working-dir info) ""))
          id)))

(defun jcode--relative-time-string (timestamp &optional now)
  "Return a compact relative age string for ISO TIMESTAMP.
NOW defaults to `current-time'.  Invalid or missing timestamps return an empty
string."
  (if (not (and (stringp timestamp) (not (string-empty-p timestamp))))
      ""
    (condition-case nil
        (let* ((seconds (max 0 (float-time (time-subtract (or now (current-time))
                                                          (date-to-time timestamp)))))
               (minute 60)
               (hour (* 60 minute))
               (day (* 24 hour))
               (week (* 7 day)))
          (cond
           ((< seconds 45) "just now")
           ((< seconds hour) (format "%dm ago" (floor (/ seconds minute))))
           ((< seconds day) (format "%dh ago" (floor (/ seconds hour))))
           ((< seconds week) (format "%dd ago" (floor (/ seconds day))))
           (t (format "%dw ago" (floor (/ seconds week))))))
      (error ""))))

(defun jcode--session-project-label (info)
  "Return a compact project label for session INFO."
  (let ((dir (jcode-session-info-working-dir info)))
    (if (and (stringp dir) (not (string-empty-p dir)))
        (file-name-nondirectory (directory-file-name dir))
      "")))

(defun jcode--session-turn-count-string (info)
  "Return display turn count for session INFO."
  (if (numberp (jcode-session-info-user-turn-count info))
      (number-to-string (jcode-session-info-user-turn-count info))
    ""))

(defun jcode-read-session-id (&optional directory prompt)
  "Read a jcode session id for DIRECTORY with PROMPT."
  (let* ((sessions (jcode-list-sessions-data directory))
         (candidates (mapcar #'jcode--session-candidate sessions)))
    (if candidates
        (cdr (assoc (completing-read (or prompt "Jcode session: ") candidates nil t) candidates))
      (read-string "Jcode session id: "))))

(defun jcode-latest-session (&optional directory only-current-directory)
  "Return latest jcode session for DIRECTORY.
When ONLY-CURRENT-DIRECTORY is non-nil, require matching `working_dir'."
  (let* ((dir (file-name-as-directory (or directory (jcode--project-directory))))
         (sessions (jcode-list-sessions-data dir)))
    (cl-find-if (lambda (info)
                  (or (not only-current-directory)
                      (let ((wd (jcode-session-info-working-dir info)))
                        (and wd (string= (file-name-as-directory wd) dir)))))
                sessions)))

(defun jcode--session-list-entry (info)
  "Return a `tabulated-list-entries' row for INFO."
  (let ((id (jcode-session-info-id info)))
    (list id
          (vector (jcode--session-display-title info)
                  (jcode--session-status-string
                   (jcode-session-info-status info))
                  (or (jcode-session-info-location info) "")
                  (or (jcode-session-info-client info) "")
                  (or (jcode-session-info-model info) "")
                  (jcode--session-turn-count-string info)
                  (jcode--relative-time-string
                   (jcode-session-info-updated-at info))
                  (jcode--session-project-label info)))))

(defun jcode--apply-session-info (info chat input)
  "Apply session INFO metadata to CHAT and INPUT buffers."
  (let ((title (jcode--session-display-title info)))
    (dolist (buffer (list chat input))
      (jcode--set-display-metadata
       buffer
       :session-id (jcode-session-info-id info)
       :title title
       :status (jcode--session-status-string
                (jcode-session-info-status info))
       :model (jcode-session-info-model info)
       :provider (jcode-session-info-provider info)))))

(defun jcode-list-refresh ()
  "Refresh the jcode session list buffer."
  (interactive)
  (setq tabulated-list-format jcode--session-list-format)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar #'jcode--session-list-entry
                (jcode-list-sessions-data jcode--session-list-directory)))
  (tabulated-list-print t))

(defun jcode-refresh-session-list-buffers ()
  "Refresh all open `jcode-list-mode' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'jcode-list-mode)
        (jcode-list-refresh)))))

(defun jcode-list-mark ()
  "Mark the session at point and move to the next row."
  (interactive)
  (unless (tabulated-list-get-id)
    (user-error "No session at point"))
  (tabulated-list-put-tag "*" t))

(defun jcode-list-unmark ()
  "Unmark the session at point and move to the next row."
  (interactive)
  (unless (tabulated-list-get-id)
    (user-error "No session at point"))
  (tabulated-list-put-tag " " t))

(defun jcode-list-unmark-backward ()
  "Move to the previous row and unmark it."
  (interactive)
  (forward-line -1)
  (jcode-list-unmark))

(defun jcode-list-unmark-all ()
  "Unmark all marked sessions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (eq (char-after (line-beginning-position)) ?*)
        (tabulated-list-put-tag " " nil))
      (forward-line 1)))
  (message "Jcode: unmarked all sessions"))

(defun jcode-list-toggle-mark ()
  "Toggle the mark on the session at point and move to the next row."
  (interactive)
  (unless (tabulated-list-get-id)
    (user-error "No session at point"))
  (tabulated-list-put-tag
   (if (eq (char-after (line-beginning-position)) ?*) " " "*")
   t))

(defun jcode-list-marked-session-ids ()
  "Return marked session ids in the current `jcode-list-mode' buffer."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (eq (char-after (line-beginning-position)) ?*)
                   (tabulated-list-get-id))
          (push (tabulated-list-get-id) ids))
        (forward-line 1)))
    (nreverse ids)))

(defun jcode--session-info-by-id (session-id &optional directory)
  "Return session info for SESSION-ID in DIRECTORY."
  (cl-find session-id (jcode-list-sessions-data directory)
           :key #'jcode-session-info-id
           :test #'string=))

(defun jcode-rename-session-file (session-id title &optional directory)
  "Persist TITLE as custom title for SESSION-ID in DIRECTORY.
An empty or nil TITLE clears the custom title."
  (let* ((info (or (jcode--session-info-by-id session-id directory)
                   (user-error "Unknown jcode session: %s" session-id)))
         (file (or (jcode-session-info-file info)
                   (user-error "No session file for: %s" session-id)))
         (normalized (and title (string-trim title))))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (json-false :false)
             (data (json-read)))
        (setf (alist-get 'custom_title data)
              (unless (string-empty-p (or normalized "")) normalized))
        (erase-buffer)
        (insert (json-encode data))
        (insert "\n")
        (write-region (point-min) (point-max) file nil 'silent)))
    (jcode-refresh-session-list-buffers)
    normalized))

(defun jcode-list-rename-session (title)
  "Rename the session at point to TITLE by editing its persisted session file."
  (interactive (list (read-string "Session title (empty clears): ")))
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session at point"))
    (jcode-rename-session-file id title jcode--session-list-directory)
    (message "Jcode: renamed session %s" id)))

(defun jcode--delete-session-files (session-ids directory)
  "Delete persisted files for SESSION-IDS in DIRECTORY.
Return the number of files deleted."
  (let ((deleted 0))
    (dolist (session-id session-ids deleted)
      (when-let* ((info (jcode--session-info-by-id session-id directory))
                  (file (jcode-session-info-file info)))
        (delete-file file)
        (setq deleted (1+ deleted))))))

(defun jcode-list-delete-session ()
  "Delete the session file at point after confirmation."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session at point"))
    (when (or noninteractive
              (yes-or-no-p (format "Delete jcode session %s? " id)))
      (let ((deleted (jcode--delete-session-files (list id) jcode--session-list-directory)))
        (jcode-list-refresh)
        (message "Jcode: deleted %d session%s"
                 deleted (if (= deleted 1) "" "s"))))))

(defun jcode-list-delete-marked-sessions ()
  "Delete all marked session files after confirmation."
  (interactive)
  (let ((ids (jcode-list-marked-session-ids)))
    (unless ids (user-error "No marked sessions"))
    (when (or noninteractive
              (yes-or-no-p (format "Delete %d marked jcode session%s? "
                                   (length ids)
                                   (if (= (length ids) 1) "" "s"))))
      (let ((deleted (jcode--delete-session-files ids jcode--session-list-directory)))
        (jcode-list-refresh)
        (message "Jcode: deleted %d session%s"
                 deleted (if (= deleted 1) "" "s"))))))

(defun jcode-list-open-marked-sessions (&optional resume-only)
  "Open all marked sessions.
With prefix argument RESUME-ONLY, attach without history replay."
  (interactive "P")
  (let ((ids (jcode-list-marked-session-ids)))
    (unless ids (user-error "No marked sessions"))
    (dolist (id ids)
      (jcode-resume id (not resume-only)))
    (message "Jcode: opened %d marked session%s"
             (length ids) (if (= (length ids) 1) "" "s"))))

(defun jcode-prune-empty-sessions (&optional directory)
  "Delete closed empty session files for DIRECTORY.
This never deletes saved sessions or sessions whose persisted status is Active."
  (interactive)
  (let* ((jcode-hide-empty-sessions nil)
         (sessions (jcode-list-sessions-data (or directory default-directory)))
         (empty-closed (cl-remove-if-not
                        (lambda (info)
                          (and (jcode--session-empty-p info)
                               (equal (jcode-session-info-status info) "Closed")
                               (jcode-session-info-file info)))
                        sessions)))
    (if (not empty-closed)
        (message "Jcode: no closed empty sessions to prune")
      (when (or noninteractive
                (yes-or-no-p (format "Delete %d closed empty jcode session file%s? "
                                     (length empty-closed)
                                     (if (= (length empty-closed) 1) "" "s"))))
        (dolist (info empty-closed)
          (delete-file (jcode-session-info-file info)))
        (when (derived-mode-p 'jcode-list-mode)
          (jcode-list-refresh))
        (message "Jcode: pruned %d empty session%s"
                 (length empty-closed)
                 (if (= (length empty-closed) 1) "" "s"))))))

(defun jcode-apply-session-info-to-buffers (session-id chat input)
  "Apply discovered SESSION-ID metadata to CHAT and INPUT buffers."
  (when-let ((info (cl-find session-id (jcode-list-sessions-data default-directory)
                            :key #'jcode-session-info-id
                            :test #'string=)))
    (jcode--apply-session-info info chat input)))

(defun jcode-list-open (&optional resume-only)
  "Open session at point with history replay.
With prefix argument RESUME-ONLY, attach without replay."
  (interactive "P")
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session at point"))
    (jcode-resume id (not resume-only))))

(defvar jcode-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'jcode-list-refresh)
    (define-key map (kbd "m") #'jcode-list-mark)
    (define-key map (kbd "*") #'jcode-list-mark)
    (define-key map (kbd "u") #'jcode-list-unmark)
    (define-key map (kbd "DEL") #'jcode-list-unmark-backward)
    (define-key map (kbd "<backspace>") #'jcode-list-unmark-backward)
    (define-key map (kbd "U") #'jcode-list-unmark-all)
    (define-key map (kbd "t") #'jcode-list-toggle-mark)
    (define-key map (kbd "d") #'jcode-list-delete-session)
    (define-key map (kbd "x") #'jcode-list-delete-marked-sessions)
    (define-key map (kbd "D") #'jcode-list-delete-marked-sessions)
    (define-key map (kbd "R") #'jcode-list-rename-session)
    (define-key map (kbd "O") #'jcode-list-open-marked-sessions)
    (define-key map (kbd "P") #'jcode-prune-empty-sessions)
    (define-key map (kbd "RET") #'jcode-list-open)
    (define-key map (kbd "r") (lambda () (interactive) (jcode-list-open t)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `jcode-list-mode'.")

;; Keep keymaps current when this package is reloaded during development.
(set-keymap-parent jcode-list-mode-map tabulated-list-mode-map)
(define-key jcode-list-mode-map (kbd "g") #'jcode-list-refresh)
(define-key jcode-list-mode-map (kbd "m") #'jcode-list-mark)
(define-key jcode-list-mode-map (kbd "*") #'jcode-list-mark)
(define-key jcode-list-mode-map (kbd "u") #'jcode-list-unmark)
(define-key jcode-list-mode-map (kbd "DEL") #'jcode-list-unmark-backward)
(define-key jcode-list-mode-map (kbd "<backspace>") #'jcode-list-unmark-backward)
(define-key jcode-list-mode-map (kbd "U") #'jcode-list-unmark-all)
(define-key jcode-list-mode-map (kbd "t") #'jcode-list-toggle-mark)
(define-key jcode-list-mode-map (kbd "d") #'jcode-list-delete-session)
(define-key jcode-list-mode-map (kbd "x") #'jcode-list-delete-marked-sessions)
(define-key jcode-list-mode-map (kbd "D") #'jcode-list-delete-marked-sessions)
(define-key jcode-list-mode-map (kbd "R") #'jcode-list-rename-session)
(define-key jcode-list-mode-map (kbd "O") #'jcode-list-open-marked-sessions)
(define-key jcode-list-mode-map (kbd "P") #'jcode-prune-empty-sessions)
(define-key jcode-list-mode-map (kbd "RET") #'jcode-list-open)
(define-key jcode-list-mode-map (kbd "r") (lambda () (interactive) (jcode-list-open t)))
(define-key jcode-list-mode-map (kbd "q") #'quit-window)

(define-derived-mode jcode-list-mode tabulated-list-mode "Jcode-Sessions"
  "Major mode for listing jcode sessions."
  (setq tabulated-list-format jcode--session-list-format)
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-padding 2)
  (setq-local header-line-format
              "RET open  r resume  R rename  m mark  x delete marked  g refresh  q quit")
  (tabulated-list-init-header))

(provide 'jcode-session)
;;; jcode-session.el ends here
