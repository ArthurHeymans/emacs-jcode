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

(cl-defstruct (jcode-session-info (:constructor jcode--make-session-info))
  id title short-name working-dir status model provider updated-at created-at file
  last-pid server-name)

(defvar-local jcode--session-list-directory nil)

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
                       (file-name-base file))))
          (when id
            (jcode--make-session-info
             :id id
             :title (jcode--safe-alist-get 'title data)
             :short-name (jcode--safe-alist-get 'short_name data)
             :working-dir (jcode--safe-alist-get 'working_dir data)
             :status (jcode--safe-alist-get 'status data)
             :model (jcode--safe-alist-get 'model data)
             :provider (jcode--safe-alist-get 'provider_key data)
             :updated-at (jcode--safe-alist-get 'updated_at data)
             :created-at (jcode--safe-alist-get 'created_at data)
             :file file
             :last-pid (jcode--safe-alist-get 'last_pid data)))))
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

(defun jcode-list-sessions-data (&optional directory)
  "Return discovered jcode session metadata for DIRECTORY's host."
  (let ((dir (jcode--sessions-directory directory)))
    (when (file-directory-p dir)
      (sort (jcode--annotate-session-server-names
             (delq nil (mapcar #'jcode--read-session-info
                               (directory-files dir t "\\.json\\'" t)))
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
                  (or (jcode-session-info-model info) "")
                  (or (jcode-session-info-updated-at info) "")
                  (or (jcode-session-info-working-dir info) "")
                  id))))

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
       :model (jcode-session-info-model info)))))

(defun jcode-list-refresh ()
  "Refresh the jcode session list buffer."
  (interactive)
  (setq tabulated-list-entries
        (mapcar #'jcode--session-list-entry
                (jcode-list-sessions-data jcode--session-list-directory)))
  (tabulated-list-print t))

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
    (define-key map (kbd "g") #'jcode-list-refresh)
    (define-key map (kbd "RET") #'jcode-list-open)
    (define-key map (kbd "r") (lambda () (interactive) (jcode-list-open t)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `jcode-list-mode'.")

(define-derived-mode jcode-list-mode tabulated-list-mode "Jcode-Sessions"
  "Major mode for listing jcode sessions."
  (setq tabulated-list-format
        [ ("Title" 18 t)
          ("Status" 10 t)
          ("Model" 16 t)
          ("Updated" 30 t)
          ("Working directory" 36 t)
          ("ID" 48 t) ])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(provide 'jcode-session)
;;; jcode-session.el ends here
