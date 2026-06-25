;;; emacs-jcode-session.el --- Session discovery UI for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Session listing/resume helpers backed by jcode's persisted session JSON files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tabulated-list)
(require 'subr-x)
(require 'emacs-jcode-ui)

(declare-function emacs-jcode-resume "emacs-jcode" (session-id &optional full-load))

(defcustom emacs-jcode-sessions-directory nil
  "Directory containing jcode session JSON files.
When nil, use ~/.jcode/sessions on the local or TRAMP host of
`default-directory'."
  :type '(choice (const :tag "Default ~/.jcode/sessions" nil) directory)
  :group 'emacs-jcode)

(cl-defstruct (emacs-jcode-session-info (:constructor emacs-jcode--make-session-info))
  id title short-name working-dir status model provider updated-at created-at file
  last-pid server-name)

(defvar-local emacs-jcode--session-list-directory nil)

(defun emacs-jcode--remote-prefix (&optional directory)
  "Return TRAMP prefix for DIRECTORY, or nil for local."
  (file-remote-p (or directory default-directory)))

(defun emacs-jcode--sessions-directory (&optional directory)
  "Return jcode sessions directory for DIRECTORY's host."
  (file-name-as-directory
   (or emacs-jcode-sessions-directory
       (concat (or (emacs-jcode--remote-prefix directory) "") "~/.jcode/sessions"))))

(defun emacs-jcode--servers-file (&optional directory)
  "Return jcode servers registry path for DIRECTORY's host."
  (concat (or (emacs-jcode--remote-prefix directory) "") "~/.jcode/servers.json"))

(defun emacs-jcode--server-name-by-pid (&optional directory)
  "Return alist mapping live jcode server pid to server name."
  (let ((file (emacs-jcode--servers-file directory)))
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

(defun emacs-jcode--safe-alist-get (key alist)
  "Return KEY from ALIST, accepting string or symbol keys."
  (or (alist-get key alist)
      (alist-get (if (symbolp key) (symbol-name key) (intern key)) alist nil nil #'equal)))

(defun emacs-jcode--read-session-info (file)
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
               (id (or (emacs-jcode--safe-alist-get 'id data)
                       (file-name-base file))))
          (when id
            (emacs-jcode--make-session-info
             :id id
             :title (emacs-jcode--safe-alist-get 'title data)
             :short-name (emacs-jcode--safe-alist-get 'short_name data)
             :working-dir (emacs-jcode--safe-alist-get 'working_dir data)
             :status (emacs-jcode--safe-alist-get 'status data)
             :model (emacs-jcode--safe-alist-get 'model data)
             :provider (emacs-jcode--safe-alist-get 'provider_key data)
             :updated-at (emacs-jcode--safe-alist-get 'updated_at data)
             :created-at (emacs-jcode--safe-alist-get 'created_at data)
             :file file
             :last-pid (emacs-jcode--safe-alist-get 'last_pid data)))))
    (error nil)))

(defun emacs-jcode--annotate-session-server-names (sessions directory)
  "Annotate SESSIONS with server names from DIRECTORY's server registry."
  (let ((servers (emacs-jcode--server-name-by-pid directory)))
    (dolist (info sessions)
      (when-let* ((pid (emacs-jcode-session-info-last-pid info))
                  (server-name (cdr (assq pid servers))))
        (setf (emacs-jcode-session-info-server-name info) server-name)))
    sessions))

(defun emacs-jcode-list-sessions-data (&optional directory)
  "Return discovered jcode session metadata for DIRECTORY's host."
  (let ((dir (emacs-jcode--sessions-directory directory)))
    (when (file-directory-p dir)
      (sort (emacs-jcode--annotate-session-server-names
             (delq nil (mapcar #'emacs-jcode--read-session-info
                               (directory-files dir t "\\.json\\'" t)))
             directory)
            (lambda (a b)
              (string> (or (emacs-jcode-session-info-updated-at a) "")
                       (or (emacs-jcode-session-info-updated-at b) "")))))))

(defun emacs-jcode--session-display-title (info)
  "Return display title for session INFO."
  (or (emacs-jcode-session-info-title info)
      (when (and (string= (or (emacs-jcode-session-info-status info) "") "Active")
                 (emacs-jcode-session-info-server-name info)
                 (emacs-jcode-session-info-short-name info))
        (format "%s %s"
                (emacs-jcode-session-info-server-name info)
                (emacs-jcode-session-info-short-name info)))
      (emacs-jcode-session-info-short-name info)
      (emacs-jcode-session-info-id info)))

(defun emacs-jcode--session-candidate (info)
  "Return completion candidate for session INFO."
  (let ((id (emacs-jcode-session-info-id info)))
    (cons (format "%s  %s  %s  %s"
                  (emacs-jcode--session-display-title info)
                  (or (emacs-jcode-session-info-status info) "")
                  (or (emacs-jcode-session-info-updated-at info) "")
                  (or (emacs-jcode-session-info-working-dir info) ""))
          id)))

(defun emacs-jcode-read-session-id (&optional directory prompt)
  "Read a jcode session id for DIRECTORY with PROMPT."
  (let* ((sessions (emacs-jcode-list-sessions-data directory))
         (candidates (mapcar #'emacs-jcode--session-candidate sessions)))
    (if candidates
        (cdr (assoc (completing-read (or prompt "Jcode session: ") candidates nil t) candidates))
      (read-string "Jcode session id: "))))

(defun emacs-jcode-latest-session (&optional directory only-current-directory)
  "Return latest jcode session for DIRECTORY.
When ONLY-CURRENT-DIRECTORY is non-nil, require matching `working_dir'."
  (let* ((dir (file-name-as-directory (or directory (emacs-jcode--project-directory))))
         (sessions (emacs-jcode-list-sessions-data dir)))
    (cl-find-if (lambda (info)
                  (or (not only-current-directory)
                      (let ((wd (emacs-jcode-session-info-working-dir info)))
                        (and wd (string= (file-name-as-directory wd) dir)))))
                sessions)))

(defun emacs-jcode--session-list-entry (info)
  "Return a `tabulated-list-entries' row for INFO."
  (let ((id (emacs-jcode-session-info-id info)))
    (list id
          (vector (emacs-jcode--session-display-title info)
                  (or (emacs-jcode-session-info-status info) "")
                  (or (emacs-jcode-session-info-model info) "")
                  (or (emacs-jcode-session-info-updated-at info) "")
                  (or (emacs-jcode-session-info-working-dir info) "")
                  id))))

(defun emacs-jcode--apply-session-info (info chat input)
  "Apply session INFO metadata to CHAT and INPUT buffers."
  (let ((title (emacs-jcode--session-display-title info)))
    (dolist (buffer (list chat input))
      (emacs-jcode--set-display-metadata
       buffer
       :session-id (emacs-jcode-session-info-id info)
       :title title
       :status (emacs-jcode-session-info-status info)
       :model (emacs-jcode-session-info-model info)))))

(defun emacs-jcode-list-refresh ()
  "Refresh the jcode session list buffer."
  (setq tabulated-list-entries
        (mapcar #'emacs-jcode--session-list-entry
                (emacs-jcode-list-sessions-data emacs-jcode--session-list-directory)))
  (tabulated-list-print t))

(defun emacs-jcode-apply-session-info-to-buffers (session-id chat input)
  "Apply discovered SESSION-ID metadata to CHAT and INPUT buffers."
  (when-let ((info (cl-find session-id (emacs-jcode-list-sessions-data default-directory)
                            :key #'emacs-jcode-session-info-id
                            :test #'string=)))
    (emacs-jcode--apply-session-info info chat input)))

(defun emacs-jcode-list-open (&optional resume-only)
  "Open session at point with history replay.
With prefix argument RESUME-ONLY, attach without replay."
  (interactive "P")
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session at point"))
    (emacs-jcode-resume id (not resume-only))))

(defvar emacs-jcode-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'emacs-jcode-list-refresh)
    (define-key map (kbd "RET") #'emacs-jcode-list-open)
    (define-key map (kbd "r") (lambda () (interactive) (emacs-jcode-list-open t)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `emacs-jcode-list-mode'.")

(define-derived-mode emacs-jcode-list-mode tabulated-list-mode "Jcode-Sessions"
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

(provide 'emacs-jcode-session)
;;; emacs-jcode-session.el ends here
