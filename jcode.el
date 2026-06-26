;;; jcode.el --- Emacs frontend for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1") (transient "0.9.0") (md-ts-mode "0.3.0"))
;; Version: 0.1.0
;; Keywords: ai llm tools

;;; Commentary:

;; Emacs frontend for jcode using `jcode acp'.
;;
;; Commands:
;;   M-x jcode          Open/create a jcode session in the current project.
;;   M-x jcode-resume   Attach to an existing jcode session by id.
;;   M-x jcode-current  Resume latest session for current project.
;;   M-x jcode-list     List known sessions.
;;   M-x jcode-plan     Open the implementation plan.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'jcode-ui)
(require 'jcode-session)
(require 'jcode-native)
(require 'jcode-acp)
(require 'jcode-input)
(require 'jcode-menu)

(declare-function jcode-apply-session-info-to-buffers "jcode-session"
                  (session-id chat input))
(declare-function jcode-session-teardown "jcode-acp" (session))
(declare-function jcode-native-open-session "jcode-native"
                  (session-id cwd chat input))
(declare-function jcode-native-close "jcode-native" (connection))

(defun jcode--get-chat-buffer ()
  "Return the chat buffer for the current jcode session.
Works from either a jcode chat or input buffer, mirroring pi-coding-agent."
  (cond
   ((derived-mode-p 'jcode-chat-mode) (current-buffer))
   ((and (boundp 'jcode--chat-buffer) (buffer-live-p jcode--chat-buffer))
    jcode--chat-buffer)))

(defun jcode--get-input-buffer ()
  "Return the input buffer for the current jcode session.
Works from either a jcode chat or input buffer, mirroring pi-coding-agent."
  (cond
   ((derived-mode-p 'jcode-input-mode) (current-buffer))
   ((and (boundp 'jcode--input-buffer) (buffer-live-p jcode--input-buffer))
    jcode--input-buffer)))

(defun jcode--pair-from-project-buffers ()
  "Return a live jcode buffer pair for the current project, if one exists."
  (catch 'pair
    (dolist (chat (jcode-project-buffers))
      (when-let ((input (buffer-local-value 'jcode--input-buffer chat)))
        (when (buffer-live-p input)
          (throw 'pair (cons chat input)))))))

(defun jcode--input-buffer-for-chat (chat)
  "Return an input buffer linked back to CHAT, if any."
  (catch 'input
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'jcode-input-mode)
                        (eq jcode--chat-buffer chat))))
        (throw 'input buffer)))))

(defun jcode--current-buffer-pair ()
  "Return current jcode buffer pair as (CHAT . INPUT), if any."
  (when (derived-mode-p 'jcode-chat-mode 'jcode-input-mode)
    (let ((chat (jcode--get-chat-buffer))
          (input (jcode--get-input-buffer)))
      (cond
       ((and (buffer-live-p chat) (buffer-live-p input))
        (cons chat input))
       ((derived-mode-p 'jcode-chat-mode)
        ;; Older/reloaded buffers may have lost the chat->input link.  Recover
        ;; from the input backlink first, then the project pair, instead of
        ;; creating or jumping to another session.
        (if-let ((linked-input (jcode--input-buffer-for-chat (current-buffer))))
            (cons (current-buffer) linked-input)
          (jcode--pair-from-project-buffers)))))))

(defun jcode--buffer-pair-for-session-id (session-id)
  "Return an existing live jcode buffer pair for SESSION-ID, if any."
  (catch 'pair
    (dolist (chat (buffer-list))
      (when (and (buffer-live-p chat)
                 (with-current-buffer chat
                   (and (derived-mode-p 'jcode-chat-mode)
                        (equal jcode--display-session-id session-id))))
        (when-let ((input (or (buffer-local-value 'jcode--input-buffer chat)
                              (jcode--input-buffer-for-chat chat))))
          (when (buffer-live-p input)
            (throw 'pair (cons chat input))))))))

(defun jcode-project-buffers (&optional directory)
  "Return live jcode chat buffers for DIRECTORY's project.
Buffers are ordered by `buffer-list' recency, most recent first."
  (let ((target (jcode--normalize-directory
                 (or directory (jcode--project-directory)))))
    (cl-remove-if-not
     (lambda (buffer)
       (and (buffer-live-p buffer)
            (with-current-buffer buffer
              (and (derived-mode-p 'jcode-chat-mode)
                   (string= (jcode--normalize-directory default-directory)
                            target)))))
     (buffer-list))))

(defun jcode--show-session-buffers (chat input)
  "Show CHAT and INPUT, focusing input when visible."
  (unless (and (buffer-live-p chat) (buffer-live-p input))
    (user-error "No complete jcode session buffers"))
  (jcode--display-buffers chat input)
  chat)

(defun jcode--current-session-id ()
  "Return the current jcode session id, if known."
  (cond
   ((derived-mode-p 'jcode-chat-mode 'jcode-input-mode)
    jcode--display-session-id)
   ((when-let ((pair (jcode--current-buffer-pair)))
      (with-current-buffer (car pair)
        jcode--display-session-id)))))

(defun jcode--read-connect-session-id ()
  "Read a session id for native connect, defaulting to the current session."
  (let ((default (jcode--current-session-id)))
    (if default
        (let ((value (read-string (format "Connect jcode session (default %s): " default)
                                  nil nil default)))
          (if (string-empty-p value) default value))
      (jcode-read-session-id (jcode--project-directory) "Connect jcode session: "))))

(defun jcode--native-connect (session-id &optional force)
  "Connect buffers to SESSION-ID through the native socket.
When FORCE is non-nil, close any existing Emacs connection first and request
session takeover so this client receives live streaming updates."
  (let* ((dir (jcode--project-directory))
         (current-pair (jcode--current-buffer-pair))
         (buffers (or (and current-pair
                           (let ((current-id (with-current-buffer (car current-pair)
                                               jcode--display-session-id)))
                             (or (not session-id)
                                 (not current-id)
                                 (string= session-id current-id)))
                           current-pair)
                      (jcode--make-buffers dir session-id)))
         (chat (car buffers))
         (input (cdr buffers)))
    (unless session-id
      (setq session-id (with-current-buffer chat jcode--display-session-id)))
    (unless (and session-id (not (string-empty-p session-id)))
      (user-error "No jcode session id to connect"))
    (jcode-apply-session-info-to-buffers session-id chat input)
    (jcode--display-buffers chat input)
    (when force
      (when-let ((native (buffer-local-value 'jcode--native-connection chat)))
        (jcode-native-close native)
        (with-current-buffer chat (setq jcode--native-connection nil))
        (with-current-buffer input (setq jcode--native-connection nil)))
      (when-let ((session (buffer-local-value 'jcode--session chat)))
        (jcode-session-teardown session)
        (with-current-buffer chat (setq jcode--session nil))
        (with-current-buffer input (setq jcode--session nil))))
    (unless (buffer-local-value 'jcode--native-connection chat)
      (let ((jcode-native-take-over-active-session t))
        (jcode-native-open-session session-id dir chat input)))
    chat))

;;;###autoload
(defun jcode (&optional session-id)
  "Open jcode buffers for the current project.
Without SESSION-ID, defer creating a persisted jcode session until the first
prompt is sent.  With prefix argument, prompt for SESSION-ID and load that
session immediately."
  (interactive
   (list (when current-prefix-arg
           (jcode-read-session-id (jcode--project-directory) "Load jcode session: "))))
  (if-let ((pair (and (not session-id)
                     (jcode--current-buffer-pair))))
      (jcode--show-session-buffers (car pair) (cdr pair))
    (let* ((dir (jcode--project-directory))
         (buffers (jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
      (when session-id
        (jcode-apply-session-info-to-buffers session-id chat input))
      (jcode--display-buffers chat input)
      (when (and session-id (not (buffer-local-value 'jcode--session chat)))
        (jcode-session-start dir chat input session-id nil))
      chat)))

;;;###autoload
(defun jcode-resume (session-id &optional full-load)
  "Resume jcode SESSION-ID without replay.
With prefix argument FULL-LOAD, call ACP `session/load' for history replay."
  (interactive (list (jcode-read-session-id (jcode--project-directory)) current-prefix-arg))
  (if-let ((existing (jcode--buffer-pair-for-session-id session-id)))
      (jcode--show-session-buffers (car existing) (cdr existing))
    (let* ((dir (jcode--project-directory))
           (buffers (jcode--make-buffers dir session-id))
           (chat (car buffers))
           (input (cdr buffers)))
      (jcode-apply-session-info-to-buffers session-id chat input)
      (jcode--display-buffers chat input)
      (when (and full-load (buffer-local-value 'jcode--session chat))
        (jcode-session-teardown (buffer-local-value 'jcode--session chat))
        (with-current-buffer chat (setq jcode--session nil))
        (with-current-buffer input (setq jcode--session nil))
        (jcode--clear-chat-buffer chat))
      (unless (buffer-local-value 'jcode--native-connection chat)
        (jcode-native-open-session session-id dir chat input))
      chat)))

;;;###autoload
(defun jcode-connect (session-id)
  "Connect to SESSION-ID using native takeover for live streaming updates.
This asks the jcode daemon to transfer the live session to this Emacs client,
which disconnects or supersedes other active UI clients for that session."
  (interactive (list (jcode--read-connect-session-id)))
  (jcode--native-connect session-id t))

;;;###autoload
(defun jcode-reconnect ()
  "Reconnect the current jcode session with native takeover.
Use this from a jcode chat/input buffer after another client, such as ghostty,
owns the live stream."
  (interactive)
  (unless (derived-mode-p 'jcode-chat-mode 'jcode-input-mode)
    (user-error "Run `jcode-reconnect' from a jcode chat or input buffer"))
  (jcode--native-connect (jcode--current-session-id) t))

;;;###autoload
(defalias 'jcode-attach #'jcode-connect)

;;;###autoload
(defun jcode-current (&optional any-directory)
  "Resume the latest jcode session for the current project.
With prefix argument ANY-DIRECTORY, resume the globally latest known session."
  (interactive "P")
  (let* ((dir (jcode--project-directory))
         (info (jcode-latest-session dir (not any-directory))))
    (unless info
      (user-error "No jcode session found%s"
                  (if any-directory "" " for current project")))
    (jcode-resume (jcode-session-info-id info) t)))

;;;###autoload
(defun jcode-list ()
  "List known jcode sessions.
RET loads with replay, `r' resumes without replay, `R' renames at point."
  (interactive)
  (let ((buffer (get-buffer-create "*jcode-sessions*"))
        (dir (jcode--project-directory)))
    (with-current-buffer buffer
      (jcode-list-mode)
      (setq jcode--session-list-directory dir)
      (jcode-list-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun jcode-plan ()
  "Open the jcode implementation plan."
  (interactive)
  (find-file (expand-file-name "PLAN.org" (file-name-directory (or load-file-name buffer-file-name)))))

(defun jcode--undefine-old-emacs-prefixed-functions ()
  "Remove stale `emacs-jcode*' functions left by older loaded versions."
  (mapatoms
   (lambda (symbol)
     (when (and (fboundp symbol)
                (string-prefix-p "emacs-jcode" (symbol-name symbol)))
       (fmakunbound symbol)))))

(jcode--undefine-old-emacs-prefixed-functions)

(provide 'jcode)
;;; jcode.el ends here
