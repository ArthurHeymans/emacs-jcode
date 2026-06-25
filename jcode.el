;;; jcode.el --- Emacs frontend for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
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

(declare-function jcode-apply-session-info-to-buffers "jcode-session"
                  (session-id chat input))
(declare-function jcode-session-teardown "jcode-acp" (session))
(declare-function jcode-native-open-session "jcode-native"
                  (session-id cwd chat input))

(defun jcode--current-buffer-pair ()
  "Return current jcode buffer pair as (CHAT . INPUT), if any."
  (cond
   ((derived-mode-p 'jcode-chat-mode)
    (and (buffer-live-p jcode--input-buffer)
         (cons (current-buffer) jcode--input-buffer)))
   ((derived-mode-p 'jcode-input-mode)
    (and (buffer-live-p jcode--chat-buffer)
         (cons jcode--chat-buffer (current-buffer))))))

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

;;;###autoload
(defun jcode (&optional session-id)
  "Open or create a jcode session for the current project.
With prefix argument, prompt for SESSION-ID and load that session."
  (interactive
   (list (when current-prefix-arg
           (jcode-read-session-id (jcode--project-directory) "Load jcode session: "))))
  (if-let ((pair (and (not session-id)
                     (or (jcode--current-buffer-pair)
                         (when-let ((chat (car (jcode-project-buffers))))
                           (cons chat (buffer-local-value 'jcode--input-buffer chat)))))))
      (jcode--show-session-buffers (car pair) (cdr pair))
    (let* ((dir (jcode--project-directory))
         (buffers (jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
      (when session-id
        (jcode-apply-session-info-to-buffers session-id chat input))
      (jcode--display-buffers chat input)
      (unless (buffer-local-value 'jcode--session chat)
        (jcode-session-start dir chat input session-id nil))
      chat)))

;;;###autoload
(defun jcode-resume (session-id &optional full-load)
  "Resume jcode SESSION-ID without replay.
With prefix argument FULL-LOAD, call ACP `session/load' for history replay."
  (interactive (list (jcode-read-session-id (jcode--project-directory)) current-prefix-arg))
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
    chat))

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
  "List known jcode sessions.  RET loads with replay, `r' resumes without replay."
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
