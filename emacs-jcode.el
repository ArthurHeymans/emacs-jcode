;;; emacs-jcode.el --- Emacs frontend for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0
;; Keywords: ai llm tools

;;; Commentary:

;; Emacs frontend for jcode using `jcode acp'.
;;
;; Commands:
;;   M-x emacs-jcode          Open/create a jcode session in the current project.
;;   M-x emacs-jcode-resume   Attach to an existing jcode session by id.
;;   M-x emacs-jcode-current  Resume latest session for current project.
;;   M-x emacs-jcode-list     List known sessions.
;;   M-x emacs-jcode-plan     Open the implementation plan.

;;; Code:

(require 'emacs-jcode-ui)
(require 'emacs-jcode-session)
(require 'emacs-jcode-acp)
(require 'emacs-jcode-input)

(declare-function emacs-jcode-apply-session-info-to-buffers "emacs-jcode-session"
                  (session-id chat input))

;;;###autoload
(defun emacs-jcode (&optional session-id)
  "Open or create a jcode session for the current project.
With prefix argument, prompt for SESSION-ID and load that session."
  (interactive
   (list (when current-prefix-arg
           (emacs-jcode-read-session-id (emacs-jcode--project-directory) "Load jcode session: "))))
  (let* ((dir (emacs-jcode--project-directory))
         (buffers (emacs-jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
    (when session-id
      (emacs-jcode-apply-session-info-to-buffers session-id chat input))
    (emacs-jcode--display-buffers chat input)
    (unless (buffer-local-value 'emacs-jcode--session chat)
      (emacs-jcode-session-start dir chat input session-id nil))
    chat))

;;;###autoload
(defun emacs-jcode-resume (session-id &optional full-load)
  "Resume jcode SESSION-ID without replay.
With prefix argument FULL-LOAD, call ACP `session/load' for history replay."
  (interactive (list (emacs-jcode-read-session-id (emacs-jcode--project-directory)) current-prefix-arg))
  (let* ((dir (emacs-jcode--project-directory))
         (buffers (emacs-jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
    (emacs-jcode-apply-session-info-to-buffers session-id chat input)
    (emacs-jcode--display-buffers chat input)
    (unless (buffer-local-value 'emacs-jcode--session chat)
      (emacs-jcode-session-start dir chat input session-id (not full-load)))
    chat))

;;;###autoload
(defun emacs-jcode-current (&optional any-directory)
  "Resume the latest jcode session for the current project.
With prefix argument ANY-DIRECTORY, resume the globally latest known session."
  (interactive "P")
  (let* ((dir (emacs-jcode--project-directory))
         (info (emacs-jcode-latest-session dir (not any-directory))))
    (unless info
      (user-error "No jcode session found%s"
                  (if any-directory "" " for current project")))
    (emacs-jcode-resume (emacs-jcode-session-info-id info) t)))

;;;###autoload
(defun emacs-jcode-list ()
  "List known jcode sessions.  RET loads with replay, `r' resumes without replay."
  (interactive)
  (let ((buffer (get-buffer-create "*jcode-sessions*"))
        (dir (emacs-jcode--project-directory)))
    (with-current-buffer buffer
      (emacs-jcode-list-mode)
      (setq emacs-jcode--session-list-directory dir)
      (emacs-jcode-list-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-jcode-plan ()
  "Open the emacs-jcode implementation plan."
  (interactive)
  (find-file (expand-file-name "PLAN.org" (file-name-directory (or load-file-name buffer-file-name)))))

(provide 'emacs-jcode)
;;; emacs-jcode.el ends here
