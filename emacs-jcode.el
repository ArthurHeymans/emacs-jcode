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
;;   M-x emacs-jcode-plan     Open the implementation plan.

;;; Code:

(require 'emacs-jcode-ui)
(require 'emacs-jcode-acp)
(require 'emacs-jcode-input)

;;;###autoload
(defun emacs-jcode (&optional session-id)
  "Open or create a jcode session for the current project.
With prefix argument, prompt for SESSION-ID and load that session."
  (interactive
   (list (when current-prefix-arg
           (read-string "Jcode session id: "))))
  (let* ((dir (emacs-jcode--project-directory))
         (buffers (emacs-jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
    (emacs-jcode--display-buffers chat input)
    (unless (buffer-local-value 'emacs-jcode--session chat)
      (emacs-jcode-session-start dir chat input session-id nil))
    chat))

;;;###autoload
(defun emacs-jcode-resume (session-id &optional full-load)
  "Resume jcode SESSION-ID without replay.
With prefix argument FULL-LOAD, call ACP `session/load' for history replay."
  (interactive (list (read-string "Jcode session id: ") current-prefix-arg))
  (let* ((dir (emacs-jcode--project-directory))
         (buffers (emacs-jcode--make-buffers dir session-id))
         (chat (car buffers))
         (input (cdr buffers)))
    (emacs-jcode--display-buffers chat input)
    (unless (buffer-local-value 'emacs-jcode--session chat)
      (emacs-jcode-session-start dir chat input session-id (not full-load)))
    chat))

;;;###autoload
(defun emacs-jcode-plan ()
  "Open the emacs-jcode implementation plan."
  (interactive)
  (find-file (expand-file-name "PLAN.org" (file-name-directory (or load-file-name buffer-file-name)))))

(provide 'emacs-jcode)
;;; emacs-jcode.el ends here
