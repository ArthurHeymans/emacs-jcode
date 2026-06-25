;;; emacs-jcode-input.el --- Input handling for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'emacs-jcode-ui)
(require 'emacs-jcode-render)
(require 'emacs-jcode-native)

(declare-function emacs-jcode-session-prompt "emacs-jcode-acp")
(declare-function emacs-jcode-session-cancel "emacs-jcode-acp")
(declare-function emacs-jcode-session-close "emacs-jcode-acp")
(declare-function emacs-jcode-native-message "emacs-jcode-native" (connection content))
(declare-function emacs-jcode-native-cancel "emacs-jcode-native" (connection))
(declare-function emacs-jcode-native-close "emacs-jcode-native" (connection))

(defvar emacs-jcode-input-ring-size 100
  "Maximum number of prompts to keep in input history.")

(defvar-local emacs-jcode--input-ring nil)
(defvar-local emacs-jcode--input-ring-index nil)
(defvar-local emacs-jcode--input-saved nil)

(defun emacs-jcode--chat-buffer-for-command ()
  "Return chat buffer relevant to the current jcode command."
  (if (derived-mode-p 'emacs-jcode-chat-mode)
      (current-buffer)
    emacs-jcode--chat-buffer))

(defun emacs-jcode--native-connection-for-chat (chat)
  "Return native connection from CHAT, if any."
  (and (buffer-live-p chat)
       (boundp 'emacs-jcode--native-connection)
       (buffer-local-value 'emacs-jcode--native-connection chat)))

(defun emacs-jcode--input-ring ()
  "Return current input history ring."
  (unless emacs-jcode--input-ring
    (setq emacs-jcode--input-ring (make-ring emacs-jcode-input-ring-size)))
  emacs-jcode--input-ring)

(defun emacs-jcode--history-add (text)
  "Add TEXT to input history."
  (let ((trimmed (string-trim text))
        (ring (emacs-jcode--input-ring)))
    (when (and (not (string-empty-p trimmed))
               (or (ring-empty-p ring) (not (string= text (ring-ref ring 0)))))
      (ring-insert ring text))))

(defun emacs-jcode-previous-input ()
  "Cycle backward through prompt history."
  (interactive)
  (let ((ring (emacs-jcode--input-ring)))
    (when (ring-empty-p ring) (user-error "No history"))
    (unless emacs-jcode--input-ring-index
      (setq emacs-jcode--input-saved (buffer-string)))
    (let ((idx (if emacs-jcode--input-ring-index (1+ emacs-jcode--input-ring-index) 0)))
      (when (>= idx (ring-length ring)) (user-error "Beginning of history"))
      (setq emacs-jcode--input-ring-index idx)
      (delete-region (point-min) (point-max))
      (insert (ring-ref ring idx)))))

(defun emacs-jcode-next-input ()
  "Cycle forward through prompt history."
  (interactive)
  (unless emacs-jcode--input-ring-index (user-error "End of history"))
  (let ((idx (1- emacs-jcode--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< idx 0)
        (progn
          (setq emacs-jcode--input-ring-index nil)
          (insert (or emacs-jcode--input-saved "")))
      (setq emacs-jcode--input-ring-index idx)
      (insert (ring-ref (emacs-jcode--input-ring) idx)))))

(defun emacs-jcode-send ()
  "Send current input buffer content to jcode."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat emacs-jcode--chat-buffer)
         (native (emacs-jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat)
                       (buffer-local-value 'emacs-jcode--session chat))))
    (when (string-empty-p text) (user-error "Prompt is empty"))
    (unless (or native session) (user-error "No jcode session"))
    (when (and session (emacs-jcode-session-busy session))
      (user-error "Jcode is busy; wait for the current request or cancel it"))
    (emacs-jcode--history-add text)
    (delete-region (point-min) (point-max))
    (emacs-jcode-render-user chat text)
    (emacs-jcode--section chat "Assistant" 'emacs-jcode-assistant-face)
    (if native
        (emacs-jcode-native-message native text)
      (emacs-jcode-session-prompt session text))))

(defun emacs-jcode-cancel ()
  "Cancel the active jcode response."
  (interactive)
  (let* ((chat (emacs-jcode--chat-buffer-for-command))
         (native (emacs-jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat) (buffer-local-value 'emacs-jcode--session chat))))
    (cond
     (native (emacs-jcode-native-cancel native))
     (session (emacs-jcode-session-cancel session))
     (t (user-error "No jcode session")))))

(defun emacs-jcode-disconnect ()
  "Detach Emacs from current jcode session."
  (interactive)
  (let* ((chat (emacs-jcode--chat-buffer-for-command))
         (native (emacs-jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat) (buffer-local-value 'emacs-jcode--session chat))))
    (cond
     (native (emacs-jcode-native-close native))
     (session (emacs-jcode-session-close session))
     (t (user-error "No jcode session")))))

(provide 'emacs-jcode-input)
;;; emacs-jcode-input.el ends here
