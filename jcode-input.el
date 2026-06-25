;;; jcode-input.el --- Input handling for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ring)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-render)
(require 'jcode-native)

(declare-function jcode-session-prompt "jcode-acp")
(declare-function jcode-session-cancel "jcode-acp")
(declare-function jcode-session-close "jcode-acp")
(declare-function jcode-native-message "jcode-native" (connection content))
(declare-function jcode-native-cancel "jcode-native" (connection))
(declare-function jcode-native-close "jcode-native" (connection))

(defvar jcode-input-ring-size 100
  "Maximum number of prompts to keep in input history.")

(defvar-local jcode--input-ring nil)
(defvar-local jcode--input-ring-index nil)
(defvar-local jcode--input-saved nil)

(defun jcode--chat-buffer-for-command ()
  "Return chat buffer relevant to the current jcode command."
  (if (derived-mode-p 'jcode-chat-mode)
      (current-buffer)
    jcode--chat-buffer))

(defun jcode--native-connection-for-chat (chat)
  "Return native connection from CHAT, if any."
  (and (buffer-live-p chat)
       (boundp 'jcode--native-connection)
       (buffer-local-value 'jcode--native-connection chat)))

(defun jcode--input-ring ()
  "Return current input history ring."
  (unless jcode--input-ring
    (setq jcode--input-ring (make-ring jcode-input-ring-size)))
  jcode--input-ring)

(defun jcode--history-add (text)
  "Add TEXT to input history."
  (let ((trimmed (string-trim text))
        (ring (jcode--input-ring)))
    (when (and (not (string-empty-p trimmed))
               (or (ring-empty-p ring) (not (string= text (ring-ref ring 0)))))
      (ring-insert ring text))))

(defun jcode-previous-input ()
  "Cycle backward through prompt history."
  (interactive)
  (let ((ring (jcode--input-ring)))
    (when (ring-empty-p ring) (user-error "No history"))
    (unless jcode--input-ring-index
      (setq jcode--input-saved (buffer-string)))
    (let ((idx (if jcode--input-ring-index (1+ jcode--input-ring-index) 0)))
      (when (>= idx (ring-length ring)) (user-error "Beginning of history"))
      (setq jcode--input-ring-index idx)
      (delete-region (point-min) (point-max))
      (insert (ring-ref ring idx)))))

(defun jcode-next-input ()
  "Cycle forward through prompt history."
  (interactive)
  (unless jcode--input-ring-index (user-error "End of history"))
  (let ((idx (1- jcode--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< idx 0)
        (progn
          (setq jcode--input-ring-index nil)
          (insert (or jcode--input-saved "")))
      (setq jcode--input-ring-index idx)
      (insert (ring-ref (jcode--input-ring) idx)))))

(defun jcode-send ()
  "Send current input buffer content to jcode."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat jcode--chat-buffer)
         (native (jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat)
                       (buffer-local-value 'jcode--session chat))))
    (when (string-empty-p text) (user-error "Prompt is empty"))
    (unless (or native session) (user-error "No jcode session"))
    (when (and session (jcode-session-busy session))
      (user-error "Jcode is busy; wait for the current request or cancel it"))
    (jcode--history-add text)
    (delete-region (point-min) (point-max))
    (jcode-render-user chat text)
    (jcode--section chat "Assistant" 'jcode-assistant-face)
    (if native
        (jcode-native-message native text)
      (jcode-session-prompt session text))))

(defun jcode-cancel ()
  "Cancel the active jcode response."
  (interactive)
  (let* ((chat (jcode--chat-buffer-for-command))
         (native (jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat) (buffer-local-value 'jcode--session chat))))
    (cond
     (native (jcode-native-cancel native))
     (session (jcode-session-cancel session))
     (t (user-error "No jcode session")))))

(defun jcode-disconnect ()
  "Detach Emacs from current jcode session."
  (interactive)
  (let* ((chat (jcode--chat-buffer-for-command))
         (native (jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat) (buffer-local-value 'jcode--session chat))))
    (cond
     (native (jcode-native-close native))
     (session (jcode-session-close session))
     (t (user-error "No jcode session")))))

(provide 'jcode-input)
;;; jcode-input.el ends here
