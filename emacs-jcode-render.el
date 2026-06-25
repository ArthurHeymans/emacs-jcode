;;; emacs-jcode-render.el --- Render jcode ACP events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'emacs-jcode-ui)
(require 'subr-x)

(declare-function emacs-jcode-session-chat-buffer "emacs-jcode-acp")

(defun emacs-jcode--alist-get-any (keys alist)
  "Return first present value for KEYS in ALIST."
  (catch 'found
    (dolist (key keys)
      (let ((cell (assq key alist)))
        (when cell (throw 'found (cdr cell)))))))

(defun emacs-jcode--event-text (params)
  "Extract text from ACP notification PARAMS."
  (or (emacs-jcode--alist-get-any '(content text delta chunk) params)
      (when-let ((content (alist-get 'content params)))
        (cond ((stringp content) content)
              ((vectorp content)
               (mapconcat (lambda (item)
                            (or (and (listp item) (emacs-jcode--alist-get-any '(text content) item)) ""))
                          content ""))))))

(defun emacs-jcode-render-user (chat text)
  "Render user TEXT in CHAT."
  (emacs-jcode--section chat "You" 'emacs-jcode-user-face)
  (emacs-jcode--append chat (concat text "\n")))

(defun emacs-jcode-render-assistant-delta (chat text)
  "Render assistant delta TEXT in CHAT."
  (when (and text (not (string-empty-p text)))
    (emacs-jcode--append chat text)))

(defun emacs-jcode-render-tool (chat params &optional update)
  "Render tool PARAMS in CHAT.  UPDATE non-nil means this is an update."
  (let* ((name (or (emacs-jcode--alist-get-any '(name title toolCallId toolCallId) params) "tool"))
         (status (or (emacs-jcode--alist-get-any '(status state) params) (if update "update" "start")))
         (text (or (emacs-jcode--event-text params)
                   (when-let ((raw (emacs-jcode--alist-get-any '(rawInput input output) params)))
                     (if (stringp raw) raw (format "%S" raw))))))
    (emacs-jcode--append chat (format "\n[%s: %s]\n" name status) 'emacs-jcode-tool-face)
    (when (and text (not (string-empty-p text)))
      (emacs-jcode--append chat (concat text "\n") 'emacs-jcode-tool-face))))

(defun emacs-jcode-render-info (chat text)
  "Render informational TEXT in CHAT."
  (emacs-jcode--append chat (concat "\n" text "\n") 'emacs-jcode-dim-face))

(defun emacs-jcode-render-error (chat text)
  "Render error TEXT in CHAT."
  (emacs-jcode--append chat (concat "\nError: " text "\n") 'emacs-jcode-error-face))

(defun emacs-jcode-handle-notification (session method params)
  "Render ACP notification METHOD/PARAMS for SESSION."
  (let ((chat (emacs-jcode-session-chat-buffer session)))
    (pcase method
      ("agent_message_chunk"
       (emacs-jcode-render-assistant-delta chat (emacs-jcode--event-text params)))
      ("tool_call"
       (emacs-jcode-render-tool chat params nil))
      ("tool_call_update"
       (emacs-jcode-render-tool chat params t))
      ("session_info_update"
       (emacs-jcode-render-info chat (format "Session: %S" params)))
      ("_jcode/server_event"
       (emacs-jcode-render-info chat (format "jcode event: %S" params)))
      (_
       (emacs-jcode-render-info chat (format "%s %S" method params))))))

(provide 'emacs-jcode-render)
;;; emacs-jcode-render.el ends here
