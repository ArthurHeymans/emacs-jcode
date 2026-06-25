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

(defun emacs-jcode--content-text (content)
  "Extract textual content from ACP CONTENT value."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat (lambda (item)
                 (or (and (listp item) (emacs-jcode--alist-get-any '(text content) item)) ""))
               content ""))
   ((listp content)
    (or (emacs-jcode--alist-get-any '(text content) content)
        (format "%S" content)))
   (content (format "%S" content))))

(defun emacs-jcode--event-text (params)
  "Extract text from ACP notification PARAMS."
  (or (emacs-jcode--alist-get-any '(text delta chunk) params)
      (emacs-jcode--content-text (alist-get 'content params))))

(defun emacs-jcode-render-user (chat text)
  "Render user TEXT in CHAT."
  (emacs-jcode--section chat "You" 'emacs-jcode-user-face)
  (emacs-jcode--append chat (concat text "\n")))

(defun emacs-jcode-render-assistant-delta (chat text)
  "Render assistant delta TEXT in CHAT."
  (when (and text (not (string-empty-p text)))
    (emacs-jcode--append chat text)))

(defun emacs-jcode--last-heading (buffer)
  "Return the last simple setext heading title in BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^\\(You\\|Assistant\\)\n=+\n" nil t)
          (match-string-no-properties 1))))))

(defun emacs-jcode-render-assistant-message (chat text)
  "Render assistant message TEXT in CHAT with a heading when needed."
  (unless (equal (emacs-jcode--last-heading chat) "Assistant")
    (emacs-jcode--section chat "Assistant" 'emacs-jcode-assistant-face))
  (emacs-jcode-render-assistant-delta chat text))

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

(defun emacs-jcode--handle-session-update (session params)
  "Render ACP session/update PARAMS for SESSION."
  (let* ((update (alist-get 'update params))
         (kind (alist-get 'sessionUpdate update)))
    (pcase kind
      ("agent_message_chunk"
       (emacs-jcode-render-assistant-message
        (emacs-jcode-session-chat-buffer session)
        (emacs-jcode--event-text update)))
      ("user_message_chunk"
       (emacs-jcode-render-user
        (emacs-jcode-session-chat-buffer session)
        (or (emacs-jcode--event-text update) "")))
      ("tool_call"
       (emacs-jcode-render-tool (emacs-jcode-session-chat-buffer session) update nil))
      ("tool_call_update"
       (emacs-jcode-render-tool (emacs-jcode-session-chat-buffer session) update t))
      (_
       (emacs-jcode-render-info
        (emacs-jcode-session-chat-buffer session)
        (format "session/update %S" params))))))

(defun emacs-jcode-handle-notification (session method params)
  "Render ACP notification METHOD/PARAMS for SESSION."
  (let ((chat (emacs-jcode-session-chat-buffer session)))
    (pcase method
      ("session/update"
       (emacs-jcode--handle-session-update session params))
      ("agent_message_chunk"
       (emacs-jcode-render-assistant-message chat (emacs-jcode--event-text params)))
      ("user_message_chunk"
       (emacs-jcode-render-user chat (or (emacs-jcode--event-text params) "")))
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
