;;; jcode-render.el --- Render jcode ACP events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'emacs-jcode-ui)
(require 'subr-x)

(declare-function jcode-session-chat-buffer "jcode-acp")

(defun jcode--sanitize-text (text)
  "Strip terminal control sequences and undesirable control chars from TEXT."
  (when text
    (let ((s text))
      ;; OSC sequences, including terminal-title updates: ESC ] ... BEL/ST.
      (setq s (replace-regexp-in-string "\033\\][^\a]*\a" "" s))
      ;; Some captured logs lose the ESC byte but keep the OSC payload.
      (setq s (replace-regexp-in-string "^\\]0;[^\a\n\r]*\a" "" s))
      (setq s (replace-regexp-in-string "\n\\]0;[^\a\n\r]*\a" "\n" s))
      (setq s (replace-regexp-in-string "\r\\]0;[^\a\n\r]*\a" "\r" s))
      ;; CSI SGR/control sequences.
      (setq s (replace-regexp-in-string "\033\\[[0-?]*[ -/]*[@-~]" "" s))
      ;; Keep newline, tab, and carriage return; remove other C0 controls.
      (replace-regexp-in-string "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" "" s))))

(defun jcode--alist-get-any (keys alist)
  "Return first present value for KEYS in ALIST."
  (catch 'found
    (dolist (key keys)
      (let ((cell (assq key alist)))
        (when cell (throw 'found (cdr cell)))))))

(defun jcode--content-text (content)
  "Extract textual content from ACP CONTENT value."
  (cond
   ((stringp content) (jcode--sanitize-text content))
   ((vectorp content)
    (mapconcat (lambda (item)
                 (or (and (listp item) (jcode--alist-get-any '(text content) item)) ""))
               content ""))
   ((listp content)
    (or (jcode--alist-get-any '(text content) content)
        (format "%S" content)))
   (content (format "%S" content))))

(defun jcode--event-text (params)
  "Extract text from ACP notification PARAMS."
  (or (jcode--alist-get-any '(text delta chunk) params)
      (jcode--content-text (alist-get 'content params))))

(defun jcode-render-user (chat text)
  "Render user TEXT in CHAT."
  (jcode--section chat "You" 'jcode-user-face)
  (jcode--append chat (concat (jcode--sanitize-text text) "\n")))

(defun jcode-render-assistant-delta (chat text)
  "Render assistant delta TEXT in CHAT."
  (let ((text (jcode--sanitize-text text)))
    (when (and text (not (string-empty-p text)))
      (jcode--append chat text))))

(defun jcode--last-heading (buffer)
  "Return the last simple setext heading title in BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^\\(You\\|Assistant\\)\n=+\n" nil t)
          (match-string-no-properties 1))))))

(defun jcode-render-assistant-message (chat text)
  "Render assistant message TEXT in CHAT with a heading when needed."
  (unless (equal (jcode--last-heading chat) "Assistant")
    (jcode--section chat "Assistant" 'jcode-assistant-face))
  (jcode-render-assistant-delta chat text))

(defun jcode-render-tool (chat params &optional update)
  "Render tool PARAMS in CHAT.  UPDATE non-nil means this is an update."
  (let* ((name (or (jcode--alist-get-any '(name title toolCallId toolCallId) params) "tool"))
         (status (or (jcode--alist-get-any '(status state) params) (if update "update" "start")))
         (text (or (jcode--event-text params)
                   (when-let ((raw (jcode--alist-get-any '(rawInput input output) params)))
                     (if (stringp raw) raw (format "%S" raw))))))
    (jcode--append chat (format "\n[%s: %s]\n" name status) 'jcode-tool-face)
    (when (and text (not (string-empty-p text)))
      (jcode--append chat (concat (jcode--sanitize-text text) "\n") 'jcode-tool-face))))

(defun jcode-render-info (chat text)
  "Render informational TEXT in CHAT."
  (jcode--append chat (concat "\n" text "\n") 'jcode-dim-face))

(defun jcode-render-error (chat text)
  "Render error TEXT in CHAT."
  (jcode--append chat (concat "\nError: " text "\n") 'jcode-error-face))

(defun jcode--handle-session-update (session params)
  "Render ACP session/update PARAMS for SESSION."
  (let* ((update (alist-get 'update params))
         (kind (alist-get 'sessionUpdate update)))
    (pcase kind
      ("agent_message_chunk"
       (jcode-render-assistant-message
        (jcode-session-chat-buffer session)
        (jcode--event-text update)))
      ("user_message_chunk"
       (jcode-render-user
        (jcode-session-chat-buffer session)
        (or (jcode--event-text update) "")))
      ("tool_call"
       (jcode-render-tool (jcode-session-chat-buffer session) update nil))
      ("tool_call_update"
       (jcode-render-tool (jcode-session-chat-buffer session) update t))
      (_
       (jcode-render-info
        (jcode-session-chat-buffer session)
        (format "session/update %S" params))))))

(defun jcode-handle-notification (session method params)
  "Render ACP notification METHOD/PARAMS for SESSION."
  (let ((chat (jcode-session-chat-buffer session)))
    (pcase method
      ("session/update"
       (jcode--handle-session-update session params))
      ("agent_message_chunk"
       (jcode-render-assistant-message chat (jcode--event-text params)))
      ("user_message_chunk"
       (jcode-render-user chat (or (jcode--event-text params) "")))
      ("tool_call"
       (jcode-render-tool chat params nil))
      ("tool_call_update"
       (jcode-render-tool chat params t))
      ("session_info_update"
       (jcode-render-info chat (format "Session: %S" params)))
      ("_jcode/server_event"
       (jcode-render-info chat (format "jcode event: %S" params)))
      (_
       (jcode-render-info chat (format "%s %S" method params))))))

(provide 'emacs-jcode-render)
;;; jcode-render.el ends here
