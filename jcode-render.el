;;; jcode-render.el --- Render jcode ACP events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'jcode-ui)
(require 'subr-x)
(require 'button)
(require 'seq)

(declare-function jcode-session-chat-buffer "jcode-acp")

(defcustom jcode-tool-preview-lines 8
  "Number of tool output lines to show before collapsing a block."
  :type 'natnum
  :group 'jcode)

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
    (jcode--insert-tool-block chat name status (jcode--sanitize-text text))))

(defun jcode--tool-block-overlay-at-point ()
  "Return the jcode tool block overlay at point, if any."
  (seq-find (lambda (overlay) (overlay-get overlay 'jcode-tool-block))
            (overlays-at (point))))

(defun jcode--markdown-fence-delimiter (text)
  "Return a markdown fence delimiter safe for TEXT."
  (if (and text (string-match-p "```+" text))
      "~~~"
    "```"))

(defun jcode--tool-block-body (text)
  "Return markdown body for tool output TEXT."
  (if (and text (not (string-empty-p text)))
      (let ((fence (jcode--markdown-fence-delimiter text)))
        (format "%s\n%s\n%s\n" fence (string-trim-right text) fence))
    ""))

(defun jcode--tool-preview (text)
  "Return (PREVIEW . HIDDEN-COUNT) for tool output TEXT."
  (let* ((lines (and text (split-string (string-trim-right text) "\n")))
         (count (length lines)))
    (if (and lines (> count jcode-tool-preview-lines))
        (cons (string-join (seq-take lines jcode-tool-preview-lines) "\n")
              (- count jcode-tool-preview-lines))
      (cons text 0))))

(defun jcode--insert-tool-toggle-button (label overlay)
  "Insert a clickable LABEL linked to tool block OVERLAY."
  (insert-text-button label
                      'face 'jcode-collapsed-face
                      'follow-link t
                      'help-echo "mouse-1 or TAB: toggle tool output"
                      'jcode-tool-block overlay
                      'action (lambda (button)
                                (when-let ((ov (button-get button 'jcode-tool-block)))
                                  (jcode--toggle-tool-overlay ov)))))

(defun jcode--render-tool-body (overlay)
  "Rewrite OVERLAY body according to its collapsed state."
  (let* ((header-end (overlay-get overlay 'jcode-header-end))
         (text (or (overlay-get overlay 'jcode-full-text) ""))
         (collapsed (overlay-get overlay 'jcode-collapsed))
         (preview (jcode--tool-preview text))
         (display-text (if collapsed (car preview) text))
         (hidden (cdr preview))
         (inhibit-read-only t))
    (save-excursion
      (goto-char header-end)
      (delete-region header-end (overlay-end overlay))
      (insert (jcode--tool-block-body display-text))
      (when (> hidden 0)
        (jcode--insert-tool-toggle-button
         (if collapsed
             (format "▸ %d more lines hidden, click or TAB to expand\n" hidden)
           "▾ collapse tool output\n")
         overlay))
      (move-overlay overlay (overlay-start overlay) (point)))))

(defun jcode--toggle-tool-overlay (overlay)
  "Toggle collapsed state for tool block OVERLAY."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (with-current-buffer (overlay-buffer overlay)
      (overlay-put overlay 'jcode-collapsed (not (overlay-get overlay 'jcode-collapsed)))
      (jcode--render-tool-body overlay))))

(defun jcode-toggle-block ()
  "Toggle the collapsible block at point."
  (interactive)
  (if-let ((overlay (jcode--tool-block-overlay-at-point)))
      (jcode--toggle-tool-overlay overlay)
    (message "No collapsible jcode block at point")))

(defun jcode--insert-tool-block (chat name status text)
  "Insert a Pi-like collapsible tool block into CHAT."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (let ((inhibit-read-only t)
            (move (= (point) (point-max))))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (let ((start (point)))
          (insert (propertize name 'font-lock-face 'jcode-tool-face)
                  (propertize (format " %s\n" status) 'font-lock-face 'jcode-dim-face))
          (let* ((header-end (point-marker))
                 (preview (jcode--tool-preview text))
                 (collapsed (> (cdr preview) 0))
                 (overlay (make-overlay start (point) nil nil nil)))
            (overlay-put overlay 'jcode-tool-block t)
            (overlay-put overlay 'jcode-header-end header-end)
            (overlay-put overlay 'jcode-full-text (or text ""))
            (overlay-put overlay 'jcode-collapsed collapsed)
            ;; Background only, so markdown/code syntax foregrounds survive.
            (overlay-put overlay 'face 'jcode-tool-block-face)
            (jcode--render-tool-body overlay)))
        (when move (goto-char (point-max)))))))

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

(provide 'jcode-render)
;;; jcode-render.el ends here
