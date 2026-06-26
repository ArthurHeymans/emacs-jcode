;;; jcode-render.el --- Render jcode ACP events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'jcode-ui)
(require 'subr-x)
(require 'button)
(require 'seq)
(require 'cl-lib)

(declare-function jcode-session-chat-buffer "jcode-acp")

(defcustom jcode-tool-preview-lines 8
  "Number of tool output lines to show before collapsing a block."
  :type 'natnum
  :group 'jcode)

(defcustom jcode-tool-show-preview-lines 0
  "Number of tool output lines shown by default in collapsed tool blocks.
The native jcode TUI defaults to compact tool rows rather than showing output;
the default of 0 matches that behavior.  Expanding the block shows full output."
  :type 'natnum
  :group 'jcode)

(defvar-local jcode--tool-block-counter 0
  "Monotonic counter for jcode tool block ids in the current chat buffer.")

(defconst jcode--edit-tool-names
  '("apply_patch" "patch" "edit" "write" "multiedit")
  "Tool names whose input/output should be presented as code changes.")

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
   ((null content) nil)
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
         (input (jcode--tool-input params))
         (intent (jcode--alist-get-any '(intent description) params))
         (text (jcode--sanitize-text (jcode--tool-output-text params)))
         (display (jcode--tool-display-text name text input)))
    (jcode--insert-tool-block chat name status display input intent text)))

(defun jcode--tool-input (params)
  "Extract structured tool input from PARAMS, if present."
  (let ((value (jcode--alist-get-any '(rawInput input args arguments parameters) params)))
    (cond
     ((and (listp value) (not (stringp value))) value)
     ((and (vectorp value) (> (length value) 0)) value)
     (t nil))))

(defun jcode--tool-output-text (params)
  "Extract textual tool output from PARAMS."
  (or (jcode--event-text params)
      (let ((raw (or (alist-get 'output params)
                     (alist-get 'rawOutput params)
                     (alist-get 'result params))))
        (cond
         ((stringp raw) raw)
         (raw (format "%S" raw))))))

(defun jcode--tool-block-overlay-at-point ()
  "Return the jcode tool block overlay at point, if any."
  (or (when-let* ((button (button-at (point)))
                  (id (button-get button 'jcode-tool-block-id)))
        (jcode--tool-block-overlay-by-id id))
      (seq-find (lambda (overlay) (overlay-get overlay 'jcode-tool-block))
                (overlays-at (point)))
      ;; `overlays-at' uses half-open intervals.  If point is at the end of the
      ;; row/button line, look one character back so TAB still works naturally.
      (and (> (point) (point-min))
           (seq-find (lambda (overlay) (overlay-get overlay 'jcode-tool-block))
                     (overlays-at (1- (point)))))))

(defun jcode--tool-block-overlay-by-id (id)
  "Return live jcode tool block overlay with ID in current buffer."
  (seq-find (lambda (overlay)
              (and (overlay-get overlay 'jcode-tool-block)
                   (equal (overlay-get overlay 'jcode-tool-block-id) id)))
            (overlays-in (point-min) (point-max))))

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

(defun jcode--diff-text-p (text)
  "Return non-nil if TEXT looks like a patch or unified diff."
  (and (stringp text)
       (string-match-p
        (rx line-start (or "diff --git" "*** Begin Patch" "--- " "+++ " "@@"))
        text)))

(defun jcode--edit-tool-name-p (name)
  "Return non-nil when NAME is an edit/apply/write tool."
  (member (downcase (format "%s" name)) jcode--edit-tool-names))

(defun jcode--tool-input-patch-text (input)
  "Return patch text embedded in tool INPUT, when present."
  (or (jcode--tool-input-string input 'patch_text)
      (jcode--tool-input-string input 'patch)
      (jcode--tool-input-string input 'diff)))

(defun jcode--tool-display-text (name output input)
  "Return preferred expanded display text for tool NAME with OUTPUT and INPUT.
Edit tools prefer their patch/diff input over terse success output, mirroring
the native TUI's inline-diff expansion behavior."
  (let ((patch (jcode--tool-input-patch-text input)))
    (cond
     ((and (jcode--edit-tool-name-p name)
           patch
           (not (string-empty-p (string-trim patch))))
      (jcode--sanitize-text patch))
     (t output))))

(defun jcode--diff-counts (text)
  "Return (ADDITIONS . DELETIONS) for diff-like TEXT."
  (let ((adds 0)
        (dels 0))
    (dolist (line (jcode--tool-lines text))
      (cond
       ((and (string-prefix-p "+" line)
             (not (string-prefix-p "+++" line)))
        (setq adds (1+ adds)))
       ((and (string-prefix-p "-" line)
             (not (string-prefix-p "---" line)))
        (setq dels (1+ dels)))))
    (cons adds dels)))

(defun jcode--tool-body-kind (name text)
  "Return render kind for tool NAME expanded TEXT."
  (if (or (jcode--diff-text-p text)
          (jcode--edit-tool-name-p name))
      'diff
    'code))

(defun jcode--propertize-diff-body (body)
  "Apply native-TUI-inspired add/delete faces to diff BODY."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((face (cond
                   ((looking-at-p "^+[^+]") 'jcode-tool-success-face)
                   ((looking-at-p "^-[^-]") 'jcode-tool-error-face)
                   ((looking-at-p "^@@") 'jcode-accent-face)
                   ((looking-at-p "^\\(?:diff --git\\|--- \\|+++ \\|\\*\\*\\* \\|---\\|+++\\)")
                    'jcode-dim-face))))
        (when face
          (add-text-properties (line-beginning-position) (line-end-position)
                               `(font-lock-face ,face))))
      (forward-line 1))
    (buffer-string)))

(defun jcode--tool-block-body-for-kind (text kind)
  "Return rendered body for TEXT of KIND."
  (cond
   ((not (and text (not (string-empty-p text)))) "")
   ((eq kind 'diff)
    (concat (propertize "┌─ diff\n" 'font-lock-face 'jcode-dim-face)
            (jcode--propertize-diff-body (string-trim-right text))
            "\n"
            (propertize "└─\n" 'font-lock-face 'jcode-dim-face)))
   (t (jcode--tool-block-body text))))

(defun jcode--tool-lines (text)
  "Return non-empty output lines for TEXT."
  (if (and text (not (string-empty-p (string-trim text))))
      (split-string (string-trim-right text) "\n")
    nil))

(defun jcode--tool-preview (text &optional max-lines)
  "Return (PREVIEW . HIDDEN-COUNT) for tool output TEXT.
MAX-LINES defaults to `jcode-tool-preview-lines'."
  (let* ((lines (jcode--tool-lines text))
         (count (length lines))
         (max-lines (or max-lines jcode-tool-preview-lines)))
    (if (and lines (> count max-lines))
        (cons (string-join (seq-take lines max-lines) "\n")
              (- count max-lines))
      (cons text 0))))

(defun jcode--tool-status-icon-face (status text)
  "Return (ICON . FACE) for tool STATUS and output TEXT."
  (let ((status (downcase (or status "")))
        (text (or text "")))
    (cond
     ((or (string-match-p (regexp-opt '("error" "fail" "failed")) status)
          (string-prefix-p "Error:" text))
      (cons "✗" 'jcode-tool-error-face))
     ((string-match-p (regexp-opt '("running" "start" "update" "pending")) status)
      (cons "◌" 'jcode-tool-running-face))
     ((string-match-p (regexp-opt '("warn" "partial")) status)
      (cons "⚠" 'jcode-tool-warning-face))
     (t (cons "✓" 'jcode-tool-success-face)))))

(defun jcode--tool-output-token-badge (text)
  "Return (LABEL . FACE) approximating the TUI tool token badge for TEXT."
  (let* ((chars (length (or text "")))
         (tokens (max 1 (/ (+ chars 3) 4))))
    (cons (cond
           ((>= tokens 1000000) (format "%.1fM" (/ tokens 1000000.0)))
           ((>= tokens 1000) (format "%.1fk" (/ tokens 1000.0)))
           (t (number-to-string tokens)))
          (cond
           ((>= tokens 20000) 'jcode-tool-error-face)
           ((>= tokens 8000) 'jcode-tool-warning-face)
           (t 'jcode-tool-face)))))

(defun jcode--tool-input-value (input key)
  "Return KEY from structured tool INPUT."
  (when (listp input)
    (or (alist-get key input)
        (alist-get (intern (substring (symbol-name key) 1)) input))))

(defun jcode--tool-input-string (input key)
  "Return string KEY from structured tool INPUT."
  (let ((value (jcode--tool-input-value input key)))
    (cond
     ((stringp value) value)
     ((numberp value) (number-to-string value))
     (value (format "%s" value)))))

(defun jcode--tool-input-count (input key)
  "Return length of vector/list KEY from INPUT."
  (let ((value (jcode--tool-input-value input key)))
    (cond
     ((vectorp value) (length value))
     ((listp value) (length value)))))

(defun jcode--truncate-end (string width)
  "Truncate STRING to WIDTH characters with an ellipsis."
  (let ((string (or string "")))
    (if (or (not width) (<= (length string) width))
        string
      (concat (substring string 0 (max 0 (1- width))) "…"))))

(defun jcode--truncate-middle (string width)
  "Truncate STRING to WIDTH characters with a middle ellipsis."
  (let ((string (or string "")))
    (cond
     ((or (not width) (<= (length string) width)) string)
     ((<= width 1) "…")
     (t (let* ((budget (1- width))
               (head (/ budget 2))
               (tail (- budget head)))
          (concat (substring string 0 head)
                  "…"
                  (substring string (- (length string) tail))))))))

(defun jcode--truncate-path (path width)
  "Return a TUI-like truncated PATH for WIDTH."
  (let ((path (or path "")))
    (if (or (not width) (<= (length path) width))
        path
      (let* ((parts (split-string (replace-regexp-in-string "\\\\" "/" path) "/" t))
             (last (car (last parts)))
             (marker (cond
                      ((string-prefix-p "~/" path) "~/…/")
                      ((string-prefix-p "./" path) "./…/")
                      ((string-prefix-p "/" path) "/…/")
                      (t "…/"))))
        (if (and last (< (length marker) width))
            (concat marker (jcode--truncate-middle last (- width (length marker))))
          (jcode--truncate-middle path width))))))

(defun jcode--tool-input-bool (input key)
  "Return boolean-like KEY from structured tool INPUT."
  (let ((value (jcode--tool-input-value input key)))
    (and value (not (eq value :false)))))

(defun jcode--tool-input-action (input fallback)
  "Return action from INPUT or FALLBACK."
  (or (jcode--tool-input-string input 'action) fallback))

(defun jcode--tool-display-name (name)
  "Return TUI-like display name for tool NAME."
  (let ((name (or name "tool")))
    (pcase name
      ((or "apply_patch" "patch") "edit")
      ("multiedit" "edit")
      ("webfetch" "web")
      ("websearch" "search")
      (_ name))))

(defun jcode--tool-input-summary (name input intent)
  "Return compact TUI-like summary for tool NAME with INPUT and INTENT."
  (let ((name (downcase (or name ""))))
    (pcase name
      ("bash"
       (when-let ((command (jcode--tool-input-string input 'command)))
         (format "$ %s" (jcode--truncate-end command (if intent 28 80)))))
      ((or "read" "write" "edit")
       (jcode--tool-input-string input 'file_path))
      ("multiedit"
       (when-let ((path (jcode--tool-input-string input 'file_path)))
         (format "%s (%d edits)" path (or (jcode--tool-input-count input 'edits) 0))))
      ("ls" (or (jcode--tool-input-string input 'path) "."))
      ("glob"
       (when-let ((pattern (jcode--tool-input-string input 'pattern)))
         (format "'%s'" (jcode--truncate-end pattern 60))))
      ("grep"
       (when-let ((pattern (jcode--tool-input-string input 'pattern)))
         (if-let ((path (jcode--tool-input-string input 'path)))
             (format "'%s' in %s" (jcode--truncate-end pattern 40) path)
           (format "'%s'" (jcode--truncate-end pattern 60)))))
      ("agentgrep"
       (let ((mode (or (jcode--tool-input-string input 'mode) "grep"))
             (query (jcode--tool-input-string input 'query)))
         (if (and query (not (string-empty-p query)))
             (format "%s '%s'" mode (jcode--truncate-end query 50))
           mode)))
      ((or "webfetch" "websearch" "codesearch" "session_search")
       (when-let ((query (or (jcode--tool-input-string input 'url)
                             (jcode--tool-input-string input 'query))))
         (format "'%s'" (jcode--truncate-end query 70))))
      ("batch"
       (format "%d calls" (or (jcode--tool-input-count input 'tool_calls) 0)))
      ("browser"
       (let ((action (jcode--tool-input-action input "browser")))
         (pcase action
           ((or "open" "new_tab")
            (let ((url (jcode--tool-input-string input 'url)))
              (if (and url (not (string-empty-p url)))
                  (format "%s %s" (replace-regexp-in-string "_" " " action)
                          (jcode--truncate-middle url 44))
                (replace-regexp-in-string "_" " " action))))
           ((or "type" "fill_form" "upload" "press" "eval" "scroll" "select_tab")
            (let ((target (or (jcode--tool-input-string input 'selector)
                              (jcode--tool-input-string input 'contains)
                              (jcode--tool-input-string input 'text)
                              (jcode--tool-input-string input 'key)
                              (jcode--tool-input-string input 'path))))
              (if (and target (not (string-empty-p target)))
                  (format "%s %s" (replace-regexp-in-string "_" " " action)
                          (jcode--truncate-middle target 36))
                (replace-regexp-in-string "_" " " action))))
           (_ (replace-regexp-in-string "_" " " action)))))
      ((or "open" "launch")
       (let ((action (jcode--tool-input-action input "open"))
             (target (jcode--tool-input-string input 'target)))
         (string-trim (format "%s %s" action (or target "")))))
      ("bg"
       (let ((action (jcode--tool-input-action input "bg"))
             (task-id (jcode--tool-input-string input 'task_id)))
         (if task-id (format "%s %s" action (jcode--truncate-middle task-id 20)) action)))
      ("memory"
       (let ((action (jcode--tool-input-action input "memory")))
         (pcase action
           ("remember"
            (format "remember: %s"
                    (jcode--truncate-end (or (jcode--tool-input-string input 'content) "") 35)))
           ("recall"
            (if-let ((query (jcode--tool-input-string input 'query)))
                (format "recall '%s'" (jcode--truncate-middle query 35))
              "recall (recent)"))
           ("search"
            (format "search '%s'"
                    (jcode--truncate-middle (or (jcode--tool-input-string input 'query) "") 35)))
           ((or "forget" "tag" "related")
            (if-let ((id (jcode--tool-input-string input 'id)))
                (format "%s %s" action (jcode--truncate-middle id 30))
              action))
           (_ action))))
      ("initiative"
       (let ((action (jcode--tool-input-action input "initiative"))
             (id (jcode--tool-input-string input 'id))
             (title (jcode--tool-input-string input 'title)))
         (cond
          ((and (equal action "create") title)
           (format "create '%s'" (jcode--truncate-end title 30)))
          ((and id (member action '("show" "focus" "update" "checkpoint")))
           (format "%s %s" action (jcode--truncate-middle id 30)))
          (t action))))
      ("side_panel"
       (let ((action (jcode--tool-input-action input "side_panel"))
             (target (or (jcode--tool-input-string input 'title)
                         (jcode--tool-input-string input 'page_id)
                         (jcode--tool-input-string input 'file_path))))
         (if target
             (format "%s %s" action (jcode--truncate-middle target 40))
           action)))
      ("subagent"
       (format "%s (%s)"
               (or (jcode--tool-input-string input 'description) "task")
               (or (jcode--tool-input-string input 'subagent_type) "agent")))
      ("swarm"
       (let* ((action (jcode--tool-input-action input "swarm"))
              (target (or (jcode--tool-input-string input 'to_session)
                          (jcode--tool-input-string input 'target_session)
                          (jcode--tool-input-string input 'channel)))
              (prompt (or (jcode--tool-input-string input 'prompt)
                          (jcode--tool-input-string input 'message))))
         (cond
          ((and (equal action "spawn") prompt)
           (format "spawn '%s'" (jcode--truncate-end prompt 34)))
          ((and target prompt (member action '("dm" "message" "channel" "broadcast")))
           (format "%s %s '%s'" action (jcode--truncate-middle target 24)
                   (jcode--truncate-end prompt 34)))
          (target (format "%s %s" action (jcode--truncate-middle target 24)))
          (t action))))
      ("conversation_search"
       (cond
        ((jcode--tool-input-string input 'query)
         (format "'%s'" (jcode--truncate-middle (jcode--tool-input-string input 'query) 40)))
        ((jcode--tool-input-bool input 'stats) "stats")
        (t "history")))
      ("lsp"
       (let* ((op (or (jcode--tool-input-string input 'operation) "lsp"))
              (file (or (jcode--tool-input-string input 'file_path) ""))
              (short (file-name-nondirectory file))
              (line (or (jcode--tool-input-string input 'line) "0")))
         (format "%s %s:%s" op short line)))
      (_ nil))))

(defun jcode--tool-summary (name status text input intent)
  "Return a compact TUI-like tool summary."
  (let* ((lines (length (jcode--tool-lines text)))
         (input-summary (jcode--tool-input-summary name input intent))
         (diff-counts (and (jcode--edit-tool-name-p name)
                           (jcode--diff-counts text)))
         (diff-suffix (and diff-counts
                           (or (> (car diff-counts) 0) (> (cdr diff-counts) 0))
                           (format " (+%d -%d)" (car diff-counts) (cdr diff-counts)))))
    (cond
     ((and intent (stringp intent) (not (string-empty-p (string-trim intent))))
      (concat (if (and input-summary (not (string-empty-p input-summary)))
                  (format "%s · %s" (string-trim intent) input-summary)
                (string-trim intent))
              diff-suffix))
     ((and input-summary (not (string-empty-p input-summary)))
      (concat input-summary diff-suffix))
     ((and status (string-match-p (regexp-opt '("running" "start" "update" "pending")) (downcase status)))
      status)
     ((> lines 0) (concat (format "%d line%s hidden" lines (if (= lines 1) "" "s"))
                          diff-suffix))
     (t status))))

(defun jcode--insert-tool-toggle-button (label overlay)
  "Insert a clickable LABEL linked to tool block OVERLAY."
  (let ((id (overlay-get overlay 'jcode-tool-block-id)))
  (insert-text-button label
                      'face 'jcode-collapsed-face
                      'follow-link t
                      'help-echo "mouse-1 or TAB: toggle tool output"
                      'keymap nil
                      'jcode-tool-block-id id
                      'action (lambda (button)
                                (when-let* ((id (button-get button 'jcode-tool-block-id))
                                            (ov (jcode--tool-block-overlay-by-id id)))
                                  (jcode--toggle-tool-overlay ov))))))

(defun jcode--render-tool-body (overlay)
  "Rewrite OVERLAY body according to its collapsed state."
  (let* ((header-end (overlay-get overlay 'jcode-header-end))
         (text (or (overlay-get overlay 'jcode-full-text) ""))
         (collapsed (overlay-get overlay 'jcode-collapsed))
         (preview (jcode--tool-preview text jcode-tool-show-preview-lines))
         (display-text (if collapsed (car preview) text))
         (hidden (cdr preview))
         (kind (overlay-get overlay 'jcode-body-kind))
         (inhibit-read-only t)
         (buffer-read-only nil))
    (save-excursion
      (goto-char header-end)
      (delete-region header-end (overlay-end overlay))
      (unless (or (not display-text) (string-empty-p display-text))
        (insert (jcode--tool-block-body-for-kind display-text kind)))
      (when (or (> hidden 0) (and collapsed (jcode--tool-lines text)))
        (jcode--insert-tool-toggle-button
         (if collapsed
             "    ▸ expand output\n"
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

(defun jcode--insert-tool-block (chat name status text &optional input intent raw-output)
  "Insert a native-TUI-like collapsible tool block into CHAT."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (let ((inhibit-read-only t)
            (move (= (point) (point-max))))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (let ((start (point)))
          (let* ((icon-face (jcode--tool-status-icon-face status text))
                 (badge (jcode--tool-output-token-badge (or raw-output text)))
                 (display-name (jcode--tool-display-name name))
                 (summary (jcode--tool-summary name status text input intent)))
            (insert "  "
                    (propertize (car icon-face) 'font-lock-face (cdr icon-face))
                    " "
                    (propertize display-name 'font-lock-face 'jcode-tool-face))
            (when (and summary (not (string-empty-p summary)))
              (insert (propertize " " 'font-lock-face 'jcode-dim-face)
                      (propertize summary 'font-lock-face 'jcode-dim-face)))
            (when (jcode--tool-lines text)
              (insert (propertize " · " 'font-lock-face 'jcode-dim-face)
                      (propertize (car badge) 'font-lock-face (cdr badge))))
            (insert "\n"))
          (let* ((header-end (point-marker))
                 (_preview (jcode--tool-preview text jcode-tool-show-preview-lines))
                 (collapsed (and (jcode--tool-lines text) t))
                 (overlay (make-overlay start (point) nil nil nil))
                 (id (cl-incf jcode--tool-block-counter)))
            (overlay-put overlay 'jcode-tool-block t)
            (overlay-put overlay 'jcode-tool-block-id id)
            (overlay-put overlay 'jcode-header-end header-end)
            (overlay-put overlay 'jcode-full-text (or text ""))
            (overlay-put overlay 'jcode-body-kind (jcode--tool-body-kind name text))
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
