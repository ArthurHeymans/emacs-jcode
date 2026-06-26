;;; jcode-test.el --- Tests for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'jcode)

(ert-deftest jcode-acp-prompt-params-use-content-blocks ()
  (let ((session (jcode--make-session :id "s1" :cwd "/tmp")))
    (should (equal (jcode--acp-prompt-params session "hello")
                   '(:sessionId "s1" :prompt [(:type "text" :text "hello")])))))

(ert-deftest jcode-acp-load-params-include-cwd ()
  (let ((session (jcode--make-session :cwd "/tmp/project")))
    (should (equal (jcode--acp-load-params session "abc")
                   '(:sessionId "abc" :cwd "/tmp/project")))))

(ert-deftest jcode-render-session-update-agent-message ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (session (jcode--make-session :id "s1" :chat-buffer chat)))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-handle-notification
           session "session/update"
           '((sessionId . "s1")
             (update . ((sessionUpdate . "agent_message_chunk")
                        (content . ((type . "text") (text . "hello")))))))
          (with-current-buffer chat
            (should (string-match-p "hello" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-render-session-update-user-message ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (session (jcode--make-session :id "s1" :chat-buffer chat)))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-handle-notification
           session "session/update"
           '((sessionId . "s1")
             (update . ((sessionUpdate . "user_message_chunk")
                        (content . ((type . "text") (text . "hi")))))))
          (with-current-buffer chat
            (should (string-match-p "You" (buffer-string)))
            (should (string-match-p "hi" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-unknown-events-render-summary-not-raw-json ()
  (let* ((chat (generate-new-buffer " *jcode-test-unknown-event-chat*"))
         (session (jcode--make-session :id "s1" :chat-buffer chat)))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-handle-notification
           session "session/update"
           '((sessionId . "s1")
             (update . ((sessionUpdate . "mystery_update")
                        (jsonrpc . "2.0")
                        (method . "secret/method")
                        (params . ((large . "payload")))))))
          (jcode-handle-notification
           session "unknown/method"
           '((jsonrpc . "2.0")
             (method . "secret/method")
             (params . ((large . "payload")))
             (sessionId . "s1")))
          (with-current-buffer chat
            (let ((text (buffer-string)))
              (should (string-match-p "mystery_update" text))
              (should (string-match-p "unknown/method: session s1" text))
              (should-not (string-match-p "jsonrpc" text))
              (should-not (string-match-p "secret/method" text))
              (should-not (string-match-p "large" text)))))
      (kill-buffer chat))))

(ert-deftest jcode-assistant-content-jsonrpc-envelope-renders-inner-event ()
  (let ((chat (generate-new-buffer " *jcode-test-jsonrpc-envelope-chat*"))
        (envelope "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"s1\",\"update\":{\"sessionUpdate\":\"tool_call_update\",\"title\":\"Running shell command\",\"status\":\"completed\",\"content\":\"ok\"}}}"))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-assistant-message chat envelope)
          (with-current-buffer chat
            (let ((text (buffer-string)))
              (should (string-match-p "Running shell command" text))
              (should-not (string-match-p "jsonrpc" text))
              (should-not (string-match-p "session/update" text)))))
      (kill-buffer chat))))

(ert-deftest jcode-chat-mode-uses-markdown-rendering-when-available ()
  (let ((chat (generate-new-buffer " *jcode-test-md-chat*")))
    (unwind-protect
        (with-current-buffer chat
          (jcode-chat-mode)
          (should (derived-mode-p 'jcode-chat-mode))
          (when (fboundp 'md-ts-mode)
            (should (derived-mode-p 'md-ts-mode))
            (should (bound-and-true-p md-ts-hide-markup)))
          (should buffer-read-only)
          (should word-wrap)
          (should (eq (get 'jcode-chat-mode 'mode-class) 'special)))
      (kill-buffer chat))))

(ert-deftest jcode-chat-mode-uses-native-inspired-face-remaps ()
  (let ((chat (generate-new-buffer " *jcode-test-native-style-chat*")))
    (unwind-protect
        (with-current-buffer chat
          (jcode-chat-mode)
          (should-not (assq 'default face-remapping-alist))
          (should (eq (face-attribute 'jcode-text-face :inherit nil t) 'default))
          (should (equal (cadr (assq 'font-lock-comment-face face-remapping-alist))
                         'jcode-dim-face))
          (should (equal (cadr (assq 'md-ts-heading-1 face-remapping-alist))
                         'jcode-heading-1-face))
          (should (equal (cadr (assq 'md-ts-code face-remapping-alist))
                         'jcode-code-face))
          (should (equal (cadr (assq 'md-ts-block-quote face-remapping-alist))
                         'jcode-dim-face))
          (should (equal (face-attribute 'jcode-assistant-face :foreground nil t)
                         "#81c784"))
          (should (equal (face-attribute 'jcode-user-face :foreground nil t)
                         "#8ab4f8")))
      (kill-buffer chat))))

(ert-deftest jcode-chat-mode-tab-toggles-blocks-not-indent ()
  (with-temp-buffer
    (jcode-chat-mode)
    (should (eq (key-binding (kbd "TAB")) #'jcode-toggle-block))
    (should (eq (key-binding (kbd "<tab>")) #'jcode-toggle-block))
    (should-not (eq (key-binding (kbd "TAB")) #'indent-for-tab-command))))

(ert-deftest jcode-input-mode-binds-history-isearch ()
  (with-temp-buffer
    (jcode-input-mode)
    (should (eq (key-binding (kbd "C-r")) #'jcode-history-isearch-backward))))

(ert-deftest jcode-history-isearch-goto-loads-history-and-restores-saved-input ()
  (with-temp-buffer
    (jcode-input-mode)
    (jcode--history-add "first prompt")
    (jcode--history-add "second prompt")
    (insert "draft")
    (setq jcode--history-isearch-saved-input (buffer-string))
    (jcode--history-isearch-goto 0)
    (should (equal (buffer-string) "second prompt"))
    (jcode--history-isearch-goto 1)
    (should (equal (buffer-string) "first prompt"))
    (jcode--history-isearch-goto nil)
    (should (equal (buffer-string) "draft"))))

(ert-deftest jcode-seed-input-history-uses-server-prompts-newest-first ()
  (with-temp-buffer
    (jcode-input-mode)
    (jcode-seed-input-history '("old prompt" "new prompt"))
    (should (= (ring-length (jcode--input-ring)) 2))
    (should (equal (ring-ref (jcode--input-ring) 0) "new prompt"))
    (should (equal (ring-ref (jcode--input-ring) 1) "old prompt"))))

(ert-deftest jcode-native-history-seeds-input-history-from-user-messages ()
  (let ((chat (generate-new-buffer " *jcode-test-history-chat*"))
        (input (generate-new-buffer " *jcode-test-history-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (jcode-native--render-history
             connection
             '((messages . [((role . "user") (content . "first"))
                            ((role . "assistant") (content . "reply"))
                            ((role . "user") (content . "second"))])))
            (with-current-buffer input
              (should (= (ring-length (jcode--input-ring)) 2))
              (should (equal (ring-ref (jcode--input-ring) 0) "second"))
              (should (equal (ring-ref (jcode--input-ring) 1) "first")))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-decorate-tables-adds-display-overlay-with-canonical-text ()
  (let ((chat (generate-new-buffer " *jcode-test-table-chat*")))
    (unwind-protect
        (with-current-buffer chat
          (jcode-chat-mode)
          (let ((raw "| Name | Notes |\n| ---- | ----- |\n| Jcode | a long table cell that can wrap |\n"))
            (let ((inhibit-read-only t)) (insert raw))
            (jcode-decorate-tables)
            (should (equal (buffer-string) raw))
            (let ((overlays (cl-remove-if-not
                             (lambda (ov) (overlay-get ov 'jcode-table-overlay))
                             (overlays-in (point-min) (point-max)))))
              (should overlays)
              (should (string-match-p "Jcode" (overlay-get (car overlays) 'display))))))
      (kill-buffer chat))))

(ert-deftest jcode-render-thinking-delta-collapses-and-toggles ()
  (let ((chat (generate-new-buffer " *jcode-test-thinking-chat*"))
        (jcode-thinking-display 'hidden))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-thinking-delta chat "planning\nmore detail")
          (with-current-buffer chat
            (should (string-match-p "▶ Thinking" (buffer-string)))
            (should (string-match-p "Thinking: planning" (buffer-string)))
            (should-not (string-match-p "more detail" (buffer-string)))
            (goto-char (point-min))
            (jcode-toggle-block)
            (should (string-match-p "▼ Thinking" (buffer-string)))
            (should (string-match-p "more detail" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-append-preserves-visible-scroll-when-reading-history ()
  (let ((chat (generate-new-buffer " *jcode-test-scroll-chat*")))
    (unwind-protect
        (let ((window (selected-window)))
          (set-window-buffer window chat)
          (with-current-buffer chat
            (jcode-chat-mode)
            (let ((inhibit-read-only t))
              (dotimes (i 80) (insert (format "line %d\n" i))))
            (goto-char (point-min)))
          (set-window-point window (with-current-buffer chat (point-min)))
          (set-window-start window (with-current-buffer chat (point-min)) t)
          (jcode--append chat "new output\n")
          (should (= (window-point window)
                     (with-current-buffer chat (point-min))))
          (should-not (pos-visible-in-window-p
                       (with-current-buffer chat (point-max)) window)))
      (when (buffer-live-p chat) (kill-buffer chat)))))

(ert-deftest jcode-append-follows-when-visible-window-at-bottom ()
  (let ((chat (generate-new-buffer " *jcode-test-follow-chat*")))
    (unwind-protect
        (let ((window (selected-window)))
          (set-window-buffer window chat)
          (with-current-buffer chat
            (jcode-chat-mode)
            (let ((inhibit-read-only t))
              (insert "line\n")))
          (set-window-point window (with-current-buffer chat (point-max)))
          (jcode--append chat "new output\n")
          (should (= (window-point window)
                     (with-current-buffer chat (point-max)))))
      (when (buffer-live-p chat) (kill-buffer chat)))))

(ert-deftest jcode-tool-block-collapses-and-expands-long-output ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-collapse-chat*"))
        (jcode-tool-preview-lines 2))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "done")
                                    (text . "one\ntwo\nthree\nfour")))
          (with-current-buffer chat
            (should (string-match-p "✓ bash" (buffer-string)))
            (should (string-match-p "4 lines hidden" (buffer-string)))
            (should-not (string-match-p "one" (buffer-string)))
            (should-not (string-match-p "two" (buffer-string)))
            (should-not (string-match-p "three" (buffer-string)))
            (goto-char (point-min))
            (search-forward "bash")
            (jcode-toggle-block)
            (should (string-match-p "one" (buffer-string)))
            (should (string-match-p "three" (buffer-string)))
            (should (string-match-p "collapse output" (buffer-string)))
            (should-not (string-match-p "^▾ collapse" (buffer-string)))
            (should (< (save-excursion (goto-char (point-min)) (search-forward "bash") (point))
                       (save-excursion (goto-char (point-min)) (search-forward "one") (point))))))
      (kill-buffer chat))))

(ert-deftest jcode-tool-block-tab-binding-toggles-from-row-and-button ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-tab-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "done")
                                    (text . "one\ntwo")))
          (with-current-buffer chat
            (goto-char (point-min))
            (search-forward "bash")
            (call-interactively (key-binding (kbd "TAB")))
            (should (string-match-p "one" (buffer-string)))
            (search-forward "collapse output")
            (call-interactively (key-binding (kbd "TAB")))
            (should-not (string-match-p "one" (buffer-string)))
            (should (string-match-p "expand output" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-tool-block-expands-when-buffer-is-read-only ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-read-only-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "done")
                                    (text . "one\ntwo")))
          (with-current-buffer chat
            (setq buffer-read-only t)
            (goto-char (point-min))
            (search-forward "bash")
            (jcode-toggle-block)
            (should (string-match-p "one" (buffer-string)))
            (should buffer-read-only)))
      (kill-buffer chat))))

(ert-deftest jcode-non-edit-update-tools-stay-collapsed-by-default ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-diff-chat*"))
        (patch "*** Begin Patch\n*** Update File: foo.el\n@@\n-old\n+new\n*** End Patch"))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat `((name . "apply_patch")
	                                    (status . "done")
	                                    (output . "Done")
	                                    (input . ((patch_text . ,patch)))))
	          (with-current-buffer chat
	            (should (string-match-p "▸ expand output" (buffer-string)))
	            (should-not (string-match-p "┌─ diff" (buffer-string)))
	            (should-not (string-match-p "old" (buffer-string)))))
	      (kill-buffer chat))))

(ert-deftest jcode-edit-update-renders-synthetic-diff-expanded ()
  (let ((chat (generate-new-buffer " *jcode-test-edit-update-diff-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((title . "edit update")
                                    (status . "done")
                                    (output . "updated")
                                    (input . ((file_path . "foo.el")
                                              (old_string . "(message \"old\")")
                                              (new_string . "(message \"new\")"))))
                             t)
          (with-current-buffer chat
            (should (string-match-p (regexp-quote "✓ edit foo.el (+1 -1)") (buffer-string)))
            (should (string-match-p "┌─ diff" (buffer-string)))
	            (should (string-match-p "--- foo.el" (buffer-string)))
	            (search-forward "-(message \"old\")")
	            (should (eq (get-text-property (line-beginning-position) 'face)
	                        'jcode-diff-removed-face))
	            (search-forward "+(message \"new\")")
	            (should (eq (get-text-property (line-beginning-position) 'face)
	                        'jcode-diff-added-face))))
      (kill-buffer chat))))

(ert-deftest jcode-render-user-inserts-blank-line-before-new-user-turn ()
  (let ((chat (generate-new-buffer " *jcode-test-user-spacing-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-assistant-message chat "Validated: ok\nCommitted: abc")
          (jcode-render-user chat "next request")
          (with-current-buffer chat
            (should (string-match-p "Committed: abc\n\nYou\n===" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-render-separates-assistant-section-after-user-turn ()
  (let ((chat (generate-new-buffer " *jcode-test-assistant-spacing-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-user chat "question")
          (jcode-render-assistant-message chat "answer")
          (with-current-buffer chat
            (should (string-match-p "question\n\nAssistant\n=========" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-section-heading-underlines-are-display-hidden ()
  (let ((chat (generate-new-buffer " *jcode-test-heading-hidden-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-assistant-message chat "answer")
          (with-current-buffer chat
            (goto-char (point-min))
            (search-forward "=========")
            (should (get-text-property (match-beginning 0) 'invisible))
            (should (equal (get-text-property (match-beginning 0) 'display) ""))))
      (kill-buffer chat))))

(ert-deftest jcode-assistant-heading-face-does-not-leak-into-markdown ()
  (let ((chat (generate-new-buffer " *jcode-test-markdown-face-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-assistant-message chat "**bold**\n\n```elisp\n(message \"x\")\n```")
          (with-current-buffer chat
            (goto-char (point-min))
            (search-forward "**bold**")
            (should-not (eq (get-text-property (match-beginning 0) 'face)
              'jcode-assistant-face))))
      (kill-buffer chat))))

(ert-deftest jcode-assistant-markdown-markup-is-hidden-after-stream-append ()
  (let ((chat (generate-new-buffer " *jcode-test-markdown-hidden-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-assistant-message chat "**bold** and `code`")
          (with-current-buffer chat
            (when (derived-mode-p 'md-ts-mode)
              ;; Some batch/Nix environments can load `md-ts-mode' but do not
              ;; install its hide-markup font-lock rules.  In that case this
              ;; regression is not meaningful for the current Emacs build.
              (font-lock-ensure (point-min) (point-max))
              (goto-char (point-min))
              (search-forward "**")
              (unless (get-text-property (match-beginning 0) 'invisible)
                (ert-skip "md-ts-mode hide-markup font-lock is unavailable"))
              (should (get-text-property (match-beginning 0) 'invisible))
              (search-forward "`")
              (should (get-text-property (match-beginning 0) 'invisible)))))
      (kill-buffer chat))))

(ert-deftest jcode-tool-row-summarizes-input-without-showing-it-as-output ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-summary-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "done")
                                    (input . ((command . "nix flake check")))))
          (with-current-buffer chat
            (should (string-match-p "✓ bash" (buffer-string)))
            (should (string-match-p "\\$ nix flake check" (buffer-string)))
            (should-not (string-match-p "output hidden" (buffer-string)))
            (should-not (string-match-p "((command" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-tool-output-text-extracts-output-key ()
  (should (equal (jcode--tool-output-text '((output . "one\ntwo")))
                 "one\ntwo")))

(ert-deftest jcode-tool-row-uses-tui-display-name-and-running-color ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-running-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "webfetch")
                                    (status . "start")
                                    (input . ((url . "https://example.com/some/long/path")))))
          (with-current-buffer chat
            (should (string-match-p "◌ web" (buffer-string)))
            (should (string-match-p "https://example.com" (buffer-string)))
            (goto-char (point-min))
            (search-forward "◌")
              (should (eq (get-text-property (1- (point)) 'font-lock-face)
                        'jcode-tool-running-face))))
	      (kill-buffer chat))))

(ert-deftest jcode-live-tool-updates-reuse-row-and-finish-icon ()
  (let ((chat (generate-new-buffer " *jcode-test-live-tool-update-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((type . "tool_start") (id . "tool-1") (name . "bash")))
          (jcode-render-tool chat '((type . "tool_done")
                                    (id . "tool-1")
                                    (name . "bash")
                                    (output . "ok"))
                             t)
          (with-current-buffer chat
            (should (= (how-many "bash" (point-min) (point-max)) 1))
            (should (string-match-p "✓ bash" (buffer-string)))
            (should-not (string-match-p "◌ bash" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-native-tool-input-does-not-render-update-row ()
  (let ((chat (generate-new-buffer " *jcode-test-tool-input-chat*"))
        (input (generate-new-buffer " *jcode-test-tool-input-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (jcode-native--handle-event connection '((type . "tool_start") (id . "t1") (name . "bash")))
            (jcode-native--handle-event connection '((type . "tool_input") (delta . "{\"command\":")))
            (jcode-native--handle-event connection '((type . "tool_done") (id . "t1") (name . "bash") (output . "ok")))
            (with-current-buffer chat
              (should (= (how-many "bash" (point-min) (point-max)) 1))
              (should (string-match-p "✓ bash" (buffer-string)))
              (should-not (string-match-p "update" (buffer-string))))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-tool-row-summarizes-native-tui-common-tools ()
  (should (equal (jcode--tool-input-summary
                  "browser" '((action . "open")
                               (url . "https://example.com/a/b/c")) nil)
                 "open https://example.com/a/b/c"))
  (should (equal (jcode--tool-input-summary
                  "memory" '((action . "remember")
                              (content . "important project fact")) nil)
                 "remember: important project fact"))
  (should (equal (jcode--tool-input-summary
                  "swarm" '((action . "dm")
                             (to_session . "worker-1")
                             (message . "please continue")) nil)
                 "dm worker-1 'please continue'"))
  (should (equal (jcode--tool-input-summary
                  "conversation_search" '((stats . t)) nil)
                 "stats"))
  (should (equal (jcode--tool-input-summary
                  "side_panel" '((action . "write")
                                  (title . "Implementation notes")) nil)
                 "write Implementation notes")))

(ert-deftest jcode-tool-input-summary-counts-batch-calls ()
  (should (equal (jcode--tool-input-summary
                  "batch" '((tool_calls . [((tool . "read")) ((tool . "grep"))])) nil)
                 "2 calls"))
  (should-not (jcode--tool-input-summary "batch" '((tool_calls . [])) nil)))

(ert-deftest jcode-tool-rows-are-compact-without-blank-lines ()
  (let ((chat (generate-new-buffer " *jcode-test-compact-tools-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "start")
                                    (input . ((command . "echo one")))))
          (jcode-render-tool chat '((name . "bash")
                                    (status . "done")
                                    (input . ((command . "echo two")))))
          (with-current-buffer chat
            (should-not (string-match-p "\n\n  [◌✓]" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-header-renders-only-on-input-with-clear-usage ()
  (let ((chat (generate-new-buffer " *jcode-test-header-chat*"))
        (input (generate-new-buffer " *jcode-test-header-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (dolist (buffer (list chat input))
            (jcode--set-display-metadata
             buffer
             :provider "anthropic"
             :credential "oauth"
             :model "claude-sonnet-4"
             :reasoning-effort "xhigh"
             :service-tier "priority"
             :context-window 200000000
             :client-count 2
             :owner 'owned
             :token-usage-totals '((input_tokens . 64000000)
                                   (output_tokens . 171100)
                                   (cache_read_input_tokens . 60900000))))
          (with-current-buffer chat
            (should-not header-line-format))
          (with-current-buffer input
            (let ((header (jcode--header-line)))
              (should (string-match-p "anthropic (oauth).*sonnet-4" header))
              (should (string-match-p "xhigh" header))
              (should-not (string-match-p "think xhigh" header))
              (should (string-match-p "fast on" header))
              (should (string-match-p "owned" header))
              (should-not (string-match-p "owner Emacs" header))
              (should-not (string-match-p "clients 2" header))
              (should (string-match-p "ctx 124\\.9M/200\\.0M" header))
              (should (string-match-p "total in 64\\.0M out 171\\.1k" header))
              (should (string-match-p "cache 60\\.9M" header))
              (should-not (string-match-p "usage input" header))
              (should-not (string-match-p "emacs-jcode" header)))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-native-send-dead-process-has-friendly-error ()
  (let ((connection (jcode--make-native-connection :process nil)))
    (should-error (jcode-native--send connection '(:type "set_model"))
                  :type 'user-error)))

(ert-deftest jcode-load-older-history-requests-expanded-compacted-window ()
  (let ((chat (generate-new-buffer " *jcode-test-compact-chat*"))
        (input (generate-new-buffer " *jcode-test-compact-input*"))
        sent)
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode) (setq jcode--chat-buffer chat))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (with-current-buffer chat
              (setq jcode--native-connection connection
                    jcode--compacted-visible 65
                    jcode--compacted-remaining 128))
            (cl-letf (((symbol-function 'jcode-native--request)
                       (lambda (_connection type &rest fields)
                         (setq sent (cons type fields))
                         9))
                      ((symbol-function 'message) #'ignore))
              (with-current-buffer chat (jcode-load-older-history))
              (should (equal sent '("get_compacted_history" :visible_messages 129))))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-native-compacted-history-rerenders-and-remembers-counters ()
  (let ((chat (generate-new-buffer " *jcode-test-compact-event-chat*"))
        (input (generate-new-buffer " *jcode-test-compact-event-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (jcode-native--handle-event
             connection
             '((type . "compacted_history")
               (session_id . "s1")
               (compacted_total . 2347)
               (compacted_visible . 129)
               (compacted_remaining . 2218)
               (messages . [((role . "user") (content . "old question"))
                            ((role . "assistant") (content . "old answer"))])))
            (with-current-buffer chat
              (should (string-match-p "old question" (buffer-string)))
              (should (string-match-p "old answer" (buffer-string)))
              (should (= jcode--compacted-visible 129))
              (should (= jcode--compacted-remaining 2218)))
            (with-current-buffer input
              (should (= jcode--compacted-total 2347)))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-select-fast-mode-sends-selected-service-tier ()
  (let ((chat (generate-new-buffer " *jcode-test-fast-chat*"))
        (input (generate-new-buffer " *jcode-test-fast-input*"))
        sent)
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode) (setq jcode--chat-buffer chat))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (with-current-buffer chat
              (setq jcode--native-connection connection)
              (jcode--set-display-metadata chat :service-tier "off"))
            (cl-letf (((symbol-function 'jcode-native--request)
                       (lambda (_connection type &rest fields)
                         (setq sent (cons type fields))
                         7))
	                      ((symbol-function 'message) #'ignore))
	              (with-current-buffer input
	                (jcode-select-fast-mode "priority")))
	            (should (equal sent '("set_service_tier" :service_tier "priority")))
	            (with-current-buffer input
	              (should (equal jcode--display-service-tier "priority")))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-native-service-tier-event-updates-header ()
  (let ((chat (generate-new-buffer " *jcode-test-fast-event-chat*"))
        (input (generate-new-buffer " *jcode-test-fast-event-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (jcode-native--handle-event
             connection '((type . "service_tier_changed")
                          (service_tier . "priority")))
            (with-current-buffer input
              (should (equal jcode--display-service-tier "priority"))
              (should (string-match-p "fast on" (jcode--header-line))))))
	      (when (buffer-live-p chat) (kill-buffer chat))
	      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-native-activity-events-update-header-labels ()
  (let ((chat (generate-new-buffer " *jcode-test-activity-chat*"))
        (input (generate-new-buffer " *jcode-test-activity-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection :chat chat :input input)))
            (jcode-native--handle-event connection '((type . "reasoning_delta")))
            (with-current-buffer input
              (should (string-match-p "thinking" (jcode--header-line))))
            (jcode-native--handle-event connection '((type . "text_delta") (text . "hi")))
            (with-current-buffer input
              (should (string-match-p "responding" (jcode--header-line))))
            (jcode-native--handle-event connection '((type . "tool_start") (name . "bash")))
            (with-current-buffer input
              (should (string-match-p "tool:bash" (jcode--header-line))))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-native-session-renamed-updates-title-and-buffer-names ()
  (let ((chat (generate-new-buffer " *jcode-test-rename-chat*"))
        (input (generate-new-buffer " *jcode-test-rename-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (let ((connection (jcode--make-native-connection
                             :chat chat :input input :session-id "s-rename")))
            (with-current-buffer chat (setq jcode--display-session-id "s-rename"))
            (cl-letf (((symbol-function 'jcode-refresh-session-list-buffers) #'ignore))
              (jcode-native--handle-event
               connection '((type . "session_renamed")
                            (display_title . "New Name"))))
            (with-current-buffer chat
              (should (equal jcode--display-title "New Name"))
              (should (string-match-p "New Name" (buffer-name))))
            (with-current-buffer input
              (should (equal jcode--display-title "New Name"))
              (should (string-match-p "New Name" (buffer-name))))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-header-fast-click-opens-selector ()
  (should (eq (lookup-key jcode--header-fast-map [header-line mouse-1])
              #'jcode-select-fast-mode)))

(ert-deftest jcode-header-shows-current-context-when-max-unknown ()
  (with-temp-buffer
    (jcode-input-mode)
    (jcode--set-display-metadata
     (current-buffer)
     :token-usage-totals '((input_tokens . 1200) (output_tokens . 300)))
    (let ((header (jcode--header-line)))
      (should (string-match-p "ctx 1\\.2k" header))
      (should (string-match-p "total in 1\\.2k out 300" header)))))

(ert-deftest jcode-header-shows-current-context-without-guessing-max ()
  (with-temp-buffer
    (jcode-input-mode)
    (jcode--set-display-metadata
     (current-buffer)
     :provider "OpenAI"
     :model "gpt-5.5"
     :token-usage-totals '((input_tokens . 718900)
	                           (output_tokens . 5100)
	                           (cache_read_input_tokens . 612100)))
    (let ((header (jcode--header-line)))
      (should (string-match-p "ctx 718\\.9k" header))
      (should-not (string-match-p "ctx 718\\.9k/" header))
      (should (string-match-p "total in 718\\.9k out 5\\.1k cache 612\\.1k" header)))))

(ert-deftest jcode-native-event-context-window-recognizes-aliases ()
  (should (equal (jcode-native--event-context-window '((max_context_tokens . 200000)))
                 200000))
  (should (equal (jcode-native--event-context-window '((context_window . 128000)))
                 128000)))

(ert-deftest jcode-command-from-chat-reuses-current-pair ()
  (let ((chat (generate-new-buffer " *jcode-test-command-chat*"))
        (input (generate-new-buffer " *jcode-test-command-input*"))
        shown)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat))
          (cl-letf (((symbol-function 'jcode--show-session-buffers)
                     (lambda (shown-chat shown-input)
                       (setq shown (cons shown-chat shown-input)))))
            (with-current-buffer chat
              (jcode)))
          (should (eq (car shown) chat))
          (should (eq (cdr shown) input)))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-command-from-chat-recovers-input-backlink ()
  (let ((chat (generate-new-buffer " *jcode-test-stale-command-chat*"))
        (input (generate-new-buffer " *jcode-test-stale-command-input*"))
        shown)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq default-directory "/tmp/"
                  jcode--input-buffer nil))
          (with-current-buffer input
            (jcode-input-mode)
            (setq default-directory "/tmp/"
                  jcode--chat-buffer chat))
          (cl-letf (((symbol-function 'jcode--show-session-buffers)
                     (lambda (shown-chat shown-input)
                       (setq shown (cons shown-chat shown-input)))))
            (with-current-buffer chat
              (jcode)))
          (should (eq (car shown) chat))
          (should (eq (cdr shown) input)))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-sanitize-text-strips-terminal-controls ()
  (should (equal (jcode--sanitize-text "\033]0;title\ahello\033[31m red\033[0m")
                 "hello red"))
  (should (equal (jcode--sanitize-text "]0;title\aTests pass")
                 "Tests pass")))

(ert-deftest jcode-input-history-roundtrip ()
  (with-temp-buffer
    (jcode-input-mode)
    (insert "first")
    (jcode--history-add (buffer-string))
    (delete-region (point-min) (point-max))
    (insert "second")
    (jcode--history-add (buffer-string))
    (delete-region (point-min) (point-max))
    (jcode-previous-input)
    (should (equal (buffer-string) "second"))
    (jcode-previous-input)
    (should (equal (buffer-string) "first"))
    (jcode-next-input)
    (should (equal (buffer-string) "second"))))

(ert-deftest jcode-send-errors-while-busy ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (input (generate-new-buffer " *jcode-test-input*"))
         (session (jcode--make-session :id "s1"
                                             :cwd "/tmp"
                                             :chat-buffer chat
                                             :input-buffer input
                                             :busy t)))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--session session))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "hello")
            (should-error (jcode-send) :type 'user-error)))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-read-session-info-parses-metadata ()
  (let ((file (make-temp-file "jcode-session" nil ".json"
                              "{\"id\":\"s1\",\"custom_title\":\"Custom\",\"title\":\"Title\",\"short_name\":\"short\",\"working_dir\":\"/tmp/project\",\"status\":\"Active\",\"model\":\"gpt\",\"provider_key\":\"openai\",\"updated_at\":\"2026-01-02T00:00:00Z\"}")))
    (unwind-protect
        (let ((info (jcode--read-session-info file)))
          (should (equal (jcode-session-info-id info) "s1"))
          (should (equal (jcode-session-info-title info) "Custom"))
          (should (equal (jcode-session-info-working-dir info) "/tmp/project"))
          (should (equal (jcode-session-info-model info) "gpt")))
      (delete-file file))))

(ert-deftest jcode-rename-session-file-writes-custom-title ()
  (let* ((root (make-temp-file "jcode-rename-session" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (file (expand-file-name "s1.json" root)))
    (unwind-protect
        (progn
          (write-region "{\"id\":\"s1\",\"short_name\":\"short\",\"title\":\"Generated\",\"status\":\"Closed\",\"messages\":[{\"id\":\"message_1\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hello\"}]}]}\n"
                        nil file)
          (cl-letf (((symbol-function 'jcode-refresh-session-list-buffers) #'ignore))
            (jcode-rename-session-file "s1" "Custom" root))
          (let ((info (jcode--read-session-info file)))
            (should (equal (jcode-session-info-title info) "Custom")))
          (cl-letf (((symbol-function 'jcode-refresh-session-list-buffers) #'ignore))
            (jcode-rename-session-file "s1" "" root))
          (let ((info (jcode--read-session-info file)))
            (should (equal (jcode-session-info-title info) "Generated"))))
      (delete-directory root t))))

(ert-deftest jcode-read-session-info-parses-large-transcripts ()
  (let* ((large (make-string 150000 ?x))
         (file (make-temp-file
                "jcode-large-session" nil ".json"
                (format "{\"id\":\"large\",\"messages\":[{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":%S}]}],\"working_dir\":\"/tmp/project\",\"short_name\":\"fixture\",\"status\":\"Active\",\"updated_at\":\"2026-01-02T00:00:00Z\"}"
                        large))))
    (unwind-protect
        (let ((info (jcode--read-session-info file)))
          (should info)
          (should (equal (jcode-session-info-id info) "large"))
          (should (equal (jcode-session-info-short-name info) "fixture"))
          (should (equal (jcode-session-info-status info) "Active")))
      (delete-file file))))

(ert-deftest jcode-latest-session-filters-current-directory ()
  (let* ((root (make-temp-file "jcode-sessions" t))
         (project-a (file-name-as-directory (expand-file-name "a" root)))
         (project-b (file-name-as-directory (expand-file-name "b" root)))
         (sessions (file-name-as-directory (expand-file-name "sessions" root)))
         (jcode-sessions-directory sessions))
    (unwind-protect
        (progn
          (make-directory project-a)
          (make-directory project-b)
          (make-directory sessions)
          (write-region (format "{\"id\":\"old\",\"short_name\":\"old\",\"working_dir\":%S,\"updated_at\":\"2026-01-01T00:00:00Z\"}" project-a)
                        nil (expand-file-name "old.json" sessions))
          (write-region (format "{\"id\":\"other\",\"short_name\":\"other\",\"working_dir\":%S,\"updated_at\":\"2026-01-03T00:00:00Z\"}" project-b)
                        nil (expand-file-name "other.json" sessions))
          (write-region (format "{\"id\":\"new\",\"short_name\":\"new\",\"working_dir\":%S,\"updated_at\":\"2026-01-02T00:00:00Z\"}" project-a)
                        nil (expand-file-name "new.json" sessions))
          (should (equal (jcode-session-info-id (jcode-latest-session project-a t)) "new"))
          (should (equal (jcode-session-info-id (jcode-latest-session project-a nil)) "other")))
      (delete-directory root t))))

(ert-deftest jcode-session-display-title-prefixes-active-server ()
  (let ((active (jcode--make-session-info :short-name "alpha"
                                                :status "Active"
                                                :server-name "server"))
        (closed (jcode--make-session-info :short-name "beta"
                                                :status "Closed"
                                                :server-name "server")))
    (should (equal (jcode--session-display-title active) "server alpha"))
    (should (equal (jcode--session-display-title closed) "beta"))))

(ert-deftest jcode-input-file-completion-capfs-exist ()
  (with-temp-buffer
    (jcode-input-mode)
    (should (memq #'jcode--file-reference-capf completion-at-point-functions))
    (should (memq #'jcode--path-capf completion-at-point-functions))
    (should (eq (key-binding (kbd "TAB")) #'jcode-complete))
    (should (eq (key-binding (kbd "C-c C-s")) #'jcode-steer))
    (should (eq (key-binding (kbd "@")) #'self-insert-command))
    (should (eq (key-binding (kbd "/")) #'self-insert-command))))

(ert-deftest jcode-file-reference-capf-completes-after-at ()
  (with-temp-buffer
    (jcode-input-mode)
    (insert "look @src")
    (cl-letf (((symbol-function 'jcode--project-file-candidates)
               (lambda () '("src/main.rs" "README.org"))))
      (let ((capf (jcode--file-reference-capf)))
        (should capf)
        (should (= (nth 0 capf) (1+ (save-excursion (search-backward "@") (point)))))
        (should (member "src/main.rs" (nth 2 capf)))))))

(ert-deftest jcode-project-file-discovery-runs-git-relative-to-remote-directory ()
  (let (seen-default-directory seen-args)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile buffer _display &rest args)
                 (setq seen-default-directory default-directory
                       seen-args (cons program args))
                 (if (eq buffer t)
                     (insert "src/main.rs\nREADME.org\n")
                   (with-current-buffer buffer
                     (insert "src/main.rs\nREADME.org\n")))
                 0))
              ((symbol-function 'jcode--scan-project-files)
               (lambda (_directory) (ert-fail "fallback scan should not run"))))
      (should (equal (jcode--find-project-files "/ssh:test-host:/home/me/project/")
                     '("src/main.rs" "README.org")))
      (should (equal seen-default-directory "/ssh:test-host:/home/me/project/"))
      (should (equal seen-args
                     '("git" "ls-files" "--cached" "--others" "--exclude-standard"))))))

(ert-deftest jcode-path-completions-resolve-against-remote-default-directory ()
  (let (checked-dir completion-dir)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (dir)
                 (setq checked-dir dir)
                 t))
              ((symbol-function 'file-name-all-completions)
               (lambda (base dir)
                 (should (equal base "ma"))
                 (setq completion-dir dir)
                 '("main.rs" "mod.rs"))))
      (let ((default-directory "/ssh:test-host:/home/me/project/"))
        (should (equal (jcode--path-completions "./src/ma")
                       '("./src/main.rs" "./src/mod.rs"))))
      (should (equal checked-dir "/ssh:test-host:/home/me/project/src/"))
      (should (equal completion-dir "/ssh:test-host:/home/me/project/src/")))))

(ert-deftest jcode-scan-project-files-uses-file-apis-and-excludes-heavy-dirs ()
  (let* ((root (make-temp-file "jcode-scan" t))
         (src (expand-file-name "src" root))
         (git (expand-file-name ".git" root)))
    (unwind-protect
        (progn
          (make-directory src)
          (make-directory git)
          (write-region "" nil (expand-file-name "main.rs" src))
          (write-region "" nil (expand-file-name "config" git))
          (should (equal (jcode--scan-project-files root) '("src/main.rs"))))
      (delete-directory root t))))

(ert-deftest jcode-project-buffers-finds-current-project-session ()
  (let* ((dir (file-name-as-directory default-directory))
         (buffers (jcode--make-buffers dir "project-find"))
         (chat (car buffers))
         (input (cdr buffers)))
    (unwind-protect
        (progn
          (should (memq chat (jcode-project-buffers dir)))
          (with-current-buffer chat
            (should (equal (jcode--current-buffer-pair) (cons chat input)))))
      (kill-buffer chat)
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-command-does-not-create-session-until-send ()
  (let (started displayed)
    (cl-letf (((symbol-function 'jcode--project-directory)
               (lambda () default-directory))
              ((symbol-function 'jcode--display-buffers)
               (lambda (chat input) (setq displayed (cons chat input))))
              ((symbol-function 'jcode-session-start)
               (lambda (&rest _args) (setq started t))))
      (let ((chat (jcode)))
        (unwind-protect
            (progn
              (should (buffer-live-p chat))
              (should displayed)
              (should-not started)
              (should-not (buffer-local-value 'jcode--session chat)))
          (kill-buffer chat)
          (when (buffer-live-p (cdr displayed))
            (kill-buffer (cdr displayed))))))))

(ert-deftest jcode-send-starts-lazy-native-session-and-sends-message ()
  (let* ((chat (generate-new-buffer " *jcode-test-lazy-chat*"))
         (input (generate-new-buffer " *jcode-test-lazy-input*"))
         opened sent)
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "hello lazy")
            (cl-letf (((symbol-function 'jcode-native-open-session)
                       (lambda (session-id cwd start-chat start-input)
                         (setq opened (list session-id cwd start-chat start-input))
                         'lazy-native-connection))
                      ((symbol-function 'jcode-native-message)
                       (lambda (connection text) (setq sent (list connection text)))))
              (jcode-send)
              (should opened)
              (should (null (nth 0 opened)))
              (should (equal (nth 2 opened) chat))
              (should (equal (nth 3 opened) input))
              (should (equal sent '(lazy-native-connection "hello lazy")))
              (should (string-empty-p (buffer-string))))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-current-buffer-pair-detects-chat-and-input ()
  (let* ((dir default-directory)
         (buffers (jcode--make-buffers dir "pair-test"))
         (chat (car buffers))
         (input (cdr buffers)))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (should (equal (jcode--current-buffer-pair) (cons chat input))))
          (with-current-buffer input
            (should (equal (jcode--current-buffer-pair) (cons chat input)))))
      (kill-buffer chat)
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-reconnect-reuses-current-buffers-and-forces-native-takeover ()
  (let* ((dir default-directory)
         (buffers (jcode--make-buffers dir "reconnect-test"))
         (chat (car buffers))
         (input (cdr buffers))
         (old (jcode--make-native-connection :chat chat :input input
                                             :session-id "reconnect-test"
                                             :cwd dir))
         closed opened takeover)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode--display-buffers) #'ignore)
                  ((symbol-function 'jcode-native-close)
                   (lambda (connection) (setq closed connection)))
                  ((symbol-function 'jcode-native-open-session)
                   (lambda (session-id cwd open-chat open-input)
                     (setq opened (list session-id cwd open-chat open-input)
                           takeover jcode-native-take-over-active-session))))
          (with-current-buffer chat
            (setq jcode--display-session-id "reconnect-test"
                  jcode--native-connection old)
            (jcode-reconnect))
          (should (eq closed old))
          (should takeover)
          (should (equal opened (list "reconnect-test" dir chat input))))
      (kill-buffer chat)
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-native-open-process-remote-starts-socat-bridge ()
  (let ((started-default-directory nil)
        (started-args nil)
        coding noquery)
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (_name _buffer program &rest args)
                 (setq started-default-directory default-directory
                       started-args (cons program args))
                 (let ((default-directory "/"))
                   (start-process "jcode-test-cat" nil "cat"))))
              ((symbol-function 'set-process-coding-system)
               (lambda (_proc in out) (setq coding (list in out))))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (_proc flag) (setq noquery (not flag)))))
      (let ((proc (jcode-native--open-process
                   "/ssh:test-host:/tmp/project/"
                   "/ssh:test-host:/run/user/1000/jcode.sock")))
        (unwind-protect
            (progn
              (should (processp proc))
              (should (equal started-default-directory "/ssh:test-host:/tmp/project/"))
              (should (equal (car started-args) jcode-native-remote-bridge-program))
              (should (equal (cdr started-args)
                             '("-" "UNIX-CONNECT:/run/user/1000/jcode.sock")))
              (should (equal coding '(utf-8-emacs-unix utf-8-emacs-unix)))
              (should noquery))
          (when (process-live-p proc)
            (delete-process proc)))))))

(ert-deftest jcode-native-open-process-local-uses-unix-socket ()
  (let (network-args started-file)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (setq network-args args)
                 (let ((default-directory "/"))
                   (start-process "jcode-test-cat" nil "cat"))))
              ((symbol-function 'start-file-process)
               (lambda (&rest _args) (setq started-file t))))
      (let ((proc (jcode-native--open-process "/tmp/project/" "/run/user/1000/jcode.sock")))
        (unwind-protect
            (progn
              (should (processp proc))
              (should-not started-file)
              (should (eq (plist-get network-args :family) 'local))
              (should (equal (plist-get network-args :service)
                             "/run/user/1000/jcode.sock")))
          (when (process-live-p proc)
            (delete-process proc)))))))

(ert-deftest jcode-native-socket-path-uses-remote-uid-from-file-attributes ()
  (cl-letf (((symbol-function 'jcode--servers-file)
             (lambda (_directory) "/ssh:test-host:/missing/servers.json"))
            ((symbol-function 'file-readable-p) (lambda (_file) nil))
            ((symbol-function 'file-attributes)
             (lambda (file &optional id-format)
               (should (equal file "/ssh:test-host:/tmp/project/"))
               (should (eq id-format 'integer))
               ;; See `file-attributes': element 2 is numeric UID when
               ;; ID-FORMAT is `integer'.
               (list nil nil 4242))))
    (should (equal (jcode-native-socket-path "/ssh:test-host:/tmp/project/")
                   "/ssh:test-host:/run/user/4242/jcode.sock"))))

(ert-deftest jcode-native-host-local-socket-path-strips-tramp-prefix ()
  (should (equal (jcode-native--host-local-socket-path
                  "/ssh:test-host:/run/user/4242/jcode.sock"
                  "/ssh:test-host:/tmp/project/")
                 "/run/user/4242/jcode.sock")))

(ert-deftest jcode-cycle-reasoning-effort-wraps-xhigh-to-none ()
  (let* ((chat (generate-new-buffer " *jcode-test-reasoning-chat*"))
         (input (generate-new-buffer " *jcode-test-reasoning-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "reasoning-test" :cwd default-directory))
         requested)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-set-reasoning-effort)
                   (lambda (_connection effort) (setq requested effort))))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input
                  jcode--native-connection connection
                  jcode--display-reasoning-effort "xhigh")
            (jcode-cycle-reasoning-effort))
          (should (equal requested "none")))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-select-reasoning-effort-sends-selected-effort ()
  (let* ((chat (generate-new-buffer " *jcode-test-select-reasoning-chat*"))
         (input (generate-new-buffer " *jcode-test-select-reasoning-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "reasoning-test" :cwd default-directory))
         requested)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-set-reasoning-effort)
                   (lambda (_connection effort) (setq requested effort)))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input
                  jcode--native-connection connection
                  jcode--display-reasoning-effort "low"))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (jcode-select-reasoning-effort "high"))
          (should (equal requested "high"))
          (with-current-buffer input
            (should (equal jcode--display-reasoning-effort "high"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-header-reasoning-click-opens-selector ()
  (should (eq (lookup-key jcode--header-reasoning-map [header-line mouse-1])
              #'jcode-select-reasoning-effort)))

(ert-deftest jcode-short-commands-replace-emacs-prefixed-functions ()
  (dolist (command '(jcode jcode-resume jcode-current jcode-list jcode-plan
	                     jcode-connect jcode-reconnect jcode-attach
	                     jcode-send jcode-cancel jcode-disconnect
	                     jcode-select-reasoning-effort jcode-select-fast-mode))
    (should (commandp command)))
  (dolist (old '(emacs-jcode emacs-jcode-resume emacs-jcode-current
                 emacs-jcode-list emacs-jcode-plan emacs-jcode-send
                 emacs-jcode-cancel emacs-jcode-disconnect))
    (should-not (fboundp old)))
  (fset 'emacs-jcode-stale-test (lambda () t))
  (should (fboundp 'emacs-jcode-stale-test))
  (jcode--undefine-old-emacs-prefixed-functions)
  (should-not (fboundp 'emacs-jcode-stale-test)))

(ert-deftest jcode-list-refresh-is-command ()
  (should (commandp #'jcode-list-refresh)))

(ert-deftest jcode-resume-reuses-existing-session-buffer-pair ()
  (let* ((dir default-directory)
         (buffers (jcode--make-buffers dir "s-reuse"))
         (chat (car buffers))
         (input (cdr buffers))
         opened displayed)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq jcode--display-session-id "s-reuse"))
          (cl-letf (((symbol-function 'jcode-native-open-session)
                     (lambda (&rest _args) (setq opened t)))
                    ((symbol-function 'jcode--display-buffers)
                     (lambda (shown-chat shown-input)
                       (setq displayed (cons shown-chat shown-input)))))
            (should (eq (jcode-resume "s-reuse") chat))
            (should (equal displayed (cons chat input)))
            (should-not opened)))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input)))))

(ert-deftest jcode-menu-and-slash-commands-are-wired ()
  (should (commandp #'jcode-menu))
  (should (eq (lookup-key jcode-input-mode-map (kbd "C-c C-p")) #'jcode-menu))
  (should (eq (lookup-key jcode-chat-mode-map (kbd "C-c C-p")) #'jcode-menu))
  (dolist (command '(jcode-select-compaction-mode jcode-select-transport
                     jcode-select-premium-mode jcode-select-memory-state
                     jcode-select-swarm-state jcode-select-review-state
                     jcode-select-judge-state))
    (should (commandp command)))
  (dolist (command '("/?" "/help" "/compact" "/clear" "/split" "/transfer"
                     "/memory" "/extract-memory" "/swarm" "/review" "/judge"
                     "/model" "/reasoning" "/fast" "/transport" "/premium"
                     "/z" "/zz" "/zzz" "/sessions" "/compaction-mode"))
    (should (assoc command jcode-slash-commands))))

(ert-deftest jcode-slash-command-capf-completes-slash-commands ()
  (with-temp-buffer
    (jcode-input-mode)
    (insert "/co")
    (let ((capf (jcode--slash-command-capf)))
      (should capf)
      (pcase-let ((`(,start ,end ,candidates . ,_) capf))
        (should (= start (point-min)))
 	        (should (= end (point-max)))
	        (should (member "/compact" candidates))
	        (should (member "/compaction-mode" candidates))))))

(ert-deftest jcode-slash-command-capf-completes-bare-slash ()
  (with-temp-buffer
    (jcode-input-mode)
    (insert "/")
    (let ((capf (jcode--slash-command-capf)))
      (should capf)
      (pcase-let ((`(,start ,end ,candidates . ,_) capf))
        (should (= start (point-min)))
        (should (= end (point-max)))
        (should (member "/?" candidates))
        (should (member "/transport" candidates))))))

(ert-deftest jcode-slash-command-capf-installs-in-live-input-buffers ()
  (with-temp-buffer
    (jcode-input-mode)
    (setq-local completion-at-point-functions '(jcode--path-capf jcode--file-reference-capf))
    (jcode--install-slash-command-capf-in-live-buffers)
    (should (eq (car completion-at-point-functions) #'jcode--slash-command-capf))))

(ert-deftest jcode-input-slash-command-recognizes-help-command ()
  (should (equal (jcode--input-slash-command "/?") "/?")))

(ert-deftest jcode-menu-knobs-send-native-protocol-requests ()
  (let ((chat (generate-new-buffer " *jcode-test-menu-chat*"))
        (input (generate-new-buffer " *jcode-test-menu-input*"))
        sent)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-request-current)
                   (lambda (type &rest fields) (push (cons type fields) sent))))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (jcode-select-transport "websocket")
	            (jcode-select-premium-mode "zero")
	            (jcode-set-feature "memory" t)
	            (jcode-select-feature-state "swarm" "off")
                    (cl-letf (((symbol-function 'jcode-refresh-session-list-buffers) #'ignore))
                      (jcode-rename-session "Renamed Session")))
	          (should (member '("set_transport" :transport "websocket") sent))
	          (should (member '("set_premium_mode" :mode 2) sent))
	          (should (member '("set_feature" :feature "memory" :enabled t) sent))
	          (should (member '("set_feature" :feature "swarm" :enabled nil) sent))
                  (should (member '("rename_session" :title "Renamed Session") sent))
	          (with-current-buffer chat
	            (should (equal jcode--display-transport "websocket"))
	            (should (equal jcode--display-premium-mode "zero"))
	            (should-not (equal jcode--display-title "Renamed Session"))
	            (should-not (string-match-p "Renamed Session" (buffer-name)))
	            (should (equal (cdr (assoc "memory" jcode--display-feature-states)) "on"))
            (should (equal (cdr (assoc "swarm" jcode--display-feature-states)) "off"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-menu-state-descriptions-show-current-values ()
  (let ((chat (generate-new-buffer " *jcode-test-menu-state-chat*"))
        (input (generate-new-buffer " *jcode-test-menu-state-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input)
            (jcode--set-display-metadata
             chat
             :reasoning-effort "low"
             :service-tier "priority"
             :transport "websocket"
             :premium-mode "one"
             :compaction-mode "semantic"
             :feature-states '(("memory" . "on") ("swarm" . "off"))))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (should (equal (jcode--menu-desc-effort) "effort: low"))
            (should (equal (jcode--menu-desc-fast) "fast: priority"))
            (should (equal (jcode--menu-desc-transport) "transport: websocket"))
            (should (equal (jcode--menu-desc-premium) "premium: one"))
            (should (equal (jcode--menu-desc-compaction) "compaction: semantic"))
            (should (equal (jcode--menu-desc-memory) "memory: on"))
            (should (equal (jcode--menu-desc-swarm) "swarm: off"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-menu-feature-description-handles-unknown-state ()
  (let ((chat (generate-new-buffer " *jcode-test-menu-unknown-chat*"))
        (input (generate-new-buffer " *jcode-test-menu-unknown-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--input-buffer input))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (should (equal (jcode--menu-desc-memory) "memory: default"))
            (should (equal (jcode--menu-desc-transport) "transport: default"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-menu-default-descriptions-and-setters-are-separate ()
  (let ((jcode-default-model nil)
        (jcode-default-reasoning-effort nil)
        (jcode-default-service-tier nil)
        (jcode-default-transport nil)
        (jcode-default-premium-mode nil)
        (jcode-default-compaction-mode nil)
        (jcode-default-feature-states nil))
    (should (equal (jcode--menu-desc-default-effort) "default effort: daemon"))
    (jcode-set-default-reasoning-effort "high")
    (jcode-set-default-fast-mode "priority")
    (jcode-set-default-transport "websocket")
    (jcode-set-default-premium-mode "zero")
    (jcode-set-default-compaction-mode "semantic")
    (jcode-set-default-feature-state "memory" "on")
    (should (equal jcode-default-reasoning-effort "high"))
    (should (equal (jcode--menu-desc-default-fast) "default fast: priority"))
    (should (equal (jcode--menu-desc-default-transport) "default transport: websocket"))
    (should (equal (jcode--menu-desc-default-premium) "default premium: zero"))
    (should (equal (jcode--menu-desc-default-compaction) "default compaction: semantic"))
    (should (equal (jcode--menu-desc-default-features) "default features: memory=on"))))

(ert-deftest jcode-send-executes-known-slash-command-locally ()
  (let ((chat (generate-new-buffer " *jcode-test-slash-chat*"))
        executed)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-execute-slash-command)
                   (lambda (command) (setq executed command))))
          (with-current-buffer chat (jcode-chat-mode))
          (with-temp-buffer
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "/compact")
            (jcode-send)
            (should (equal executed "/compact"))
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest jcode-list-mode-has-standard-mark-keybindings ()
  (with-temp-buffer
    (jcode-list-mode)
    (should (eq (key-binding (kbd "m")) #'jcode-list-mark))
    (should (eq (key-binding (kbd "*")) #'jcode-list-mark))
    (should (eq (key-binding (kbd "u")) #'jcode-list-unmark))
    (should (eq (key-binding (kbd "DEL")) #'jcode-list-unmark-backward))
    (should (eq (key-binding (kbd "U")) #'jcode-list-unmark-all))
    (should (eq (key-binding (kbd "t")) #'jcode-list-toggle-mark))
    (should (eq (key-binding (kbd "x")) #'jcode-list-delete-marked-sessions))
    (should (eq (key-binding (kbd "D")) #'jcode-list-delete-marked-sessions))
    (should (eq (key-binding (kbd "R")) #'jcode-list-rename-session))))

(ert-deftest jcode-send-uses-native-connection-when-present ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-send-chat*"))
         (input (generate-new-buffer " *jcode-test-native-send-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp"))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-message)
                   (lambda (conn text)
                     (setq sent (list conn text)))))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--native-connection connection))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "hello native")
            (jcode-send)
            (should (equal (buffer-string) "")))
          (should (equal sent (list connection "hello native")))
          (with-current-buffer chat
            (should (string-match-p "hello native" (buffer-string)))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-send-queues-native-followup-when-busy ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-queue-chat*"))
         (input (generate-new-buffer " *jcode-test-native-queue-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp" :busy t))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-message)
                   (lambda (_conn _text) (setq sent t)))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--native-connection connection))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "queued followup")
            (jcode-send)
            (should (equal (buffer-string) "")))
          (should-not sent)
          (should (equal (jcode-native-connection-followup-queue connection)
                         '("queued followup"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-steer-sends-native-soft-interrupt-when-busy ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-steer-chat*"))
         (input (generate-new-buffer " *jcode-test-native-steer-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp" :busy t))
         steered)
    (unwind-protect
        (cl-letf (((symbol-function 'jcode-native-steer)
                   (lambda (conn text) (setq steered (list conn text))))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--native-connection connection))
          (with-current-buffer input
            (jcode-input-mode)
            (setq jcode--chat-buffer chat)
            (insert "change course")
            (jcode-steer)
            (should (equal (buffer-string) "")))
          (should (equal steered (list connection "change course"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-session-status-string-handles-structured-status ()
  (should (equal (jcode--session-status-string "Active") "live"))
  (should (equal (jcode--session-status-string "Closed") "closed"))
  (should (equal (jcode--session-status-string
                  '((Crashed (message . "Terminal or window closed (SIGHUP)"))))
                 "Crashed: Terminal or window closed (SIGHUP)"))
  (let ((info (jcode--make-session-info
               :id "s-crash"
               :short-name "broken"
               :status '((Crashed (message . "Terminal or window closed (SIGHUP)"))))))
    (should (equal (aref (cadr (jcode--session-list-entry info)) 1)
                   "Crashed: Terminal or window closed (SIGHUP)"))))

(ert-deftest jcode-session-list-entry-uses-relative-age-and-project ()
  (let* ((now (date-to-time "2026-06-26T12:00:00Z"))
         (info (jcode--make-session-info
                :id "session_shrimp_1234567890"
                :short-name "shrimp"
                :status "Closed"
                :model "sonnet"
                :user-turn-count 7
                :updated-at "2026-06-26T09:30:00Z"
                :working-dir "/home/arthur/src/emacs-jcode")))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (let ((row (cadr (jcode--session-list-entry info))))
        (should (= (length row) 8))
        (should (equal (aref row 0) "shrimp"))
        (should (equal (aref row 2) ""))
        (should (equal (aref row 3) ""))
        (should (equal (aref row 5) "7"))
        (should (equal (aref row 6) "2h ago"))
        (should (equal (aref row 7) "emacs-jcode"))
        (should-not (seq-some (lambda (cell)
                                (and (stringp cell)
                                     (string-match-p "session_shrimp" cell)))
                              row))))))

(ert-deftest jcode-relative-time-string-formats-common-ages ()
  (let ((now (date-to-time "2026-06-26T12:00:00Z")))
    (should (equal (jcode--relative-time-string "2026-06-26T11:59:40Z" now)
                   "just now"))
    (should (equal (jcode--relative-time-string "2026-06-26T11:20:00Z" now)
                   "40m ago"))
    (should (equal (jcode--relative-time-string "2026-06-25T09:00:00Z" now)
                   "1d ago"))
    (should (equal (jcode--relative-time-string nil now) ""))))

(ert-deftest jcode-empty-session-detection-hides-system-only-sessions ()
  (let* ((root (make-temp-file "jcode-empty-sessions" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (system-file (expand-file-name "empty.json" root))
         (real-file (expand-file-name "real.json" root))
         (saved-file (expand-file-name "saved.json" root))
         (zero-file (expand-file-name "zero.json" root)))
    (unwind-protect
        (progn
          (write-region
           "{\"id\":\"empty\",\"saved\":false,\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"display_role\":\"system\",\"content\":[]}]}"
           nil system-file)
          (write-region
           "{\"id\":\"real\",\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]},{\"role\":\"assistant\",\"content\":\"hello\"}]}"
           nil real-file)
          (write-region
           "{\"id\":\"saved\",\"saved\":true,\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"display_role\":\"system\",\"content\":[]}]}"
           nil saved-file)
          (write-region
           "{\"id\":\"zero\",\"status\":\"Active\",\"updated_at\":\"2026\",\"messages\":[]}"
           nil zero-file)
	  (let ((ids (mapcar #'jcode-session-info-id (jcode-list-sessions-data root))))
	    (should-not (member "empty" ids))
	    (should (member "zero" ids))
	    (should (member "real" ids))
	    (should (member "saved" ids)))
          (let ((jcode-hide-empty-sessions nil))
            (should (equal (jcode-session-info-conversation-count
                            (jcode--read-session-info real-file))
                           2))
            (should (equal (jcode-session-info-user-turn-count
                            (jcode--read-session-info real-file))
                           1))
            (should (jcode--session-empty-p (jcode--read-session-info system-file)))
            (should (jcode--session-empty-p (jcode--read-session-info zero-file)))
            (should-not (jcode--session-empty-p (jcode--read-session-info real-file)))
            (should-not (jcode--session-empty-p (jcode--read-session-info saved-file)))))
	      (delete-directory root t))))

(ert-deftest jcode-apply-session-info-includes-provider ()
  (let* ((root (make-temp-file "jcode-provider-info" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (chat (generate-new-buffer " *jcode-test-provider-chat*"))
         (input (generate-new-buffer " *jcode-test-provider-input*")))
    (unwind-protect
        (progn
          (write-region
           "{\"id\":\"s1\",\"status\":\"Active\",\"model\":\"gpt-5.5\",\"provider_key\":\"openai\",\"messages\":[]}"
           nil (expand-file-name "s1.json" root))
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (jcode-apply-session-info-to-buffers "s1" chat input)
          (with-current-buffer input
            (should (equal jcode--display-provider "openai"))
            (should (equal jcode--display-model "gpt-5.5"))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (buffer-live-p input) (kill-buffer input))
      (delete-directory root t))))

(ert-deftest jcode-user-turn-count-counts-only-user-text-sends ()
  (let* ((root (make-temp-file "jcode-user-turn-count" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (file (expand-file-name "turns.json" root)))
    (unwind-protect
        (progn
          (write-region
           (concat
            "{\"id\":\"turns\",\"status\":\"Closed\",\"messages\":["
            "{\"role\":\"user\",\"display_role\":\"system\",\"content\":[{\"type\":\"text\",\"text\":\"system reminder\"}]},"
            "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"first prompt\"}]},"
            "{\"role\":\"assistant\",\"content\":\"ok\"},"
            "{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_call_id\":\"t1\",\"content\":\"out\"}]},"
            "{\"role\":\"user\",\"display_role\":\"background_task\",\"content\":[{\"type\":\"text\",\"text\":\"background completed\"}]},"
            "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"second prompt\"}]}"
            "]}")
           nil file)
          (let ((info (jcode--read-session-info file)))
            (should (equal (jcode-session-info-user-turn-count info) 2))))
      (delete-directory root t))))

(ert-deftest jcode-session-list-uses-live-rendered-user-count-when-open ()
  (let* ((root (make-temp-file "jcode-live-turn-count" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (chat (generate-new-buffer " *jcode-test-live-turn-chat*")))
    (unwind-protect
        (progn
          (write-region
           "{\"id\":\"live\",\"status\":\"Active\",\"messages\":[{\"role\":\"user\",\"display_role\":\"system\",\"content\":[{\"type\":\"text\",\"text\":\"system\"}]}]}"
           nil (expand-file-name "live.json" root))
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--display-session-id "live")
            (jcode-render-user chat "actual prompt"))
          (let ((info (jcode--session-info-by-id "live" root)))
            (should (equal (jcode-session-info-user-turn-count info) 1))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory root t))))

(ert-deftest jcode-prune-empty-sessions-deletes-only-closed-empty-files ()
  (let* ((root (make-temp-file "jcode-prune-sessions" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (closed-empty (expand-file-name "closed-empty.json" root))
         (active-empty (expand-file-name "active-empty.json" root))
         (closed-real (expand-file-name "closed-real.json" root)))
    (unwind-protect
        (progn
          (write-region
           "{\"id\":\"closed-empty\",\"saved\":false,\"status\":\"Closed\",\"messages\":[{\"role\":\"user\",\"display_role\":\"system\",\"content\":[]}]}"
           nil closed-empty)
          (write-region
           "{\"id\":\"active-empty\",\"status\":\"Active\",\"messages\":[{\"role\":\"user\",\"display_role\":\"system\",\"content\":[]}]}"
           nil active-empty)
          (write-region
           "{\"id\":\"closed-real\",\"status\":\"Closed\",\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}]}"
           nil closed-real)
          (jcode-prune-empty-sessions root)
          (should-not (file-exists-p closed-empty))
          (should (file-exists-p active-empty))
          (should (file-exists-p closed-real)))
      (delete-directory root t))))

(ert-deftest jcode-list-mark-unmark-and-open-marked-sessions ()
  (let* ((root (make-temp-file "jcode-list-marks" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (buf (generate-new-buffer " *jcode-test-list-marks*"))
         opened)
    (unwind-protect
        (progn
          (write-region
           "{\"id\":\"one\",\"short_name\":\"one\",\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"
           nil (expand-file-name "one.json" root))
          (write-region
           "{\"id\":\"two\",\"short_name\":\"two\",\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"
           nil (expand-file-name "two.json" root))
          (with-current-buffer buf
            (jcode-list-mode)
            (setq jcode--session-list-directory root)
            (jcode-list-refresh)
            (goto-char (point-min))
            (search-forward "one")
            (beginning-of-line)
            (jcode-list-mark)
            (should (equal (jcode-list-marked-session-ids) '("one")))
            (forward-line -1)
            (jcode-list-toggle-mark)
            (should-not (jcode-list-marked-session-ids))
            (forward-line -1)
            (jcode-list-toggle-mark)
            (should (equal (jcode-list-marked-session-ids) '("one")))
            (forward-line -1)
            (jcode-list-unmark)
            (should-not (jcode-list-marked-session-ids))
            (goto-char (point-min))
            (search-forward "one")
            (beginning-of-line)
            (jcode-list-mark)
            (goto-char (point-min))
            (search-forward "two")
            (beginning-of-line)
            (jcode-list-mark)
            (should (equal (sort (copy-sequence (jcode-list-marked-session-ids)) #'string<)
                           '("one" "two")))
            (cl-letf (((symbol-function 'message) #'ignore))
              (jcode-list-unmark-all))
            (should-not (jcode-list-marked-session-ids))
            (goto-char (point-min))
            (search-forward "one")
            (beginning-of-line)
            (jcode-list-mark)
            (goto-char (point-min))
            (search-forward "two")
            (beginning-of-line)
            (jcode-list-mark)
            (cl-letf (((symbol-function 'jcode-resume)
                       (lambda (id replay) (push (list id replay) opened)))
                      ((symbol-function 'message) #'ignore))
              (jcode-list-open-marked-sessions)
              (should (equal (sort (nreverse opened) (lambda (a b) (string< (car a) (car b))))
                             '(("one" t) ("two" t)))))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest jcode-list-delete-current-and-marked-session-files ()
  (let* ((root (make-temp-file "jcode-list-delete" t))
         (jcode-sessions-directory (file-name-as-directory root))
         (buf (generate-new-buffer " *jcode-test-list-delete*"))
         (one (expand-file-name "one.json" root))
         (two (expand-file-name "two.json" root))
         (three (expand-file-name "three.json" root)))
    (unwind-protect
        (progn
          (dolist (spec `((,one . "one") (,two . "two") (,three . "three")))
            (write-region
             (format "{\"id\":\"%s\",\"short_name\":\"%s\",\"status\":\"Closed\",\"updated_at\":\"2026\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"
                     (cdr spec) (cdr spec))
             nil (car spec)))
          (with-current-buffer buf
            (jcode-list-mode)
            (setq jcode--session-list-directory root)
            (jcode-list-refresh)
            (goto-char (point-min))
            (search-forward "one")
            (beginning-of-line)
            (cl-letf (((symbol-function 'message) #'ignore))
              (jcode-list-delete-session))
            (should-not (file-exists-p one))
            (should (file-exists-p two))
            (goto-char (point-min))
            (search-forward "two")
            (beginning-of-line)
            (jcode-list-mark)
            (goto-char (point-min))
            (search-forward "three")
            (beginning-of-line)
            (jcode-list-mark)
            (cl-letf (((symbol-function 'message) #'ignore))
              (jcode-list-delete-marked-sessions))
            (should-not (file-exists-p two))
            (should-not (file-exists-p three))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest jcode-session-list-entry-shows-location-and-live-client ()
  (let* ((chat (generate-new-buffer " *jcode-test-list-client-chat*"))
         (info (jcode--make-session-info
                :id "s-list" :short-name "list-test" :status "Active"
                :model "gpt-test" :updated-at "2026-06-26T14:00:00Z"
                :working-dir "/tmp/project")))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (jcode-chat-mode)
            (setq jcode--display-session-id "s-list")
            (jcode--set-display-metadata
             chat :owner 'owned :client-count 2 :connection-type "native"))
          (jcode--annotate-session-runtime (list info) "/ssh:example:/tmp/project/")
          (let ((columns (cadr (jcode--session-list-entry info))))
            (should (equal (aref columns 2) "remote ssh:example"))
            (should (equal (aref columns 3) "Emacs owned x2 native"))))
      (kill-buffer chat))))

(ert-deftest jcode-native-json-read-parses-history ()
  (let ((event (jcode-native--json-read
                "{\"type\":\"history\",\"provider_model\":\"gpt\",\"messages\":[{\"role\":\"assistant\",\"content\":\"hello\"}]}")))
    (should (equal (alist-get 'type event) "history"))
    (should (equal (alist-get 'provider_model event) "gpt"))
    (should (equal (alist-get 'content (aref (alist-get 'messages event) 0)) "hello"))))

(ert-deftest jcode-native-stream-events-mark-connection-busy ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-busy-chat*"))
         (input (generate-new-buffer " *jcode-test-native-busy-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (jcode-native--handle-event connection '((type . "text_delta") (text . "hello")))
          (should (jcode-native-connection-busy connection))
          (jcode-native--handle-event connection '((type . "done") (id . 1)))
          (should-not (jcode-native-connection-busy connection)))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-native-history-renders-messages-and-metadata ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-chat*"))
         (input (generate-new-buffer " *jcode-test-native-input*"))
         (connection (jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp")))
    (unwind-protect
        (progn
          (with-current-buffer chat (jcode-chat-mode))
          (with-current-buffer input (jcode-input-mode))
          (jcode-native--render-history
           connection
           '((type . "history")
             (server_name . "garden")
             (provider_model . "gpt-test")
             (client_count . 3)
             (connection_type . "websocket")
             (messages . [((role . "user") (content . "hi"))
                          ((role . "assistant") (content . "hello"))])))
          (with-current-buffer chat
            (should (string-match-p "hi" (buffer-string)))
            (should (string-match-p "hello" (buffer-string)))
            (should (equal jcode--display-model "gpt-test"))
            (should (equal jcode--display-client-count 3))
            (should (equal jcode--display-connection-type "websocket"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest jcode-killing-chat-kills-input ()
  (let* ((dir default-directory)
         (buffers (jcode--make-buffers dir "linked-test"))
         (chat (car buffers))
         (input (cdr buffers)))
    (should (buffer-live-p chat))
    (should (buffer-live-p input))
    (kill-buffer chat)
    (should-not (buffer-live-p chat))
    (should-not (buffer-live-p input))))

(provide 'jcode-test)
;;; jcode-test.el ends here
