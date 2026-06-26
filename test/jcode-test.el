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
          (should (assq 'default face-remapping-alist))
          (should (equal (cadr (assq 'default face-remapping-alist)) 'jcode-text-face))
          (should (equal (cadr (assq 'font-lock-comment-face face-remapping-alist))
                         'jcode-dim-face))
          (should (equal (face-attribute 'jcode-assistant-face :foreground nil t)
                         "#81c784"))
          (should (equal (face-attribute 'jcode-user-face :foreground nil t)
                         "#8ab4f8")))
      (kill-buffer chat))))

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
            (should (string-match-p "collapse tool output" (buffer-string)))))
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
                              "{\"id\":\"s1\",\"title\":\"Title\",\"short_name\":\"short\",\"working_dir\":\"/tmp/project\",\"status\":\"Active\",\"model\":\"gpt\",\"provider_key\":\"openai\",\"updated_at\":\"2026-01-02T00:00:00Z\"}")))
    (unwind-protect
        (let ((info (jcode--read-session-info file)))
          (should (equal (jcode-session-info-id info) "s1"))
          (should (equal (jcode-session-info-title info) "Title"))
          (should (equal (jcode-session-info-working-dir info) "/tmp/project"))
          (should (equal (jcode-session-info-model info) "gpt")))
      (delete-file file))))

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

(ert-deftest jcode-short-commands-replace-emacs-prefixed-functions ()
  (dolist (command '(jcode jcode-resume jcode-current jcode-list jcode-plan
                     jcode-connect jcode-reconnect jcode-attach
                     jcode-send jcode-cancel jcode-disconnect))
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
  (should (equal (jcode--session-status-string
                  '((Crashed (message . "Terminal or window closed (SIGHUP)"))))
                 "Crashed: Terminal or window closed (SIGHUP)"))
  (let ((info (jcode--make-session-info
               :id "s-crash"
               :short-name "broken"
               :status '((Crashed (message . "Terminal or window closed (SIGHUP)"))))))
    (should (equal (aref (cadr (jcode--session-list-entry info)) 1)
                   "Crashed: Terminal or window closed (SIGHUP)"))))

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
             (messages . [((role . "user") (content . "hi"))
                          ((role . "assistant") (content . "hello"))])))
          (with-current-buffer chat
            (should (string-match-p "hi" (buffer-string)))
            (should (string-match-p "hello" (buffer-string)))
            (should (equal jcode--display-model "gpt-test"))))
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
