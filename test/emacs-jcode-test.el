;;; emacs-jcode-test.el --- Tests for emacs-jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'emacs-jcode)

(ert-deftest emacs-jcode-acp-prompt-params-use-content-blocks ()
  (let ((session (emacs-jcode--make-session :id "s1" :cwd "/tmp")))
    (should (equal (emacs-jcode--acp-prompt-params session "hello")
                   '(:sessionId "s1" :prompt [(:type "text" :text "hello")])))))

(ert-deftest emacs-jcode-acp-load-params-include-cwd ()
  (let ((session (emacs-jcode--make-session :cwd "/tmp/project")))
    (should (equal (emacs-jcode--acp-load-params session "abc")
                   '(:sessionId "abc" :cwd "/tmp/project")))))

(ert-deftest emacs-jcode-render-session-update-agent-message ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (session (emacs-jcode--make-session :id "s1" :chat-buffer chat)))
    (unwind-protect
        (progn
          (with-current-buffer chat (emacs-jcode-chat-mode))
          (emacs-jcode-handle-notification
           session "session/update"
           '((sessionId . "s1")
             (update . ((sessionUpdate . "agent_message_chunk")
                        (content . ((type . "text") (text . "hello")))))))
          (with-current-buffer chat
            (should (string-match-p "hello" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest emacs-jcode-render-session-update-user-message ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (session (emacs-jcode--make-session :id "s1" :chat-buffer chat)))
    (unwind-protect
        (progn
          (with-current-buffer chat (emacs-jcode-chat-mode))
          (emacs-jcode-handle-notification
           session "session/update"
           '((sessionId . "s1")
             (update . ((sessionUpdate . "user_message_chunk")
                        (content . ((type . "text") (text . "hi")))))))
          (with-current-buffer chat
            (should (string-match-p "You" (buffer-string)))
            (should (string-match-p "hi" (buffer-string)))))
      (kill-buffer chat))))

(ert-deftest emacs-jcode-sanitize-text-strips-terminal-controls ()
  (should (equal (emacs-jcode--sanitize-text "\033]0;title\ahello\033[31m red\033[0m")
                 "hello red"))
  (should (equal (emacs-jcode--sanitize-text "]0;title\aTests pass")
                 "Tests pass")))

(ert-deftest emacs-jcode-input-history-roundtrip ()
  (with-temp-buffer
    (emacs-jcode-input-mode)
    (insert "first")
    (emacs-jcode--history-add (buffer-string))
    (delete-region (point-min) (point-max))
    (insert "second")
    (emacs-jcode--history-add (buffer-string))
    (delete-region (point-min) (point-max))
    (emacs-jcode-previous-input)
    (should (equal (buffer-string) "second"))
    (emacs-jcode-previous-input)
    (should (equal (buffer-string) "first"))
    (emacs-jcode-next-input)
    (should (equal (buffer-string) "second"))))

(ert-deftest emacs-jcode-send-errors-while-busy ()
  (let* ((chat (generate-new-buffer " *jcode-test-chat*"))
         (input (generate-new-buffer " *jcode-test-input*"))
         (session (emacs-jcode--make-session :id "s1"
                                             :cwd "/tmp"
                                             :chat-buffer chat
                                             :input-buffer input
                                             :busy t)))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (emacs-jcode-chat-mode)
            (setq emacs-jcode--session session))
          (with-current-buffer input
            (emacs-jcode-input-mode)
            (setq emacs-jcode--chat-buffer chat)
            (insert "hello")
            (should-error (emacs-jcode-send) :type 'user-error)))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest emacs-jcode-read-session-info-parses-metadata ()
  (let ((file (make-temp-file "emacs-jcode-session" nil ".json"
                              "{\"id\":\"s1\",\"title\":\"Title\",\"short_name\":\"short\",\"working_dir\":\"/tmp/project\",\"status\":\"Active\",\"model\":\"gpt\",\"provider_key\":\"openai\",\"updated_at\":\"2026-01-02T00:00:00Z\"}")))
    (unwind-protect
        (let ((info (emacs-jcode--read-session-info file)))
          (should (equal (emacs-jcode-session-info-id info) "s1"))
          (should (equal (emacs-jcode-session-info-title info) "Title"))
          (should (equal (emacs-jcode-session-info-working-dir info) "/tmp/project"))
          (should (equal (emacs-jcode-session-info-model info) "gpt")))
      (delete-file file))))

(ert-deftest emacs-jcode-read-session-info-parses-large-transcripts ()
  (let* ((large (make-string 150000 ?x))
         (file (make-temp-file
                "emacs-jcode-large-session" nil ".json"
                (format "{\"id\":\"large\",\"messages\":[{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":%S}]}],\"working_dir\":\"/tmp/project\",\"short_name\":\"fixture\",\"status\":\"Active\",\"updated_at\":\"2026-01-02T00:00:00Z\"}"
                        large))))
    (unwind-protect
        (let ((info (emacs-jcode--read-session-info file)))
          (should info)
          (should (equal (emacs-jcode-session-info-id info) "large"))
          (should (equal (emacs-jcode-session-info-short-name info) "fixture"))
          (should (equal (emacs-jcode-session-info-status info) "Active")))
      (delete-file file))))

(ert-deftest emacs-jcode-latest-session-filters-current-directory ()
  (let* ((root (make-temp-file "emacs-jcode-sessions" t))
         (project-a (file-name-as-directory (expand-file-name "a" root)))
         (project-b (file-name-as-directory (expand-file-name "b" root)))
         (sessions (file-name-as-directory (expand-file-name "sessions" root)))
         (emacs-jcode-sessions-directory sessions))
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
          (should (equal (emacs-jcode-session-info-id (emacs-jcode-latest-session project-a t)) "new"))
          (should (equal (emacs-jcode-session-info-id (emacs-jcode-latest-session project-a nil)) "other")))
      (delete-directory root t))))

(ert-deftest emacs-jcode-session-display-title-prefixes-active-server ()
  (let ((active (emacs-jcode--make-session-info :short-name "alpha"
                                                :status "Active"
                                                :server-name "server"))
        (closed (emacs-jcode--make-session-info :short-name "beta"
                                                :status "Closed"
                                                :server-name "server")))
    (should (equal (emacs-jcode--session-display-title active) "server alpha"))
    (should (equal (emacs-jcode--session-display-title closed) "beta"))))

(ert-deftest emacs-jcode-list-refresh-is-command ()
  (should (commandp #'emacs-jcode-list-refresh)))

(ert-deftest emacs-jcode-send-uses-native-connection-when-present ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-send-chat*"))
         (input (generate-new-buffer " *jcode-test-native-send-input*"))
         (connection (emacs-jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp"))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-jcode-native-message)
                   (lambda (conn text)
                     (setq sent (list conn text)))))
          (with-current-buffer chat
            (emacs-jcode-chat-mode)
            (setq emacs-jcode--native-connection connection))
          (with-current-buffer input
            (emacs-jcode-input-mode)
            (setq emacs-jcode--chat-buffer chat)
            (insert "hello native")
            (emacs-jcode-send)
            (should (equal (buffer-string) "")))
          (should (equal sent (list connection "hello native")))
          (with-current-buffer chat
            (should (string-match-p "hello native" (buffer-string)))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest emacs-jcode-session-status-string-handles-structured-status ()
  (should (equal (emacs-jcode--session-status-string
                  '((Crashed (message . "Terminal or window closed (SIGHUP)"))))
                 "Crashed: Terminal or window closed (SIGHUP)"))
  (let ((info (emacs-jcode--make-session-info
               :id "s-crash"
               :short-name "broken"
               :status '((Crashed (message . "Terminal or window closed (SIGHUP)"))))))
    (should (equal (aref (cadr (emacs-jcode--session-list-entry info)) 1)
                   "Crashed: Terminal or window closed (SIGHUP)"))))

(ert-deftest emacs-jcode-native-json-read-parses-history ()
  (let ((event (emacs-jcode-native--json-read
                "{\"type\":\"history\",\"provider_model\":\"gpt\",\"messages\":[{\"role\":\"assistant\",\"content\":\"hello\"}]}")))
    (should (equal (alist-get 'type event) "history"))
    (should (equal (alist-get 'provider_model event) "gpt"))
    (should (equal (alist-get 'content (aref (alist-get 'messages event) 0)) "hello"))))

(ert-deftest emacs-jcode-native-history-renders-messages-and-metadata ()
  (let* ((chat (generate-new-buffer " *jcode-test-native-chat*"))
         (input (generate-new-buffer " *jcode-test-native-input*"))
         (connection (emacs-jcode--make-native-connection
                      :chat chat :input input :session-id "s-native" :cwd "/tmp")))
    (unwind-protect
        (progn
          (with-current-buffer chat (emacs-jcode-chat-mode))
          (with-current-buffer input (emacs-jcode-input-mode))
          (emacs-jcode-native--render-history
           connection
           '((type . "history")
             (server_name . "garden")
             (provider_model . "gpt-test")
             (messages . [((role . "user") (content . "hi"))
                          ((role . "assistant") (content . "hello"))])))
          (with-current-buffer chat
            (should (string-match-p "hi" (buffer-string)))
            (should (string-match-p "hello" (buffer-string)))
            (should (equal emacs-jcode--display-model "gpt-test"))))
      (kill-buffer chat)
      (kill-buffer input))))

(ert-deftest emacs-jcode-killing-chat-kills-input ()
  (let* ((dir default-directory)
         (buffers (emacs-jcode--make-buffers dir "linked-test"))
         (chat (car buffers))
         (input (cdr buffers)))
    (should (buffer-live-p chat))
    (should (buffer-live-p input))
    (kill-buffer chat)
    (should-not (buffer-live-p chat))
    (should-not (buffer-live-p input))))

(provide 'emacs-jcode-test)
;;; emacs-jcode-test.el ends here
