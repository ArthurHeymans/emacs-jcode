;;; jcode-input.el --- Input handling for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ring)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jcode-ui)
(require 'jcode-render)
(require 'jcode-native)

(declare-function jcode-session-prompt "jcode-acp")
(declare-function jcode-session-cancel "jcode-acp")
(declare-function jcode-session-close "jcode-acp")
(declare-function jcode-native-message "jcode-native" (connection content))
(declare-function jcode-native-steer "jcode-native" (connection content))
(declare-function jcode-native-cancel "jcode-native" (connection))
(declare-function jcode-native-close "jcode-native" (connection))

(defvar jcode-input-ring-size 100
  "Maximum number of prompts to keep in input history.")

(defvar-local jcode--input-ring nil)
(defvar-local jcode--input-ring-index nil)
(defvar-local jcode--input-saved nil)
(defvar-local jcode--project-files-cache nil)
(defvar-local jcode--project-files-cache-time nil)

(defconst jcode--file-exclude-patterns
  '(".git" ".jj" "node_modules" ".elpa" "target" "build" "__pycache__" ".venv" "dist" ".direnv")
  "Directory names excluded from project file completion fallback scans.")

(defun jcode--file-cache-valid-p ()
  "Return non-nil when the project file completion cache is still fresh."
  (and jcode--project-files-cache
       jcode--project-files-cache-time
       (< (float-time (time-subtract (current-time) jcode--project-files-cache-time))
          30)))

(defun jcode--find-project-files (directory)
  "Return project-relative files in DIRECTORY using pi-style discovery."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (git-output (and (executable-find "git")
                          (shell-command-to-string
                           "git ls-files --cached --others --exclude-standard 2>/dev/null")))
         (git-files (and git-output
                         (seq-filter (lambda (file) (not (string-empty-p file)))
                                     (split-string git-output "\n" t)))))
    (or git-files
        (let ((find-command
               (concat "find . -type f "
                       (mapconcat (lambda (pattern)
                                    (format "-not -path '*/%s/*'" pattern))
                                  jcode--file-exclude-patterns
                                  " ")
                       " 2>/dev/null")))
          (mapcar (lambda (file)
                    (string-remove-prefix "./" file))
                  (seq-filter (lambda (file)
                                (not (string-empty-p file)))
                              (split-string (shell-command-to-string find-command) "\n" t)))))))

(defun jcode--project-file-candidates ()
  "Return project-relative file candidates for the current buffer."
  (unless (jcode--file-cache-valid-p)
    (setq jcode--project-files-cache
          (condition-case nil
              (jcode--find-project-files (jcode--project-directory))
            (file-error nil)
            (permission-denied nil))
          jcode--project-files-cache-time
          (current-time)))
  jcode--project-files-cache)

(defun jcode--complete-file-reference ()
  "Complete file reference after a typed @."
  (let ((choice (completing-read "File: " (jcode--project-file-candidates) nil nil)))
    (unless (string-empty-p choice)
      (insert choice))))

(defun jcode--at-trigger-p ()
  "Return non-nil when a typed @ should trigger file completion."
  (or (< (point) 3)
      (save-excursion
        (backward-char 2)
        (looking-at-p "[^[:alnum:]]"))))

(defun jcode--maybe-complete-at ()
  "Trigger project file completion after @ at a word boundary."
  (when (and (eq last-command-event ?@)
             (jcode--at-trigger-p))
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (derived-mode-p 'jcode-input-mode)
                           (jcode--complete-file-reference)))))))))

(defun jcode--file-reference-capf ()
  "Completion-at-point for @file references."
  (when-let* ((at-pos (save-excursion
                        (when (search-backward "@" (line-beginning-position) t)
                          (point)))))
    (let ((start (1+ at-pos))
          (end (point)))
      (list start end (jcode--project-file-candidates)
            :exclusive 'no
            :annotation-function (lambda (_) " (file)")))))

(defun jcode--path-prefix-p (path)
  "Return non-nil if PATH should receive filename completion."
  (or (string-prefix-p "./" path)
      (string-prefix-p "../" path)
      (string-prefix-p "~/" path)
      (string-prefix-p "/" path)))

(defun jcode--path-completions (path)
  "Return file completion candidates for PATH."
  (let* ((dir (file-name-directory path))
         (base (file-name-nondirectory path))
         (expanded-dir (expand-file-name (or dir "") default-directory)))
    (when (file-directory-p expanded-dir)
      (condition-case nil
          (mapcar (lambda (file) (concat (or dir "") file))
                  (cl-remove-if (lambda (file) (member file '("." ".." "./" "../")))
                                (file-name-all-completions base expanded-dir)))
        (file-error nil)
        (permission-denied nil)))))

(defun jcode--path-capf ()
  "Completion-at-point for paths beginning with ./, ../, ~/, or /."
  (when-let* ((bounds (bounds-of-thing-at-point 'filename))
              (start (car bounds))
              (end (cdr bounds))
              (path (buffer-substring-no-properties start end))
              ((jcode--path-prefix-p path))
              (candidates (jcode--path-completions path)))
    (list start end candidates :exclusive 'no)))

(defun jcode-complete ()
  "Complete at point in a jcode input buffer."
  (interactive)
  (let ((completion-show-help nil))
    (completion-at-point)))

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
  "Send current input buffer content to jcode.
If a native session is busy, queue the text as a follow-up."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat jcode--chat-buffer)
         (native (jcode--native-connection-for-chat chat))
         (session (and (buffer-live-p chat)
                       (buffer-local-value 'jcode--session chat))))
    (cond
     ((string-empty-p text)
      (user-error "Prompt is empty"))
     ((and native (jcode-native-connection-busy native))
      (jcode--history-add text)
      (delete-region (point-min) (point-max))
      (setf (jcode-native-connection-followup-queue native)
            (append (jcode-native-connection-followup-queue native) (list text)))
      (message "Jcode: Message queued (will send when ready)"))
     ((and session (jcode-session-busy session))
      (user-error "Jcode is busy; native follow-up queue is unavailable for ACP sessions"))
     ((or native session)
      (jcode--history-add text)
      (delete-region (point-min) (point-max))
      (jcode-render-user chat text)
      (jcode--section chat "Assistant" 'jcode-assistant-face)
      (if native
          (jcode-native-message native text)
        (jcode-session-prompt session text)))
     (t
      (user-error "No jcode session")))))

(defun jcode-steer ()
  "Send current input as a steering message for a busy native session."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat jcode--chat-buffer)
         (native (jcode--native-connection-for-chat chat)))
    (when (string-empty-p text) (user-error "Prompt is empty"))
    (unless native (user-error "No native jcode session"))
    (unless (jcode-native-connection-busy native)
      (user-error "Nothing to steer; use C-c C-c to send"))
    (jcode--history-add text)
    (delete-region (point-min) (point-max))
    (jcode-native-steer native text)
    (message "Jcode: Steering message sent")))

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
