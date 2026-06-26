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
(declare-function jcode-session-busy "jcode-acp" (session))
(declare-function jcode-session-cancel "jcode-acp")
(declare-function jcode-session-close "jcode-acp")
(declare-function jcode-session-start "jcode-acp")
(declare-function jcode-native-message "jcode-native" (connection content))
(declare-function jcode-native-steer "jcode-native" (connection content))
(declare-function jcode-native-cancel "jcode-native" (connection))
(declare-function jcode-native-open-session "jcode-native" (session-id cwd chat input))
(declare-function jcode-native-close "jcode-native" (connection))
(declare-function jcode-execute-slash-command "jcode-menu" (command))

(defvar jcode-slash-commands)

(defvar jcode-input-ring-size 100
  "Maximum number of prompts to keep in input history.")

(defvar-local jcode--input-ring nil)
(defvar-local jcode--input-ring-index nil)
(defvar-local jcode--input-saved nil)
(defvar-local jcode--history-isearch-active nil
  "Non-nil while jcode input history isearch is active.")
(defvar-local jcode--history-isearch-saved-input nil
  "Input text saved before starting history isearch.")
(defvar-local jcode--history-isearch-index nil
  "Current input history index during history isearch.")
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
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (or (jcode--git-project-files directory)
        (jcode--scan-project-files directory))))

(defun jcode--git-project-files (directory)
  "Return git-tracked project files in DIRECTORY using TRAMP process APIs."
  (let ((default-directory directory))
    (with-temp-buffer
      (when (equal 0 (ignore-errors
                       (process-file "git" nil t nil
                                     "ls-files" "--cached" "--others"
                                     "--exclude-standard")))
        (seq-filter (lambda (file) (not (string-empty-p file)))
                    (split-string (buffer-string) "\n" t))))))

(defun jcode--excluded-file-name-p (name)
  "Return non-nil when NAME is excluded from fallback scans."
  (member name jcode--file-exclude-patterns))

(defun jcode--scan-project-files (directory)
  "Return project-relative files in DIRECTORY using Emacs file APIs."
  (let ((root (file-name-as-directory directory))
        result)
    (cl-labels
        ((scan (dir prefix)
           (dolist (name (condition-case nil
                             (directory-files dir nil nil t)
                           (file-error nil)
                           (permission-denied nil)))
             (unless (member name '("." ".."))
               (let ((full (expand-file-name name dir))
                     (relative (concat prefix name)))
                 (cond
                  ((and (file-directory-p full)
                        (not (file-symlink-p full))
                        (not (jcode--excluded-file-name-p name)))
                   (scan (file-name-as-directory full) (concat relative "/")))
                  ((file-regular-p full)
                   (push relative result))))))))
      (scan root ""))
    (nreverse result)))

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

(defun jcode--input-slash-command (text)
  "Return known slash command represented by TEXT, or nil."
  (when (and (boundp 'jcode-slash-commands)
             (string-match-p "\\`/\\(?:[[:alnum:]_-]+\\|?\\)[[:space:]]*\\'" text))
    (let ((command (car (split-string (string-trim text) "[[:space:]]+" t))))
      (and (assoc command jcode-slash-commands) command))))

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

(defun jcode-history-isearch-backward ()
  "Search prompt history backward with incremental isearch.
Matches are loaded directly into the input buffer, like readline history
search.  Quit restores the input that was present before search started."
  (interactive)
  (let ((ring (jcode--input-ring)))
    (when (ring-empty-p ring) (user-error "No history"))
    (setq jcode--history-isearch-active t
          jcode--history-isearch-saved-input (buffer-string)
          jcode--history-isearch-index nil)
    (isearch-backward nil t)))

(defun jcode--history-isearch-setup ()
  "Configure isearch for jcode prompt history search."
  (when jcode--history-isearch-active
    (setq isearch-message-prefix-add "history ")
    (setq-local isearch-search-fun-function #'jcode--history-isearch-search-fun)
    (setq-local isearch-wrap-function #'jcode--history-isearch-wrap)
    (setq-local isearch-push-state-function #'jcode--history-isearch-push-state)
    (setq-local isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook #'jcode--history-isearch-end nil t)))

(defun jcode--history-isearch-end ()
  "Clean up after jcode prompt history isearch."
  (setq isearch-message-prefix-add nil)
  (setq-local isearch-search-fun-function #'isearch-search-fun-default)
  (setq-local isearch-wrap-function nil)
  (setq-local isearch-push-state-function nil)
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook #'jcode--history-isearch-end t)
  (when isearch-mode-end-hook-quit
    (delete-region (point-min) (point-max))
    (insert (or jcode--history-isearch-saved-input "")))
  (unless isearch-suspended
    (setq jcode--history-isearch-active nil
          jcode--history-isearch-saved-input nil
          jcode--history-isearch-index nil)))

(defun jcode--history-isearch-goto (index)
  "Load prompt history item INDEX into the input buffer.
When INDEX is nil, restore the input saved before history search started."
  (setq jcode--history-isearch-index index)
  (delete-region (point-min) (point-max))
  (if (and index (not (ring-empty-p (jcode--input-ring))))
      (insert (ring-ref (jcode--input-ring) index))
    (insert (or jcode--history-isearch-saved-input ""))))

(defun jcode--history-isearch-search-fun ()
  "Return search function used by jcode prompt history isearch."
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default))
          (ring (jcode--input-ring))
          found)
      (or (funcall search-fun string bound noerror)
          (unless bound
            (condition-case nil
                (progn
                  (while (not found)
                    (if isearch-forward
                        (progn
                          (when (null jcode--history-isearch-index)
                            (error "End of history"))
                          (let ((idx (1- jcode--history-isearch-index)))
                            (jcode--history-isearch-goto (and (>= idx 0) idx)))
                          (goto-char (point-min)))
                      (let ((idx (1+ (or jcode--history-isearch-index -1))))
                        (when (>= idx (ring-length ring))
                          (error "Beginning of history"))
                        (jcode--history-isearch-goto idx)
                        (goto-char (point-max))))
                    (setq isearch-barrier (point)
                          isearch-opoint (point)
                          found (funcall search-fun string nil noerror)))
                  (point))
              (error nil)))))))

(defun jcode--history-isearch-wrap ()
  "Wrap jcode prompt history isearch."
  (jcode--history-isearch-goto
   (if isearch-forward
       (1- (ring-length (jcode--input-ring)))
     nil))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun jcode--history-isearch-push-state ()
  "Save jcode prompt history search state for isearch."
  (let ((index jcode--history-isearch-index))
    (lambda (_cmd) (jcode--history-isearch-goto index))))

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
	     ((jcode--input-slash-command text)
	      (delete-region (point-min) (point-max))
	      (jcode-execute-slash-command (jcode--input-slash-command text)))
     ((and native (jcode-native-connection-busy native))
      (jcode--history-add text)
      (delete-region (point-min) (point-max))
      (setf (jcode-native-connection-followup-queue native)
            (append (jcode-native-connection-followup-queue native) (list text)))
      (message "Jcode: Message queued (will send when ready)"))
     ((and session (jcode-session-busy session))
      (user-error "Jcode is busy; native follow-up queue is unavailable for ACP sessions"))
     ((and (not native) (not session) (buffer-live-p chat))
      (jcode--history-add text)
      (delete-region (point-min) (point-max))
      (jcode-render-user chat text)
      (jcode--section chat "Assistant" 'jcode-assistant-face)
      (let ((connection (jcode-native-open-session nil default-directory chat (current-buffer))))
        (jcode-native-message connection text)))
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
