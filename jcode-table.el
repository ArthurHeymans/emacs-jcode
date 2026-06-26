;;; jcode-table.el --- Display-only markdown table decoration for jcode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Lightweight display-only wrapping for markdown pipe tables in jcode chat
;; buffers.  The underlying buffer text stays canonical markdown.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom jcode-prettify-tables t
  "Whether to display markdown pipe tables as wrapped, aligned table overlays."
  :type 'boolean
  :group 'jcode)

(defvar-local jcode--table-display-width nil
  "Last chat window width used to decorate markdown tables.")

(defconst jcode--table-overlay-property 'jcode-table-overlay)

(defun jcode--table-line-p (line)
  "Return non-nil when LINE looks like a markdown pipe table row."
  (and (string-match-p "|" line)
       (string-match-p "^[[:space:]>]*|.*|[[:space:]]*$" line)))

(defun jcode--table-separator-line-p (line)
  "Return non-nil when LINE looks like a markdown table separator."
  (string-match-p "^[[:space:]>]*|?[[:space:]-:|]+|[[:space:]-:|]*$" line))

(defun jcode--table-split-row (line)
  "Split markdown table LINE into trimmed cell strings."
  (let* ((text (string-trim line))
         (text (string-remove-prefix "|" text))
         (text (string-remove-suffix "|" text)))
    (mapcar #'string-trim (split-string text "|"))))

(defun jcode--table-region-valid-p (rows)
  "Return non-nil when ROWS form a markdown pipe table."
  (and (>= (length rows) 2)
       (cl-some #'jcode--table-separator-line-p rows)))

(defun jcode--table-column-widths (cells max-width)
  "Return display widths for table CELLS constrained by MAX-WIDTH."
  (let* ((cols (apply #'max 1 (mapcar #'length cells)))
         (raw-widths (make-vector cols 3)))
    (dolist (row cells)
      (cl-loop for cell in row
               for i from 0
               do (aset raw-widths i (max (aref raw-widths i) (string-width cell)))))
    (let* ((budget (max 3 (- max-width (* (1+ cols) 3))))
           (per-col (max 3 (/ budget cols))))
      (cl-loop for i below cols
               collect (min (aref raw-widths i) per-col)))))

(defun jcode--table-pad (text width)
  "Pad TEXT to display WIDTH."
  (concat text (make-string (max 0 (- width (string-width text))) ? )))

(defun jcode--table-wrap-cell (cell width)
  "Wrap CELL into display chunks no wider than WIDTH."
  (let ((words (split-string cell "[[:space:]]+" t))
        lines current)
    (dolist (word words)
      (cond
       ((not current) (setq current word))
       ((<= (+ (string-width current) 1 (string-width word)) width)
        (setq current (concat current " " word)))
       (t
        (push current lines)
        (setq current word))))
    (when current (push current lines))
    (or (nreverse lines) '(""))))

(defun jcode--table-render-row (cells widths)
  "Render table row CELLS with column WIDTHS."
  (let* ((wrapped (cl-loop for width in widths
                           for cell = (or (pop cells) "")
                           collect (jcode--table-wrap-cell cell width)))
         (height (apply #'max (mapcar #'length wrapped)))
         lines)
    (dotimes (line height)
      (push (concat "| "
                    (mapconcat
                     (lambda (col)
                       (jcode--table-pad (or (nth line (nth col wrapped)) "")
                                         (nth col widths)))
                     (number-sequence 0 (1- (length widths)))
                     " | ")
                    " |")
            lines))
    (nreverse lines)))

(defun jcode--table-render-separator (widths)
  "Render a markdown table separator for WIDTHS."
  (concat "| "
          (mapconcat (lambda (width) (make-string width ?-)) widths " | ")
          " |"))

(defun jcode--table-render (rows width)
  "Render markdown table ROWS for display WIDTH."
  (let* ((data-rows (cl-remove-if #'jcode--table-separator-line-p rows))
         (cells (mapcar #'jcode--table-split-row data-rows))
         (widths (jcode--table-column-widths cells width))
         (out nil)
         (first t))
    (dolist (row rows)
      (if (jcode--table-separator-line-p row)
          (push (jcode--table-render-separator widths) out)
        (dolist (line (jcode--table-render-row (jcode--table-split-row row) widths))
          (push line out))
        (when first
          (setq first nil))))
    (string-join (nreverse out) "\n")))

(defun jcode--remove-table-overlays (&optional beg end)
  "Remove jcode table overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   jcode--table-overlay-property t))

(defun jcode-decorate-tables (&optional beg end)
  "Decorate markdown pipe tables between BEG and END with display overlays."
  (when jcode-prettify-tables
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t)
              (width (max 30 (- (or (and (get-buffer-window (current-buffer) t)
                                        (window-body-width (get-buffer-window (current-buffer) t)))
                                   fill-column 80)
                               2))))
          (jcode--remove-table-overlays beg end)
          (goto-char (or beg (point-min)))
          (beginning-of-line)
          (while (< (point) (or end (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (if (jcode--table-line-p line)
                  (let ((start (line-beginning-position)) rows finish)
                    (while (and (< (point) (point-max))
                                (jcode--table-line-p
                                 (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position))))
                      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) rows)
                      (forward-line 1))
                    (setq finish (point))
                    (setq rows (nreverse rows))
                    (when (jcode--table-region-valid-p rows)
                      (let ((ov (make-overlay start finish nil t nil)))
                        (overlay-put ov jcode--table-overlay-property t)
                        (overlay-put ov 'display (jcode--table-render rows width)))))
                (forward-line 1)))))))))

(defun jcode-refresh-table-overlays ()
  "Refresh jcode table overlays when the visible chat width changes."
  (when (and jcode-prettify-tables (derived-mode-p 'jcode-chat-mode))
    (let ((width (and (get-buffer-window (current-buffer) t)
                      (window-body-width (get-buffer-window (current-buffer) t)))))
      (unless (equal width jcode--table-display-width)
        (setq jcode--table-display-width width)
        (jcode-decorate-tables)))))

(provide 'jcode-table)
;;; jcode-table.el ends here
