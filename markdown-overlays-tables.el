;;; markdown-overlays-tables.el --- Table prettification for markdown-overlays  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Edd Wilder-James https://ewj.me
;; URL: https://github.com/xenodium/shell-maker

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Table prettification support for `markdown-overlays'.
;;
;; Tables use a different strategy than other markdown elements in
;; markdown-overlays.el.  Other elements (bold, italic, etc.) use buffer
;; overlays to hide markup characters while styling the visible text
;; in-place.  Tables use the overlay `display' property to replace entire
;; cell regions with formatted strings.
;;
;; We chose the display property approach because:
;; - Tables require precise column alignment and width control
;; - Cell content may wrap to multiple lines
;; - Unicode box-drawing characters replace ASCII pipes/dashes
;; - A single overlay per cell is simpler than multiple hide/show overlays
;;
;; The helper functions below process markdown syntax in strings, returning
;; propertized strings with markup removed.  This differs from the buffer-based
;; fontify-* functions in markdown-overlays.el.

;;; Code:

(require 'map)
(require 'seq)

(declare-function markdown-overlays--put "markdown-overlays")

(defcustom markdown-overlays-prettify-tables t
  "Whether or not to prettify markdown table columns.
When non-nil, table columns are visually aligned using overlays."
  :type 'boolean
  :group 'markdown-overlays)

(defvar markdown-overlays--table-use-unicode-borders t
  "Whether to use Unicode box-drawing characters for table borders.")

(defvar markdown-overlays--table-wrap-columns t
  "Whether to wrap table columns to fit within window width.")

(defvar markdown-overlays--table-max-width-fraction 0.9
  "Fraction of window width to use as max table width.")

(defvar markdown-overlays--table-zebra-stripe t
  "Whether to alternate row backgrounds for better readability.")

(defvar markdown-overlays--table-row-underline nil
  "Whether to underline the last line of each data row.")

(defvar markdown-overlays--table-header-face 'bold
  "Face to apply to table header row content.")

(defvar markdown-overlays--table-border-face 'font-lock-comment-face
  "Face to apply to table borders (pipes and dashes).")

(defface markdown-overlays-table-zebra-face
  '((((background light)) :background "#f0f0f0" :foreground "#333333")
    (((background dark)) :background "#2a2a2a" :foreground "#cccccc"))
  "Face for alternating (even) rows in tables."
  :group 'markdown-overlays)

(defface markdown-overlays-table-row-face
  '((((background light)) :foreground "#555555")
    (((background dark)) :foreground "#aaaaaa"))
  "Face for regular (odd) data rows in tables."
  :group 'markdown-overlays)

;; Unicode box-drawing characters for tables
(defconst markdown-overlays--table-border-pipe "│"
  "Unicode vertical line for table borders.")

(defconst markdown-overlays--table-border-dash "─"
  "Unicode horizontal line for table borders.")

(defconst markdown-overlays--table-border-cross "┼"
  "Unicode cross for table border intersections.")

(defconst markdown-overlays--table-border-tee-left "├"
  "Unicode left-edge tee for table borders.")

(defconst markdown-overlays--table-border-tee-right "┤"
  "Unicode right-edge tee for table borders.")

;; Table alignment support

(defvar markdown-overlays--table-line-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (not (any "\n")))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a single line of a markdown table.")

(defvar markdown-overlays--table-separator-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (or "-" ":" "|" " "))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a table separator line (e.g., |---|---|).")

(defun markdown-overlays--find-tables (&optional avoid-ranges)
  "Find all markdown tables in the buffer.
Returns a list of alists with :start, :end, :separator-row, and :rows.
AVOID-RANGES is a list of (start . end) cons to skip."
  (let ((tables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-overlays--table-line-regexp nil t)
        (let ((line-start (line-beginning-position))
              (in-avoided nil))
          ;; Check if we're in an avoided range
          (dolist (range avoid-ranges)
            (when (and (>= line-start (car range))
                       (<= line-start (cdr range)))
              (setq in-avoided t)))
          (unless in-avoided
            ;; Found potential table start, scan for full table
            (goto-char line-start)
            (let ((table-start line-start)
                  (table-end nil)
                  (separator-row nil)
                  (rows '())
                  (row-num 0))
              ;; Collect all consecutive table lines
              (while (and (not (eobp))
                          (looking-at markdown-overlays--table-line-regexp))
                (let ((row-start (point))
                      (row-end (line-end-position))
                      (is-sep (looking-at markdown-overlays--table-separator-regexp)))
                  (when (and (not separator-row) is-sep)
                    (setq separator-row row-num))
                  (push (list (cons :start row-start)
                              (cons :end row-end)
                              (cons :num row-num)
                              (cons :separator is-sep))
                        rows)
                  (setq row-num (1+ row-num))
                  (setq table-end row-end))
                (forward-line 1))
              ;; Only count as table if we have at least 2 rows
              (when (>= (length rows) 2)
                (push (list (cons :start table-start)
                            (cons :end table-end)
                            (cons :separator-row separator-row)
                            (cons :rows (nreverse rows)))
                      tables)))))))
    (nreverse tables)))

(defun markdown-overlays--parse-table-row (start end)
  "Parse a table row between START and END into cells.
Returns a list of alists with :start, :end, :content for each cell."
  (let ((cells '()))
    (save-excursion
      (goto-char start)
      ;; Skip leading whitespace and pipe
      (when (looking-at "[ \t]*|")
        (goto-char (match-end 0)))
      (let ((cell-start (point)))
        ;; Find each cell delimited by |
        (while (re-search-forward "|" end t)
          (let ((cell-end (1- (point))))
            (push (list (cons :start cell-start)
                        (cons :end cell-end)
                        (cons :content (string-trim
                                  (buffer-substring-no-properties cell-start cell-end))))
                  cells)
            (setq cell-start (point))))))
    (nreverse cells)))

(defun markdown-overlays--compute-table-column-widths (table)
  "Compute the maximum width of each column in TABLE.
Returns a list of integers, one per column.
Widths are based on PROCESSED content (after markdown syntax is removed)."
  (let ((widths nil))
    (dolist (row (map-elt table :rows))
      (unless (map-elt row :separator)
        (let ((cells (markdown-overlays--parse-table-row
                      (map-elt row :start)
                      (map-elt row :end)))
              (col 0))
          (dolist (cell cells)
            ;; Process markdown first to get actual display width
            (let* ((raw-content (map-elt cell :content))
                   (processed (markdown-overlays--process-cell-content raw-content))
                   (content-width (string-width processed)))
              (if (nth col widths)
                  (setf (nth col widths)
                        (max (nth col widths) content-width))
                ;; Extend widths list
                (setq widths (append widths (list content-width))))
              (setq col (1+ col)))))))
    widths))

(defun markdown-overlays--compute-table-min-column-widths (table)
  "Compute minimum width for each column in TABLE.
Minimum is the longest single word in the column (to avoid breaking words).
Based on PROCESSED content (after markdown syntax is removed)."
  (let ((min-widths nil))
    (dolist (row (map-elt table :rows))
      (unless (map-elt row :separator)
        (let ((cells (markdown-overlays--parse-table-row
                      (map-elt row :start)
                      (map-elt row :end)))
              (col 0))
          (dolist (cell cells)
            ;; Process markdown first to get actual display content
            (let* ((raw-content (map-elt cell :content))
                   (processed (markdown-overlays--process-cell-content raw-content))
                   (longest-word (markdown-overlays--table-longest-word processed)))
              (if (nth col min-widths)
                  (setf (nth col min-widths)
                        (max (nth col min-widths) longest-word))
                (setq min-widths (append min-widths (list longest-word))))
              (setq col (1+ col)))))))
    min-widths))

(defun markdown-overlays--table-longest-word (str)
  "Return length of longest word in STR."
  (if (or (null str) (string-empty-p str))
      0
    (apply #'max (mapcar #'length (split-string str "[ \t\n]+" t)))))

(defun markdown-overlays--table-total-width (widths)
  "Calculate total rendered width for WIDTHS including borders and padding."
  ;; Each column: content + 2 spaces padding + 1 pipe. Plus 1 pipe at start.
  (+ 1 (seq-reduce (lambda (acc w) (+ acc w 3)) widths 0)))

(defun markdown-overlays--table-allocate-widths (natural-widths min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS."
  (let* ((total (markdown-overlays--table-total-width natural-widths))
         (excess (- total target)))
    (if (<= excess 0)
        natural-widths  ; Fits already
      ;; Calculate how much each column can shrink
      (let* ((shrinkable (seq-mapn (lambda (w m) (max 0 (- w m)))
                                   natural-widths min-widths))
             (total-shrinkable (seq-reduce #'+ shrinkable 0)))
        (if (<= total-shrinkable 0)
            min-widths  ; Can't shrink further
          (let ((ratio (min 1.0 (/ (float excess) total-shrinkable))))
            (seq-mapn (lambda (w m s)
                        (max m (floor (- w (* s ratio)))))
                      natural-widths min-widths shrinkable)))))))

(defun markdown-overlays--table-wrap-text (text width)
  "Wrap TEXT to fit within WIDTH, returning list of lines.
Preserves text properties across wrapped lines."
  (if (or (null text) (string-empty-p text))
      (list "")
    (if (<= (string-width text) width)
        (list text)
      ;; Wrap while preserving text properties
      (let ((lines '())
            (pos 0)
            (len (length text)))
        (while (< pos len)
          (let* ((end-pos (min (+ pos width) len))
                 ;; Find a good break point (space) if we're not at the end
                 (break-pos (if (>= end-pos len)
                                end-pos
                              ;; Look backwards for a space
                              (let ((space-pos nil))
                                (save-match-data
                                  (when (string-match ".*\\s-" (substring text pos end-pos))
                                    (setq space-pos (+ pos (match-end 0)))))
                                (or space-pos end-pos))))
                 (line (substring text pos break-pos)))
            ;; Trim trailing whitespace from line
            (setq line (string-trim-right line))
            (push line lines)
            ;; Skip leading whitespace on next line
            (setq pos break-pos)
            (while (and (< pos len)
                        (memq (aref text pos) '(?\s ?\t)))
              (setq pos (1+ pos)))))
        (nreverse lines)))))

(defun markdown-overlays--pad-string (str width)
  "Pad STR with spaces to reach WIDTH."
  (let ((current-width (string-width str)))
    (if (>= current-width width)
        str
      (concat str (make-string (- width current-width) ?\s)))))

(defun markdown-overlays--make-table-separator-cell (width)
  "Create a separator cell string of dashes for WIDTH."
  (if markdown-overlays--table-use-unicode-borders
      (make-string width (string-to-char markdown-overlays--table-border-dash))
    (make-string width ?-)))

(defun markdown-overlays--apply-face-to-unpropertized (str face &optional extra-props)
  "Apply FACE and EXTRA-PROPS to characters in STR lacking a `face' property.
Characters that already have a `face' property are left untouched."
  (let ((result (copy-sequence str))
        (i 0)
        (len (length str)))
    (while (< i len)
      (unless (get-text-property i 'face result)
        (put-text-property i (1+ i) 'face face result)
        (when extra-props
          (let ((p extra-props))
            (while p
              (put-text-property i (1+ i) (car p) (cadr p) result)
              (setq p (cddr p))))))
      (setq i (1+ i)))
    result))

(defun markdown-overlays--replace-markup (str regex group-num face &optional extra-props)
  "Replace REGEX matches in STR with propertized text.
GROUP-NUM is the capture group containing the text to keep.
FACE is applied only to characters that lack an existing `face' property.
Matches whose start position already has a `face' are skipped entirely
so that their delimiter characters are preserved."
  (let ((new-result "")
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0)))
        (if (get-text-property match-start 'face str)
            ;; Already propertized — emit verbatim and advance past it.
            (let ((prop-end (next-single-property-change
                             match-start 'face str (length str))))
              (setq new-result (concat new-result
                                       (substring str pos prop-end)))
              (setq pos prop-end))
          (let* ((text (match-string group-num str))
                 (styled (markdown-overlays--apply-face-to-unpropertized
                          text face extra-props)))
            (setq new-result (concat new-result
                                     (substring str pos match-start)
                                     styled))
            (setq pos match-end)))))
    (concat new-result (substring str pos))))

(defun markdown-overlays--replace-markup-alt (str regex group1 group2 face)
  "Replace REGEX matches in STR, using GROUP1 or GROUP2 for text.
Used for patterns like **text** or __text__ where either group may match.
FACE is applied only to characters that lack an existing `face' property.
Matches whose start position already has a `face' are skipped entirely."
  (let ((new-result "")
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0)))
        (if (get-text-property match-start 'face str)
            (let ((prop-end (next-single-property-change
                             match-start 'face str (length str))))
              (setq new-result (concat new-result
                                       (substring str pos prop-end)))
              (setq pos prop-end))
          (let* ((text (or (match-string group1 str) (match-string group2 str)))
                 (styled (markdown-overlays--apply-face-to-unpropertized text face)))
            (setq new-result (concat new-result
                                     (substring str pos match-start)
                                     styled))
            (setq pos match-end)))))
    (concat new-result (substring str pos))))

(defun markdown-overlays--process-cell-content (content)
  "Process markdown syntax in CONTENT string, returning propertized string.
Handles: links [text](url), bold **text**/__text__, italic *text*/_text_,
bold-italic ***text***, inline code `text`, and strikethrough ~~text~~."
  (let ((result content))
    ;; Process inline code FIRST so its contents are protected from
    ;; bold/italic processing (e.g., `**text**` should render as code).
    ;; After extracting code spans, neutralize markup chars within them
    ;; so subsequent bold/italic regexes cannot match inside code.
    (setq result (markdown-overlays--replace-markup
                  result "`\\([^`]+\\)`" 1 'font-lock-doc-markup-face))

    ;; Links need special handling for keymap.
    ;; Skip matches inside already-propertized regions (e.g. inline code).
    (let ((link-re "\\[\\([^]]+\\)\\](\\([^)]+\\))")
          (new-result "")
          (pos 0))
      (while (string-match link-re result pos)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0)))
          (if (get-text-property match-start 'face result)
              (let ((prop-end (next-single-property-change
                               match-start 'face result (length result))))
                (setq new-result (concat new-result
                                         (substring result pos prop-end)))
                (setq pos prop-end))
            (let* ((title (match-string 1 result))
                   (url (match-string 2 result))
                   (link-map (make-sparse-keymap)))
              (setq new-result (concat new-result (substring result pos match-start)))
              (define-key link-map [mouse-1]
                          (lambda () (interactive) (browse-url url)))
              (define-key link-map (kbd "RET")
                          (lambda () (interactive) (browse-url url)))
              (setq new-result (concat new-result
                                       (propertize title
                                                   'face 'link
                                                   'mouse-face 'highlight
                                                   'keymap link-map
                                                   'help-echo url)))
              (setq pos match-end)))))
      (setq result (concat new-result (substring result pos))))

    ;; Bold-italic, bold, italic, strikethrough
    (setq result (markdown-overlays--replace-markup
                  result "\\*\\*\\*\\([^*]+\\)\\*\\*\\*" 1 '(:weight bold :slant italic)))
    (setq result (markdown-overlays--replace-markup-alt
                  result "\\(?:\\*\\*\\([^*]+\\)\\*\\*\\|__\\([^_]+\\)__\\)" 1 2 'bold))
    ;; Italic: the regex captures a preceding non-backslash char (group 1)
    ;; to avoid matching escaped \*text\*.  We must re-emit that char.
    ;; Skip matches where the delimiter already has a face (e.g. inside code).
    (let ((italic-re "\\(\\`\\|[^\\\\]\\)\\(?:\\*\\([^*]+\\)\\*\\|_\\([^_]+\\)_\\)")
          (new-result "")
          (pos 0))
      (while (string-match italic-re result pos)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (delim-pos (match-beginning 2))
               (delim-pos (or delim-pos (match-beginning 3))))
          (if (and delim-pos
                   (> delim-pos 0)
                   (get-text-property (1- delim-pos) 'face result))
              ;; Delimiter is inside propertized region — skip.
              (let ((prop-end (next-single-property-change
                               match-start 'face result (length result))))
                (setq new-result (concat new-result
                                         (substring result pos prop-end)))
                (setq pos prop-end))
            (let* ((prefix (match-string 1 result))
                   (text (or (match-string 2 result) (match-string 3 result)))
                   (styled (markdown-overlays--apply-face-to-unpropertized
                            text 'italic)))
              (setq new-result (concat new-result
                                       (substring result pos match-start)
                                       prefix styled))
              (setq pos match-end)))))
      (setq result (concat new-result (substring result pos))))
    (setq result (markdown-overlays--replace-markup
                  result "~~\\([^~]+\\)~~" 1 '(:strike-through t)))

    result))

(defun markdown-overlays--align-table (table)
  "Apply display overlays to align TABLE columns.
If `markdown-overlays--table-wrap-columns' is non-nil and table is too wide,
columns are wrapped to fit within window width."
  (let* ((natural-widths (markdown-overlays--compute-table-column-widths table))
         (min-widths (markdown-overlays--compute-table-min-column-widths table))
         (target-width (when markdown-overlays--table-wrap-columns
                         (floor (* (window-body-width)
                                   markdown-overlays--table-max-width-fraction))))
         (total-natural (markdown-overlays--table-total-width natural-widths))
         (col-widths (if (and markdown-overlays--table-wrap-columns
                              target-width
                              (> total-natural target-width))
                         (markdown-overlays--table-allocate-widths
                          natural-widths min-widths target-width)
                       natural-widths))
         (needs-wrapping (not (equal col-widths natural-widths)))
         (separator-row (map-elt table :separator-row))
         (rows (map-elt table :rows))
         (data-row-num 0))  ; Track data rows for zebra striping

    (dolist (row rows)
      (let* ((row-start (map-elt row :start))
             (row-end (map-elt row :end))
             (row-num (map-elt row :num))
             (is-separator (map-elt row :separator))
             (is-header (and separator-row (< row-num separator-row)))
             (is-zebra (and markdown-overlays--table-zebra-stripe
                            (not is-header)
                            (not is-separator)
                            (= (mod data-row-num 2) 1)))
             (cells (markdown-overlays--parse-table-row row-start row-end)))

        ;; Increment data row counter for non-header, non-separator rows
        (unless (or is-header is-separator)
          (setq data-row-num (1+ data-row-num)))

        (if (and (not is-separator) (not needs-wrapping))
            ;; Simple case: no wrapping needed and not separator
            (let ((col 0))
              (dolist (cell cells)
                (let* ((cell-start (map-elt cell :start))
                       (cell-end (map-elt cell :end))
                       (content (map-elt cell :content))
                       (processed-content (markdown-overlays--process-cell-content content))
                       (cell-width (or (nth col col-widths) (string-width content)))
                       (padded-content
                        (concat " " (markdown-overlays--pad-string processed-content cell-width) " "))
                       (base-face (cond
                                   (is-header markdown-overlays--table-header-face)
                                   (is-zebra 'markdown-overlays-table-zebra-face)
                                   (t 'markdown-overlays-table-row-face)))
                       (display-str (progn
                                      (add-face-text-property 0 (length padded-content)
                                                              base-face t padded-content)
                                      padded-content))
                       (ov (make-overlay cell-start cell-end)))
                  (markdown-overlays--put ov 'display display-str)
                  (setq col (1+ col)))))

          ;; Wrapping case OR separator row - render as complete row overlay
          (if is-separator
              ;; Separator row
              (let* ((pipe (if markdown-overlays--table-use-unicode-borders
                               markdown-overlays--table-border-cross "|"))
                     (pipe-left (if markdown-overlays--table-use-unicode-borders
                                    markdown-overlays--table-border-tee-left "|"))
                     (pipe-right (if markdown-overlays--table-use-unicode-borders
                                     markdown-overlays--table-border-tee-right "|"))
                     ;; Preserve leading whitespace
                     (leading-ws (save-excursion
                                   (goto-char row-start)
                                   (if (looking-at "^[ \t]*")
                                       (match-string 0) "")))
                     (row-display
                      (concat
                       leading-ws
                       (propertize pipe-left 'face markdown-overlays--table-border-face)
                       (mapconcat
                        (lambda (idx)
                          (let ((width (nth idx col-widths)))
                            (propertize (markdown-overlays--make-table-separator-cell (+ width 2))
                                        'face markdown-overlays--table-border-face)))
                        (number-sequence 0 (1- (length col-widths)))
                        (propertize pipe 'face markdown-overlays--table-border-face))
                       (propertize pipe-right 'face markdown-overlays--table-border-face)))
                     (ov (make-overlay row-start row-end)))
                (markdown-overlays--put ov 'display row-display))

            ;; Complex case: wrapping needed
            (let* ((wrapped-cells
                    (seq-map-indexed
                     (lambda (cell idx)
                       (let* ((width (or (nth idx col-widths) 10))
                              (content (map-elt cell :content))
                              ;; Process markdown FIRST, then wrap (preserves properties)
                              (processed (markdown-overlays--process-cell-content content)))
                         (markdown-overlays--table-wrap-text processed width)))
                     cells))
                   (max-lines (apply #'max 1 (mapcar #'length wrapped-cells)))
                   (pipe (if markdown-overlays--table-use-unicode-borders
                             markdown-overlays--table-border-pipe "|"))
                   (styled-pipe (propertize pipe 'face markdown-overlays--table-border-face))
                   ;; Preserve leading whitespace
                   (leading-ws (save-excursion
                                 (goto-char row-start)
                                 (if (looking-at "^[ \t]*")
                                     (match-string 0) "")))
                   ;; Build multi-line display string for entire row
                   (row-display
                    (mapconcat
                     (lambda (line-idx)
                       (let ((is-last-line (= line-idx (1- max-lines))))
                         (concat
                          leading-ws
                          styled-pipe
                          (mapconcat
                           (lambda (col-idx)
                             (let* ((cell-lines (nth col-idx wrapped-cells))
                                    (width (nth col-idx col-widths))
                                    (line (or (nth line-idx cell-lines) ""))
                                    (padded (concat " "
                                                    (markdown-overlays--pad-string line width)
                                                    " "))
                                    (base-face (cond
                                                (is-header markdown-overlays--table-header-face)
                                                (is-zebra 'markdown-overlays-table-zebra-face)
                                                (t 'markdown-overlays-table-row-face)))
                                    (final-face (if (and markdown-overlays--table-row-underline
                                                         is-last-line
                                                         (not is-header))
                                                    (list base-face '(:underline t))
                                                  base-face)))
                               ;; Use add-face-text-property to preserve inline formatting
                               (add-face-text-property 0 (length padded) final-face t padded)
                               padded))
                           (number-sequence 0 (1- (length cells)))
                           styled-pipe)
                          styled-pipe)))
                     (number-sequence 0 (1- max-lines))
                     "\n"))
                   ;; Create overlay for entire row (including pipes)
                   (ov (make-overlay row-start row-end)))
              (markdown-overlays--put ov 'display row-display))))

        ;; Style pipe characters at row boundaries (only for simple non-wrapped rows)
        (when (and (not is-separator) (not needs-wrapping))
          (save-excursion
            (goto-char row-start)
            (while (re-search-forward "|" row-end t)
              (let ((ov (make-overlay (1- (point)) (point))))
                (markdown-overlays--put ov 'face markdown-overlays--table-border-face)
                (when markdown-overlays--table-use-unicode-borders
                  (markdown-overlays--put
                   ov 'display
                   (propertize markdown-overlays--table-border-pipe
                               'face markdown-overlays--table-border-face)))))))))))

(defun markdown-overlays--fontify-tables (tables)
  "Align all markdown TABLES using display overlays."
  (when markdown-overlays-prettify-tables
    (dolist (table tables)
      (markdown-overlays--align-table table))))

(provide 'markdown-overlays-tables)

;;; markdown-overlays-tables.el ends here
