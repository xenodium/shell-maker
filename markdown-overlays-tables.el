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
;; Extends markdown-overlays to render markdown tables with:
;; - Column alignment using overlay display properties
;; - Automatic column wrapping when table exceeds window width
;; - Unicode box-drawing borders (│ ─ ┼ ├ ┤)
;; - Zebra striping for better row distinction
;; - Bold headers, dimmed borders
;; - Inline markdown formatting (bold, italic, code, links, strikethrough)
;;
;; Before: | Name | Role |
;;         |------|------|
;;         | **Alice** | [Engineer](http://x.com) |
;;
;; After:  │ Name      │ Role     │
;;         ├───────────┼──────────┤
;;         │ Alice     │ Engineer │  (with bold and clickable link)
;;
;; Note on implementation strategy:
;; Tables use `invisible' + `before-string' overlays to replace entire
;; rows with formatted strings.  This differs from other markdown elements
;; in markdown-overlays.el which use buffer overlays to hide markup while
;; styling text in-place.
;;
;; We chose the invisible + before-string approach because:
;; - Tables require precise column alignment and width control
;; - Cell content may wrap to multiple lines
;; - Unicode box-drawing characters replace ASCII pipes/dashes
;; - Text properties (e.g. fractional-width spaces) are honoured
;; - A single overlay per row is simpler than multiple hide/show overlays
;;
;; Trade-offs:
;; - Search won't find markdown syntax hidden in table cells
;; - Consistent with how wrapped/multi-line content must work anyway

;;; Code:

(require 'seq)
(require 'map)

(declare-function markdown-overlays--put "markdown-overlays")

(defvar markdown-overlays-prettify-tables t
  "Whether or not to prettify markdown table columns.
When non-nil, table columns are visually aligned using overlays.")

(defvar markdown-overlays--table-header-face 'bold
  "Face to apply to table header row content.")

(defvar markdown-overlays--table-border-face 'font-lock-comment-face
  "Face to apply to table borders (pipes and dashes).")

(defvar markdown-overlays--table-use-unicode-borders t
  "When non-nil, use Unicode box-drawing characters for table borders.")

(defvar markdown-overlays--table-wrap-columns t
  "When non-nil, wrap table columns to fit within window width.")

(defvar markdown-overlays--table-max-width-fraction 0.9
  "Fraction of window width to use as max table width.")

(defvar markdown-overlays--table-zebra-stripe t
  "When non-nil, alternate row backgrounds for better readability.")

(defvar markdown-overlays--table-zebra-face 'lazy-highlight
  "Face for alternating (even) rows in tables.")

(defvar markdown-overlays--table-row-face 'default
  "Face for regular (odd) data rows in tables.")

(defvar markdown-overlays--table-border-pipe "│"
  "Unicode vertical line for table borders.")

(defvar markdown-overlays--table-border-dash "─"
  "Unicode horizontal line for table borders.")

(defvar markdown-overlays--table-border-cross "┼"
  "Unicode cross for table border intersections.")

(defvar markdown-overlays--table-border-tee-left "├"
  "Unicode left-edge tee for table borders.")

(defvar markdown-overlays--table-border-tee-right "┤"
  "Unicode right-edge tee for table borders.")

(defconst markdown-overlays--table-line-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (not (any "\n")))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a single line of a markdown table.")

(defconst markdown-overlays--table-separator-regexp
  (rx line-start
      (* (any " \t"))
      "|"
      (+ (or "-" ":" "|" " "))
      "|"
      (* (any " \t"))
      line-end)
  "Regexp matching a table separator line (e.g., |---|---|).")

;;; Table finding

(defun markdown-overlays--find-tables (&optional avoid-ranges)
  "Find all markdown tables in the buffer.
Returns a list of alists with :start, :end, :separator-row, and :rows.
AVOID-RANGES is a list of (start . end) cons to skip."
  (let ((tables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-overlays--table-line-regexp nil t)
        (let ((in-avoided nil))
          ;; Check if we're in an avoided range
          (dolist (range avoid-ranges)
            (when (and (>= (line-beginning-position) (car range))
                       (<= (line-beginning-position) (cdr range)))
              (setq in-avoided t)))
          (unless in-avoided
            ;; Found potential table start, scan for full table
            (goto-char (line-beginning-position))
            (let ((table-start (line-beginning-position))
                  (table-end nil)
                  (separator-row nil)
                  (rows '())
                  (row-num 0))
              ;; Collect all consecutive table lines
              (while (and (not (eobp))
                          (looking-at markdown-overlays--table-line-regexp))
                (let ((row-start (point))
                      (is-sep (looking-at markdown-overlays--table-separator-regexp)))
                  (when (and (not separator-row) is-sep)
                    (setq separator-row row-num))
                  (push (list (cons :start row-start)
                              (cons :end (line-end-position))
                              (cons :num row-num)
                              (cons :separator is-sep))
                        rows)
                  (setq row-num (1+ row-num))
                  (setq table-end (line-end-position)))
                (forward-line 1))
              ;; Only count as table if we have at least 2 rows
              (when (>= (length rows) 2)
                (push (list (cons :start table-start)
                            (cons :end table-end)
                            (cons :separator-row separator-row)
                            (cons :rows (nreverse rows)))
                      tables)))))))
    (nreverse tables)))

;;; Row parsing

(defun markdown-overlays--parse-table-row (start end)
  "Parse a table row between START and END into cells.
Returns a list of alists with :start, :end, :content for each cell.
Pipes inside backtick code spans are not treated as delimiters."
  (let ((cells '()))
    (save-excursion
      (goto-char start)
      ;; Skip leading whitespace and pipe
      (when (looking-at (rx (* (any " \t")) "|"))
        (goto-char (match-end 0)))
      (let ((cell-start (point))
            (in-code nil))
        ;; Scan character by character to respect code spans
        (while (< (point) end)
          (let ((ch (char-after)))
            (cond
             ((eq ch ?\`)
              (setq in-code (not in-code))
              (forward-char 1))
             ((and (eq ch ?|) (not in-code))
              (let ((cell-end (point)))
                (push (list (cons :start cell-start)
                            (cons :end cell-end)
                            (cons :content (string-trim
                                            (buffer-substring-no-properties cell-start cell-end))))
                      cells)
                (forward-char 1)
                (setq cell-start (point))))
             ((eq ch ?\\)
              ;; Skip escaped character (e.g. \|)
              (forward-char (min 2 (- end (point)))))
             (t (forward-char 1)))))))
    (nreverse cells)))

;;; Inline Markdown Processing for Table Cells

(defun markdown-overlays--apply-face-to-unpropertized (str face)
  "Apply FACE to characters in STR lacking a `face' property.
Characters that already have a `face' property are left untouched."
  (let ((result (copy-sequence str))
        (i 0)
        (len (length str)))
    (while (< i len)
      (unless (get-text-property i 'face result)
        (put-text-property i (1+ i) 'face face result))
      (setq i (1+ i)))
    result))

(defun markdown-overlays--replace-markup (str regex groups face
                                              &optional nestable prefix-group)
  "Replace REGEX matches in STR, applying FACE to captured text.
GROUPS is a list of capture group numbers to try; the first non-nil
match provides the inner text whose delimiters are removed.

When NESTABLE is non-nil, FACE is layered on top of any existing face
using `add-face-text-property' (for italic/strikethrough inside bold).
Otherwise, matches inside already-propertized regions are skipped
entirely (protecting code spans from further processing).

When PREFIX-GROUP is non-nil, that group's text is preserved verbatim
before the styled text (used for italic's lookbehind character)."
  (let ((new-result "")
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (existing (get-text-property match-start 'face str))
             (protected (and existing
                             (if nestable
                                 (not (memq existing '(bold italic)))
                               t))))
        (if protected
            ;; Inside a protected region — emit verbatim and skip past.
            (let ((prop-end (next-single-property-change
                             match-start 'face str (length str))))
              (setq new-result (concat new-result
                                       (substring str pos prop-end)))
              (setq pos prop-end))
          ;; Extract inner text from first non-nil capture group
          (let* ((inner (seq-some (lambda (g) (match-string g str)) groups))
                 (prefix (if prefix-group (or (match-string prefix-group str) "") ""))
                 (styled (if nestable
                             (let ((s (copy-sequence inner)))
                               (add-face-text-property 0 (length s) face t s)
                               s)
                           (markdown-overlays--apply-face-to-unpropertized
                            inner face))))
            (setq new-result (concat new-result
                                     (substring str pos match-start)
                                     prefix styled))
            (setq pos match-end)))))
    (concat new-result (substring str pos))))

(defun markdown-overlays--process-cell-content (content)
  "Process markdown syntax in CONTENT string, returning propertized string.
Handles: links [text](url), bold **text**/__text__, italic *text*/_text_,
bold-italic ***text***, inline code `text`, and strikethrough ~~text~~."
  (let ((result content))
    ;; Process inline code FIRST so its contents are protected from
    ;; bold/italic processing (e.g., `**text**` should render as code).
    (setq result (markdown-overlays--replace-markup
                  result (rx "`" (group (+ (not (any "`")))) "`")
                  '(1) 'font-lock-doc-markup-face))

    ;; Links need special handling for keymap.
    ;; Skip matches inside already-propertized regions (e.g. inline code).
    (let ((link-re (rx "[" (group (+ (not (any "]")))) "]("
                       (group (+ (not (any ")")))) ")"))
          (new-result "")
          (pos 0))
      (while (string-match link-re result pos)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          (if (get-text-property match-start 'face result)
              (let ((prop-end (next-single-property-change
                               match-start 'face result (length result))))
                (setq new-result (concat new-result
                                         (substring result pos prop-end)))
                (setq pos prop-end))
            (let ((title (match-string 1 result))
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

    ;; Bold-italic, bold
    (setq result (markdown-overlays--replace-markup
                  result (rx "***" (group (+ (not (any "*")))) "***")
                  '(1) '(:weight bold :slant italic)))
    (setq result (markdown-overlays--replace-markup
                  result (rx (or (seq "**" (group (+? anything)) "**")
                                 (seq "__" (group (+ (not (any "_")))) "__")))
                  '(1 2) 'bold))
    ;; Italic: nestable inside bold, with lookbehind for escaped \*text\*
    (setq result (markdown-overlays--replace-markup
                  result (rx (group (or string-start (not (any "\\"))))
                             (or (seq "*" (group (+ (not (any "*")))) "*")
                                 (seq "_" (group (+ (not (any "_")))) "_")))
                  '(2 3) 'italic t 1))
    ;; Strikethrough: nestable inside bold/italic
    (setq result (markdown-overlays--replace-markup
                  result (rx "~~" (group (+? anything)) "~~")
                  '(1) '(:strike-through t) t))

    ;; Scale tall characters to prevent uneven row heights
    (setq result (markdown-overlays--table-apply-height-scaling result))

    result))

;;; Glyph Height Normalization
;;
;; Some glyphs (color emoji, CJK ideographs) render taller than the
;; default line height, causing uneven row heights and gaps in table
;; borders.  We detect tall characters by measuring their actual line
;; height and scale them with (display (height N)) to match the
;; default.  `string-pixel-width' respects this display property, so
;; pad-string and display-width get correct widths automatically.

(defvar markdown-overlays--table-default-line-height nil
  "Cached default line height in pixels.
Computed once per session by `markdown-overlays--table-char-height-scale'.")

(defconst markdown-overlays--table-min-height-scale 0.75
  "Minimum height scale factor.
Characters needing more aggressive scaling than this are left
unscaled — shrinking text below 75% makes it unreadable.  This
allows emoji (~0.77) and CJK (~0.90) through while skipping
scripts with tall ascenders/descenders like Arabic (~0.63).")

(defvar markdown-overlays--table-height-scale-cache (make-hash-table :test 'eq)
  "Cache of height scale factors keyed by character.")

(defun markdown-overlays--table-char-height-scale (char)
  "Return the display height scale needed for CHAR, or nil if none.
Measures actual line height and returns a scale factor that brings
it down to the default line height.  Results are cached."
  (let ((cached (gethash char markdown-overlays--table-height-scale-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((scale
             (let ((win (selected-window))
                   (orig-buf (window-buffer)))
               (unwind-protect
                   (let ((default-h
                           (or markdown-overlays--table-default-line-height
                               (setq markdown-overlays--table-default-line-height
                                     (with-temp-buffer
                                       (set-window-buffer win (current-buffer))
                                       (insert "A\n")
                                       (cdr (window-text-pixel-size
                                             win 1 3))))))
                          (char-h (with-temp-buffer
                                    (set-window-buffer win (current-buffer))
                                    (insert (string char) "\n")
                                    (cdr (window-text-pixel-size
                                          win 1 3)))))
                     (when (> char-h default-h)
                       ;; Binary search for highest scale ≤ default-h
                       (let ((lo 0.5) (hi 1.0) (best 0.5))
                         (dotimes (_ 10)
                           (let* ((mid (/ (+ lo hi) 2.0))
                                  (h (with-temp-buffer
                                       (set-window-buffer win
                                                          (current-buffer))
                                       (insert (propertize
                                                (string char)
                                                'display
                                                (list 'height mid))
                                               "\n")
                                       (cdr (window-text-pixel-size
                                             win 1 3)))))
                             (if (<= h default-h)
                                 (setq best mid lo mid)
                               (setq hi mid))))
                         ;; Skip if scaling would shrink too aggressively
                         (when (>= best markdown-overlays--table-min-height-scale)
                           best))))
                 (set-window-buffer win orig-buf)))))
        (puthash char scale markdown-overlays--table-height-scale-cache)
        scale))))

(defun markdown-overlays--table-apply-height-scaling (str)
  "Add display height scaling to any tall characters in STR.
Returns a new string with (display (height N)) on glyphs that
would otherwise cause uneven row heights.  Handles emoji, CJK,
and any other characters that render taller than the default."
  (let ((result (copy-sequence str))
        (len (length str)))
    (dotimes (i len)
      (let* ((ch (aref result i))
             (scale (markdown-overlays--table-char-height-scale ch)))
        ;; Also scale base char before a variation selector
        (unless scale
          (when (and (< (1+ i) len)
                     (= (aref result (1+ i)) #xFE0F))
            (setq scale (markdown-overlays--table-char-height-scale #xFE0F))))
        (when scale
          (put-text-property i (1+ i) 'display
                             `(height ,scale)
                             result))))
    result))

;;; Column Width Computation

(defun markdown-overlays--table-display-width (str)
  "Return display width of STR in character units.
Uses pixel measurements to detect characters that render wider than
`string-width' reports (e.g., emoji).  `string-pixel-width' respects
display properties like (height N) set by height scaling.
Falls back to `string-width' if `string-pixel-width' is unavailable."
  (if (fboundp 'string-pixel-width)
      (let ((char-px (string-pixel-width " "))
            (actual-px (string-pixel-width str)))
        (ceiling (/ (float actual-px) char-px)))
    (string-width str)))

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
            (let ((processed (markdown-overlays--process-cell-content
                              (map-elt cell :content))))
              (if (nth col widths)
                  (setf (nth col widths)
                        (max (nth col widths)
                             (markdown-overlays--table-display-width processed)))
                ;; Extend widths list
                (setq widths (append widths
                                     (list (markdown-overlays--table-display-width processed)))))
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
            (let ((processed (markdown-overlays--process-cell-content
                              (map-elt cell :content))))
              (if (nth col min-widths)
                  (setf (nth col min-widths)
                        (max (nth col min-widths)
                             (markdown-overlays--table-longest-word processed)))
                (setq min-widths (append min-widths
                                         (list (markdown-overlays--table-longest-word processed)))))
              (setq col (1+ col)))))))
    min-widths))

(defun markdown-overlays--table-longest-word (str)
  "Return display width of longest word in STR."
  (if (or (null str) (string-empty-p str))
      0
    (seq-max (seq-map #'markdown-overlays--table-display-width
                      (split-string str "[ \t\n]+" t)))))

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

;;; Text Wrapping (preserves text properties)

(defun markdown-overlays--table-wrap-text (text width)
  "Wrap TEXT to fit within WIDTH, returning list of lines.
Preserves text properties across wrapped lines."
  (if (or (null text) (string-empty-p text))
      (list "")
    (if (<= (markdown-overlays--table-display-width text) width)
        (list text)
      ;; Wrap using display-width-aware measurement
      (let ((lines '())
            (pos 0)
            (len (length text)))
        (while (< pos len)
          ;; Find end position where display-width reaches column width
          (let ((end-pos pos)
                (line-width 0))
            (while (and (< end-pos len) (<= line-width width))
              (let ((ch-dw (markdown-overlays--table-display-width
                            (substring text end-pos (1+ end-pos)))))
                (if (<= (+ line-width ch-dw) width)
                    (progn (setq end-pos (1+ end-pos))
                           (setq line-width (+ line-width ch-dw)))
                  (setq line-width (1+ width))))) ;; break
            (when (>= end-pos len) (setq end-pos len))
            ;; Find a good break point (space) if not at end
            (let* ((break-pos (if (>= end-pos len)
                                  end-pos
                                (let ((space-pos nil))
                                  (save-match-data
                                    (when (string-match (rx (* nonl) (syntax whitespace))
                                                        (substring text pos end-pos))
                                      (setq space-pos (+ pos (match-end 0)))))
                                  (or space-pos end-pos))))
                   (line (substring text pos break-pos)))
              (setq line (string-trim-right line))
              (push line lines)
              (setq pos break-pos)
              (while (and (< pos len)
                          (memq (aref text pos) '(?\s ?\t)))
                (setq pos (1+ pos))))))
        (nreverse lines)))))

(defun markdown-overlays--pad-string (str width)
  "Pad STR with spaces to reach WIDTH.
Uses pixel measurements when available to compensate for characters
that render wider than `string-width' reports (e.g., emoji)."
  (if (fboundp 'string-pixel-width)
      (let* ((char-px (string-pixel-width " "))
             (target-px (* width char-px))
             (actual-px (string-pixel-width str))
             (pad-px (- target-px actual-px)))
        (if (<= pad-px 0)
            str
          (let* ((full-spaces (floor (/ (float pad-px) char-px)))
                 (remaining (/ (- (float pad-px) (* full-spaces char-px)) char-px)))
            (concat str
                    (make-string full-spaces ?\s)
                    (if (> remaining 0.01)
                        (propertize " " 'display `(space :width ,remaining))
                      "")))))
    ;; Fallback for older Emacs
    (let ((current-width (string-width str)))
      (if (>= current-width width)
          str
        (concat str (make-string (- width current-width) ?\s))))))

(defun markdown-overlays--make-table-separator-cell (width)
  "Create a separator cell string of dashes for WIDTH.
For WIDTH=5, return \"─────\"."
  (if markdown-overlays--table-use-unicode-borders
      (make-string width (string-to-char markdown-overlays--table-border-dash))
    (make-string width ?-)))

;;; Main Table Alignment

(defun markdown-overlays--align-table (table)
  "Apply display overlays to align TABLE columns.
If `markdown-overlays--table-wrap-columns' is non-nil and table is too wide,
columns are wrapped to fit within window width.

Before: | Name | Role |       After: │ Name  │ Role     │
        |------|------|              ├───────┼──────────┤
        | Alice | Engineer |        │ Alice │ Engineer │"
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
                                 (if (looking-at (rx line-start (* (any " \t"))))
                                     (match-string 0) "")))
                   (row-display
                    (concat
                     leading-ws
                     (propertize pipe-left 'face markdown-overlays--table-border-face)
                     (mapconcat
                      (lambda (w)
                        (propertize (markdown-overlays--make-table-separator-cell (+ w 2))
                                    'face markdown-overlays--table-border-face))
                      col-widths
                      (propertize pipe 'face markdown-overlays--table-border-face))
                     (propertize pipe-right 'face markdown-overlays--table-border-face)))
                   (ov (make-overlay row-start row-end)))
              ;; Use invisible+before-string so text properties in
              ;; row-display (e.g. fractional-width spaces) are honoured.
              (let ((lp (get-text-property row-start 'line-prefix)))
                (when lp
                  (put-text-property 0 (length row-display) 'line-prefix lp row-display)))
              (markdown-overlays--put ov
                                     'evaporate t
                                     'invisible 'markdown-overlays-tables
                                     'before-string row-display))

          ;; Content row — unified approach handles both simple and wrapped cases
          (let* ((wrapped-cells
                  (seq-map-indexed
                   (lambda (cell idx)
                     (let ((width (or (nth idx col-widths) 10))
                            (processed (markdown-overlays--process-cell-content
                                        (map-elt cell :content))))
                       (markdown-overlays--table-wrap-text processed width)))
                   cells))
                 (max-lines (max 1 (seq-max (seq-map #'length wrapped-cells))))
                 (pipe (if markdown-overlays--table-use-unicode-borders
                           markdown-overlays--table-border-pipe "|"))
                 (styled-pipe (propertize pipe 'face markdown-overlays--table-border-face))
                 ;; Preserve leading whitespace
                 (leading-ws (save-excursion
                               (goto-char row-start)
                               (if (looking-at (rx line-start (* (any " \t"))))
                                   (match-string 0) "")))
                 ;; Build multi-line display string for entire row
                 (row-display
                  (mapconcat
                   (lambda (line-idx)
                     (concat
                      leading-ws
                      styled-pipe
                      (string-join
                       (seq-map-indexed
                        (lambda (_cell col-idx)
                            (let* ((cell-lines (nth col-idx wrapped-cells))
                                   (width (nth col-idx col-widths))
                                   (line (or (nth line-idx cell-lines) ""))
                                   (padded (concat " "
                                                   (markdown-overlays--pad-string line width)
                                                   " "))
                                   (face (cond
                                          (is-header markdown-overlays--table-header-face)
                                          (is-zebra markdown-overlays--table-zebra-face)
                                          (t markdown-overlays--table-row-face))))
                              ;; Use add-face-text-property to preserve inline formatting
                              (add-face-text-property 0 (length padded) face t padded)
                              padded))
                          cells)
                         styled-pipe)
                        styled-pipe))
                   (number-sequence 0 (1- max-lines))
                   "\n"))
                 ;; Create overlay for entire row (including pipes)
                 (ov (make-overlay row-start row-end)))
            ;; Use invisible+before-string so text properties in
            ;; row-display (e.g. fractional-width spaces) are honoured.
            ;; Propagate line-prefix so wrapped lines indent correctly
            ;; even for the last row where buffer properties may differ.
            (let ((lp (get-text-property row-start 'line-prefix)))
              (when lp
                (put-text-property 0 (length row-display) 'line-prefix lp row-display)))
            (markdown-overlays--put ov
                                   'evaporate t
                                   'invisible 'markdown-overlays-tables
                                   'before-string row-display)))))))

(defun markdown-overlays--fontify-tables (tables)
  "Align all markdown TABLES using display overlays."
  (when (and markdown-overlays-prettify-tables tables)
    (add-to-invisibility-spec 'markdown-overlays-tables)
    (dolist (table tables)
      (markdown-overlays--align-table table))))

(provide 'markdown-overlays-tables)

;;; markdown-overlays-tables.el ends here
