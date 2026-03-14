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

(defvar-local markdown-overlays--table-cache (make-hash-table :test 'equal)
  "Hash table mapping table buffer text to cached overlay data.
Each value is a list of (ROW-START ROW-END BEFORE-STRING LINE-PREFIX)
entries, one per row, sufficient to re-apply overlays without
reprocessing.")

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
        (while (< (point) end)
          (if in-code
              ;; Inside code span — scan char-by-char for closing backtick.
              (if (eq (char-after) ?\`)
                  (progn (setq in-code nil) (forward-char 1))
                (forward-char 1))
            ;; Outside code — jump to next delimiter.
            (if (re-search-forward (rx (any "|`\\")) (line-end-position) t)
                (let ((ch (char-before)))
                  (cond
                   ((eq ch ?|)
                    (let ((cell-end (1- (point))))
                      (push (list (cons :start cell-start)
                                  (cons :end cell-end)
                                  (cons :content (string-trim
                                                  (buffer-substring-no-properties
                                                   cell-start cell-end))))
                            cells)
                      (setq cell-start (point))))
                   ((eq ch ?\`)
                    (setq in-code t))
                   ((eq ch ?\\)
                    ;; Skip escaped character
                    (when (< (point) end) (forward-char 1)))))
              (goto-char end))))))
    (nreverse cells)))

;;; Inline Markdown Processing for Table Cells

(defun markdown-overlays--apply-face-to-unpropertized (str face)
  "Apply FACE to characters in STR lacking a `face' property.
Characters that already have a `face' property are left untouched."
  (let ((result (copy-sequence str))
        (len (length str))
        (pos 0))
    (while (< pos len)
      (let ((next (next-single-property-change pos 'face result len)))
        (unless (get-text-property pos 'face result)
          (put-text-property pos next 'face face result))
        (setq pos next)))
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
  (let ((parts nil)
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
              (push (substring str pos prop-end) parts)
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
            (push (substring str pos match-start) parts)
            (unless (string-empty-p prefix)
              (push prefix parts))
            (push styled parts)
            (setq pos match-end)))))
    (push (substring str pos) parts)
    (apply #'concat (nreverse parts))))

(defun markdown-overlays--process-cell-content (content)
  "Process markdown syntax in CONTENT string, returning propertized string.
Handles: links [text](url), bold **text**/__text__, italic *text*/_text_,
bold-italic ***text***, inline code `text`, and strikethrough ~~text~~."
  ;; Skip all regex processing for plain cells (no markdown syntax).
  (if (string-match-p (rx (any "*`~[_")) content)
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
            (parts nil)
            (pos 0))
        (while (string-match link-re result pos)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (if (get-text-property match-start 'face result)
                (let ((prop-end (next-single-property-change
                                 match-start 'face result (length result))))
                  (push (substring result pos prop-end) parts)
                  (setq pos prop-end))
              (let ((title (match-string 1 result))
                    (url (match-string 2 result))
                    (link-map (make-sparse-keymap)))
                (push (substring result pos match-start) parts)
                (define-key link-map [mouse-1]
                            (lambda () (interactive) (browse-url url)))
                (define-key link-map (kbd "RET")
                            (lambda () (interactive) (browse-url url)))
                (push (propertize title
                                  'face 'link
                                  'mouse-face 'highlight
                                  'keymap link-map
                                  'help-echo url)
                      parts)
                (setq pos match-end)))))
        (push (substring result pos) parts)
        (setq result (apply #'concat (nreverse parts))))

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

      result)
    ;; Plain text — skip all regex processing.
    (markdown-overlays--table-apply-height-scaling content)))

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
    (if (eq cached 'miss)
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
        scale)
      cached)))

(defun markdown-overlays--table-apply-height-scaling (str)
  "Add display height scaling to any tall characters in STR.
Returns a new string with (display (height N)) on glyphs that
would otherwise cause uneven row heights.  Handles emoji, CJK,
and any other characters that render taller than the default."
  ;; ASCII characters never render taller than the default line height,
  ;; so skip the per-character scaling loop for ASCII-only strings.
  (if (string-match-p (rx bos (* ascii) eos) str)
      str
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
      result)))

;;; Column Width Computation

(defvar markdown-overlays--table-char-pixel-width nil
  "Cached pixel width of a single space character.")

(defun markdown-overlays--table-display-width (str)
  "Return display width of STR in character units.
Uses pixel measurements to detect characters that render wider than
`string-width' reports (e.g., emoji).  `string-pixel-width' respects
display properties like (height N) set by height scaling.
Falls back to `string-width' if `string-pixel-width' is unavailable."
  ;; ASCII characters render at exactly 1 × space-pixel-width, so
  ;; `string-width' matches pixel measurement.  Non-ASCII (emoji, CJK)
  ;; can render wider, requiring expensive `string-pixel-width'.
  (if (and (fboundp 'string-pixel-width)
           (not (string-match-p (rx bos (* ascii) eos) str)))
      (let ((char-px (or markdown-overlays--table-char-pixel-width
                         (setq markdown-overlays--table-char-pixel-width
                               (string-pixel-width " "))))
            (actual-px (string-pixel-width str)))
        (ceiling (/ (float actual-px) char-px)))
    (string-width str)))

(defun markdown-overlays--preprocess-table (table)
  "Parse and process all cells in TABLE in a single pass.
Returns a plist with:
  :natural-widths  - max display width per column
  :min-widths      - longest word width per column
  :processed-rows  - list of (row-meta . processed-cells) for each row
where each processed-cell is the propertized string from `process-cell-content'."
  (let ((widths nil)
        (min-widths nil)
        (processed-rows nil))
    (dolist (row (map-elt table :rows))
      (if (map-elt row :separator)
          (push (cons row nil) processed-rows)
        (let ((cells (markdown-overlays--parse-table-row
                      (map-elt row :start)
                      (map-elt row :end)))
              (col 0)
              (processed-cells nil))
          (dolist (cell cells)
            (let* ((processed (markdown-overlays--process-cell-content
                               (map-elt cell :content)))
                   (dw (markdown-overlays--table-display-width processed))
                   (mw (markdown-overlays--table-longest-word processed)))
              (push processed processed-cells)
              (if (nth col widths)
                  (progn
                    (setf (nth col widths) (max (nth col widths) dw))
                    (setf (nth col min-widths) (max (nth col min-widths) mw)))
                (setq widths (append widths (list dw)))
                (setq min-widths (append min-widths (list mw))))
              (setq col (1+ col))))
          (push (cons row (nreverse processed-cells)) processed-rows))))
    (list :natural-widths widths
          :min-widths min-widths
          :processed-rows (nreverse processed-rows))))

(defun markdown-overlays--table-longest-word (str)
  "Return display width of longest word in STR."
  (if (or (null str) (string-empty-p str))
      0
    ;; ASCII renders at exactly 1 × space-pixel-width per char,
    ;; so cheap `string-width' matches pixel measurement per word.
    (let ((words (split-string str "[ \t\n]+" t)))
      (if words
          (apply #'max (mapcar (if (string-match-p (rx bos (* ascii) eos) str)
                                   #'string-width
                                 #'markdown-overlays--table-display-width)
                               words))
        0))))

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
  ;; ASCII characters render at exactly 1 × space-pixel-width, so
  ;; column-based padding is pixel-perfect.  Non-ASCII (emoji, CJK)
  ;; can render wider, requiring pixel-based padding with fractional spaces.
  (if (and (fboundp 'string-pixel-width)
           (not (string-match-p (rx bos (* ascii) eos) str)))
      (let* ((char-px (or markdown-overlays--table-char-pixel-width
                          (setq markdown-overlays--table-char-pixel-width
                                (string-pixel-width " "))))
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
    ;; ASCII-only or older Emacs — string-width is accurate.
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
  (let* ((preprocessed (markdown-overlays--preprocess-table table))
         (natural-widths (plist-get preprocessed :natural-widths))
         (min-widths (plist-get preprocessed :min-widths))
         (processed-rows (plist-get preprocessed :processed-rows))
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
         (data-row-num 0))  ; Track data rows for zebra striping

    (dolist (row-entry processed-rows)
      (let* ((row (car row-entry))
             (processed-cells (cdr row-entry))
             (row-start (map-elt row :start))
             (row-end (map-elt row :end))
             (row-num (map-elt row :num))
             (is-separator (map-elt row :separator))
             (is-header (and separator-row (< row-num separator-row)))
             (is-zebra (and markdown-overlays--table-zebra-stripe
                            (not is-header)
                            (not is-separator)
                            (= (mod data-row-num 2) 1))))

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

          ;; Content row — use pre-processed cell content
          (let* ((ncols (length processed-cells))
                 (wrapped-cells-vec
                  (let ((idx 0)
                        (vec (make-vector ncols nil)))
                    (dolist (processed processed-cells)
                      (aset vec idx
                            (vconcat
                             (markdown-overlays--table-wrap-text
                              processed (or (nth idx col-widths) 10))))
                      (setq idx (1+ idx)))
                    vec))
                 (col-widths-vec (vconcat col-widths))
                 (max-lines (let ((m 0) (i 0))
                                     (while (< i ncols)
                                       (setq m (max m (length (aref wrapped-cells-vec i)))
                                             i (1+ i)))
                                     (max 1 m)))
                 (pipe (if markdown-overlays--table-use-unicode-borders
                           markdown-overlays--table-border-pipe "|"))
                 (styled-pipe (propertize pipe 'face markdown-overlays--table-border-face))
                 ;; Preserve leading whitespace
                 (leading-ws (save-excursion
                               (goto-char row-start)
                               (if (looking-at (rx line-start (* (any " \t"))))
                                   (match-string 0) "")))
                 (face (cond
                        (is-header markdown-overlays--table-header-face)
                        (is-zebra markdown-overlays--table-zebra-face)
                        (t markdown-overlays--table-row-face)))
                 ;; Build multi-line display string for entire row
                 (row-display
                  (let ((lines nil))
                    (dotimes (line-idx max-lines)
                      (let ((parts nil))
                        (dotimes (col-idx ncols)
                          (let* ((cell-lines (aref wrapped-cells-vec col-idx))
                                 (line (if (< line-idx (length cell-lines))
                                           (aref cell-lines line-idx) ""))
                                 (padded (concat " "
                                                 (markdown-overlays--pad-string
                                                  line (aref col-widths-vec col-idx))
                                                 " ")))
                            ;; Use add-face-text-property to preserve inline formatting
                            (add-face-text-property 0 (length padded) face t padded)
                            (push padded parts)))
                        (push (concat leading-ws styled-pipe
                                      (string-join (nreverse parts) styled-pipe)
                                      styled-pipe)
                              lines)))
                    (mapconcat #'identity (nreverse lines) "\n")))
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
  "Align all markdown TABLES using display overlays.
Uses a content-based cache to skip reprocessing unchanged tables."
  (when-let (((and markdown-overlays-prettify-tables tables))
             (new-cache (make-hash-table :test 'equal)))
    (unless (memq 'markdown-overlays-tables
                  (if (listp buffer-invisibility-spec)
                      buffer-invisibility-spec))
      (add-to-invisibility-spec 'markdown-overlays-tables))
    (dolist (table tables)
      (let ((key (buffer-substring-no-properties (map-elt table :start)
                                                 (map-elt table :end))))
        (if-let* ((cached (map-elt markdown-overlays--table-cache key)))
            ;; Table unchanged, re-apply cached overlays.
            (progn
              (dolist (entry cached)
                (let* ((before-string (map-elt entry :before-string))
                       (line-prefix (map-elt entry :line-prefix))
                       (ov (make-overlay (map-elt entry :start) (map-elt entry :end))))
                  (when line-prefix
                    (put-text-property 0 (length before-string) 'line-prefix line-prefix before-string))
                  (markdown-overlays--put
                   ov
                   'evaporate t
                   'invisible 'markdown-overlays-tables
                   'before-string before-string)))
              (map-put! new-cache key cached))
          ;; Table is new or changed, full processing.
          (markdown-overlays--align-table table)
          ;; Collect the overlays created for this table region.
          (let ((entries nil))
            (dolist (ov (overlays-in (map-elt table :start) (map-elt table :end)))
              (when (eq (overlay-get ov 'invisible) 'markdown-overlays-tables)
                (push `((:start . ,(overlay-start ov))
                        (:end . ,(overlay-end ov))
                        (:before-string . ,(copy-sequence (overlay-get ov 'before-string)))
                        (:line-prefix . ,(get-text-property 0 'line-prefix
                                                            (overlay-get ov 'before-string))))
                      entries)))
            (map-put! new-cache key entries)))))
    ;; Replace old cache — entries for deleted tables are dropped.
    (setq markdown-overlays--table-cache new-cache)))


(provide 'markdown-overlays-tables)

;;; markdown-overlays-tables.el ends here
