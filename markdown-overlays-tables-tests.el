;;; markdown-overlays-tables-tests.el --- Tests for markdown-overlays-tables  -*- lexical-binding: t -*-

;; Run with: emacs --batch -l ert -l markdown-overlays-tables.el -l markdown-overlays-tables-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'markdown-overlays-tables)

;;; Helpers

(defun markdown-overlays-tables-tests--face-at (str pos)
  "Return the face at POS in propertized string STR."
  (get-text-property pos 'face str))

(defun markdown-overlays-tables-tests--face-range (str face start end)
  "Return non-nil if every char in STR from START to END has FACE."
  (let ((ok t)
        (i start))
    (while (and ok (< i (min end (length str))))
      (unless (equal (get-text-property i 'face str) face)
        (setq ok nil))
      (setq i (1+ i)))
    ok))

;;; Basic formatting

(ert-deftest markdown-overlays-tables-test-bold ()
  (let ((r (markdown-overlays--process-cell-content "**bold**")))
    (should (string= (substring-no-properties r) "bold"))
    (should (markdown-overlays-tables-tests--face-range r 'bold 0 4))))

(ert-deftest markdown-overlays-tables-test-italic ()
  (let ((r (markdown-overlays--process-cell-content "*italic*")))
    (should (string= (substring-no-properties r) "italic"))
    (should (markdown-overlays-tables-tests--face-range r 'italic 0 6))))

(ert-deftest markdown-overlays-tables-test-bold-italic ()
  (let ((r (markdown-overlays--process-cell-content "***bi***")))
    (should (string= (substring-no-properties r) "bi"))
    (should (markdown-overlays-tables-tests--face-range
             r '(:weight bold :slant italic) 0 2))))

(ert-deftest markdown-overlays-tables-test-inline-code ()
  (let ((r (markdown-overlays--process-cell-content "`code`")))
    (should (string= (substring-no-properties r) "code"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 4))))

(ert-deftest markdown-overlays-tables-test-strikethrough ()
  (let ((r (markdown-overlays--process-cell-content "~~strike~~")))
    (should (string= (substring-no-properties r) "strike"))
    (should (markdown-overlays-tables-tests--face-range
             r '(:strike-through t) 0 6))))

(ert-deftest markdown-overlays-tables-test-underscore-bold ()
  (let ((r (markdown-overlays--process-cell-content "__bold__")))
    (should (string= (substring-no-properties r) "bold"))
    (should (markdown-overlays-tables-tests--face-range r 'bold 0 4))))

(ert-deftest markdown-overlays-tables-test-underscore-italic ()
  (let ((r (markdown-overlays--process-cell-content "_italic_")))
    (should (string= (substring-no-properties r) "italic"))
    (should (markdown-overlays-tables-tests--face-range r 'italic 0 6))))

(ert-deftest markdown-overlays-tables-test-plain-text ()
  (let ((r (markdown-overlays--process-cell-content "plain text")))
    (should (string= r "plain text"))
    (should-not (markdown-overlays-tables-tests--face-at r 0))))

;;; Mixed formatting

(ert-deftest markdown-overlays-tables-test-bold-and-italic ()
  (let ((r (markdown-overlays--process-cell-content "**bold** and *italic*")))
    (should (string= (substring-no-properties r) "bold and italic"))
    (should (markdown-overlays-tables-tests--face-range r 'bold 0 4))
    (should-not (markdown-overlays-tables-tests--face-at r 5))
    (should (markdown-overlays-tables-tests--face-range r 'italic 9 15))))

(ert-deftest markdown-overlays-tables-test-code-and-bold ()
  (let ((r (markdown-overlays--process-cell-content "`code` and **bold**")))
    (should (string= (substring-no-properties r) "code and bold"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 4))
    (should (markdown-overlays-tables-tests--face-range r 'bold 9 13))))

;;; Nesting: code protects inner markup

(ert-deftest markdown-overlays-tables-test-code-wrapping-bold-syntax ()
  "Backtick code containing **text** should preserve the asterisks."
  (let ((r (markdown-overlays--process-cell-content "`**text**`")))
    (should (string= (substring-no-properties r) "**text**"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 8))))

(ert-deftest markdown-overlays-tables-test-code-wrapping-italic-syntax ()
  "Backtick code containing *text* should preserve the asterisks."
  (let ((r (markdown-overlays--process-cell-content "`*text*`")))
    (should (string= (substring-no-properties r) "*text*"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 6))))

(ert-deftest markdown-overlays-tables-test-code-wrapping-strikethrough ()
  "Backtick code containing ~~text~~ should preserve the tildes."
  (let ((r (markdown-overlays--process-cell-content "`~~text~~`")))
    (should (string= (substring-no-properties r) "~~text~~"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 8))))

;;; Nesting: bold wrapping code

(ert-deftest markdown-overlays-tables-test-bold-wrapping-code ()
  "**bold with `code`** should render bold text + code-faced code."
  (let ((r (markdown-overlays--process-cell-content "**bold with `code`**")))
    (should (string= (substring-no-properties r) "bold with code"))
    (should (markdown-overlays-tables-tests--face-range r 'bold 0 10))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 10 14))))

;;; Italic edge cases

(ert-deftest markdown-overlays-tables-test-inline-italic ()
  "Italic in the middle of text should not eat surrounding spaces."
  (let ((r (markdown-overlays--process-cell-content "word *italic* end")))
    (should (string= (substring-no-properties r) "word italic end"))
    (should-not (markdown-overlays-tables-tests--face-at r 4))
    (should (markdown-overlays-tables-tests--face-range r 'italic 5 11))
    (should-not (markdown-overlays-tables-tests--face-at r 11))))

(ert-deftest markdown-overlays-tables-test-start-italic ()
  "Italic at the start of string."
  (let ((r (markdown-overlays--process-cell-content "*start italic*")))
    (should (string= (substring-no-properties r) "start italic"))
    (should (markdown-overlays-tables-tests--face-range r 'italic 0 12))))

(ert-deftest markdown-overlays-tables-test-escaped-asterisks ()
  "Escaped \\*text\\* should not be treated as italic."
  (let ((r (markdown-overlays--process-cell-content "\\*escaped\\*")))
    (should (string= (substring-no-properties r) "\\*escaped\\*"))
    (should-not (markdown-overlays-tables-tests--face-at r 0))))

;;; Links

(ert-deftest markdown-overlays-tables-test-link ()
  (let ((r (markdown-overlays--process-cell-content "[click](https://example.com)")))
    (should (string= (substring-no-properties r) "click"))
    (should (eq (markdown-overlays-tables-tests--face-at r 0) 'link))
    (should (string= (get-text-property 0 'help-echo r) "https://example.com"))))

;;; Edge cases from bug investigations

(ert-deftest markdown-overlays-tables-test-bold-wrapping-code-preserves-both ()
  "**bold `code`** — bold should not override code face (real bug fix)."
  (let ((r (markdown-overlays--process-cell-content "**bold `code`**")))
    (should (string= (substring-no-properties r) "bold code"))
    (should (markdown-overlays-tables-tests--face-range r 'bold 0 5))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 5 9))))

(ert-deftest markdown-overlays-tables-test-code-and-italic ()
  "`code` followed by *italic*."
  (let ((r (markdown-overlays--process-cell-content "`code` and *italic*")))
    (should (string= (substring-no-properties r) "code and italic"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 4))
    (should (markdown-overlays-tables-tests--face-range r 'italic 9 15))))

(ert-deftest markdown-overlays-tables-test-multiple-code-spans ()
  "Multiple inline code spans in one cell."
  (let ((r (markdown-overlays--process-cell-content "`a` and `b`")))
    (should (string= (substring-no-properties r) "a and b"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 1))
    (should-not (markdown-overlays-tables-tests--face-at r 2))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 6 7))))

(ert-deftest markdown-overlays-tables-test-empty-string ()
  "Empty string should pass through unchanged."
  (let ((r (markdown-overlays--process-cell-content "")))
    (should (string= r ""))))

(ert-deftest markdown-overlays-tables-test-whitespace-only ()
  "Whitespace-only string should pass through unchanged."
  (let ((r (markdown-overlays--process-cell-content "   ")))
    (should (string= r "   "))
    (should-not (markdown-overlays-tables-tests--face-at r 0))))

(ert-deftest markdown-overlays-tables-test-code-wrapping-link-syntax ()
  "Backtick code containing [text](url) should preserve as code, not link."
  (let ((r (markdown-overlays--process-cell-content "`[text](url)`")))
    (should (string= (substring-no-properties r) "[text](url)"))
    (should (markdown-overlays-tables-tests--face-range
             r 'font-lock-doc-markup-face 0 11))))

;;; Table finding

(ert-deftest markdown-overlays-tables-test-find-tables ()
  "Should find a simple markdown table in buffer text."
  (with-temp-buffer
    (insert "Some text\n\n| A | B |\n|---|---|\n| 1 | 2 |\n\nMore text\n")
    (let ((tables (markdown-overlays--find-tables nil)))
      (should (= (length tables) 1))
      (let ((table (car tables)))
        (should (= (length (map-elt table :rows)) 3))))))

(ert-deftest markdown-overlays-tables-test-find-tables-avoids-source-blocks ()
  "Tables inside source block ranges should be excluded."
  (with-temp-buffer
    (insert "| A | B |\n|---|---|\n| 1 | 2 |\n")
    (let* ((avoid (list (cons (point-min) (point-max))))
           (tables (markdown-overlays--find-tables avoid)))
      (should (= (length tables) 0)))))

(ert-deftest markdown-overlays-tables-test-find-tables-alignment ()
  "Tables with alignment indicators (:---, :---:, ---:) should parse."
  (with-temp-buffer
    (insert "| L | C | R |\n|:---|:---:|---:|\n| a | b | c |\n")
    (let ((tables (markdown-overlays--find-tables nil)))
      (should (= (length tables) 1))
      (let ((table (car tables)))
        (should (= (length (map-elt table :rows)) 3))))))

(ert-deftest markdown-overlays-tables-test-pipe-in-code-span ()
  "Pipes inside backtick code spans should not split cells."
  (with-temp-buffer
    (insert "| A | `x \\| y` | B |\n|---|---|---|\n| 1 | 2 | 3 |\n")
    (let ((tables (markdown-overlays--find-tables nil)))
      (should (= (length tables) 1))
      (let* ((table (car tables))
             (rows (map-elt table :rows))
             (header (car rows))
             (cells (markdown-overlays--parse-table-row
                     (map-elt header :start) (map-elt header :end))))
        (should (= (length cells) 3))
        (should (string= (string-trim (map-elt (nth 1 cells) :content)) "`x \\| y`"))))))

(ert-deftest markdown-overlays-tables-test-escaped-pipe ()
  "Escaped pipes (backslash-pipe) should not split cells."
  (with-temp-buffer
    (insert "| A | x \\| y | B |\n|---|---|---|\n| 1 | 2 | 3 |\n")
    (let* ((tables (markdown-overlays--find-tables nil))
           (table (car tables))
           (header (car (map-elt table :rows)))
           (cells (markdown-overlays--parse-table-row
                   (map-elt header :start) (map-elt header :end))))
      (should (= (length cells) 3))
      (should (string= (string-trim (map-elt (nth 1 cells) :content)) "x \\| y")))))

(ert-deftest markdown-overlays-tables-test-nested-bold-italic ()
  "Bold wrapping italic (**bold *italic* bold**) should apply both faces."
  (let ((r (markdown-overlays--process-cell-content "**bold with *italic* inside**")))
    (should (string= (substring-no-properties r) "bold with italic inside"))
    ;; "bold" region should have bold face
    (should (equal (get-text-property 0 'face r) 'bold))
    ;; "italic" region (chars 10-15) should have both bold and italic
    (should (memq 'italic (ensure-list (get-text-property 10 'face r))))
    (should (memq 'bold (ensure-list (get-text-property 10 'face r))))))

(ert-deftest markdown-overlays-tables-test-strikethrough-wrapping-bold ()
  "Strikethrough wrapping bold (~~struck **bold** struck~~) keeps both."
  (let ((r (markdown-overlays--process-cell-content "~~struck **bold** struck~~")))
    (should (string= (substring-no-properties r) "struck bold struck"))
    ;; "struck" has strikethrough
    (should (equal (get-text-property 0 'face r) '(:strike-through t)))
    ;; "bold" has both bold and strikethrough
    (let ((face (get-text-property 7 'face r)))
      (should (memq 'bold (ensure-list face)))
      (should (member '(:strike-through t) (ensure-list face))))))

(ert-deftest markdown-overlays-tables-test-bold-wrapping-strikethrough ()
  "Bold wrapping strikethrough (**bold ~~struck~~ bold**) keeps both."
  (let ((r (markdown-overlays--process-cell-content "**bold ~~struck~~ bold**")))
    (should (string= (substring-no-properties r) "bold struck bold"))
    ;; "bold" has bold face
    (should (equal (get-text-property 0 'face r) 'bold))
    ;; "struck" has both bold and strikethrough
    (let ((face (get-text-property 5 'face r)))
      (should (memq 'bold (ensure-list face)))
      (should (member '(:strike-through t) (ensure-list face))))))

(provide 'markdown-overlays-tables-tests)

;;; markdown-overlays-tables-tests.el ends here
