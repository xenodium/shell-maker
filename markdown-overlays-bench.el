;;; markdown-overlays-bench.el --- Benchmark for markdown overlay rendering  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Usage:
;;   emacs --batch -L . -l markdown-overlays-bench -f markdown-overlays-bench-run
;;
;; Or from a running Emacs:
;;   (load-file "markdown-overlays-bench.el")
;;   (markdown-overlays-bench-run)

;;; Code:

(require 'markdown-overlays-tables)
(require 'markdown-overlays)

(defvar markdown-overlays-bench-sample
  (concat
   "This has **bold** and *italic* and `code` in a sentence.\n"
   "Visit [Example](https://example.com) and ~~deleted~~ text.\n"
   "More **bold** words and ***bold-italic*** and __underscore bold__.\n"
   "Nested: **bold with *italic* inside** and ~~struck **bold** struck~~.\n"
   "Consecutive: **a** **b** **c** **d** **e** **f**.\n"
   "A [link](https://a.com) then **bold [link](https://b.com)** end.\n")
  "One block of markup-dense text (~368 chars).")

(defvar markdown-overlays-bench-table-sample
  (concat
   "| Name | Role | Status |\n"
   "|------|------|--------|\n"
   "| **Alice** | [Engineer](http://x.com) | `active` |\n"
   "| *Bob* | ~~Manager~~ Lead | active |\n"
   "| Carol | ***bold-italic*** | `pending` |\n")
  "A small markdown table with inline markup (~200 chars).")

(defun markdown-overlays-bench--time-call (func iterations)
  "Call FUNC for ITERATIONS, return milliseconds per call."
  (garbage-collect)
  (let ((start (float-time)))
    (dotimes (_ iterations)
      (funcall func))
    (let ((elapsed (- (float-time) start)))
      (/ (* 1000.0 elapsed) iterations))))

(defun markdown-overlays-bench-run ()
  "Run the benchmark suite and print results."
  (interactive)
  (let ((iters 10)
        (copies 100)
        (sep (make-string 60 ?-)))
    (message "\n%s" sep)
    (message "Markdown overlay benchmark")
    (message "  Sample: %d chars × %d copies = %d chars"
             (length markdown-overlays-bench-sample) copies
             (* (length markdown-overlays-bench-sample) copies))
    (message "  Iterations: %d" iters)
    (message "%s" sep)

    ;; --- Buffer overlay rendering ---

    (with-temp-buffer
      (dotimes (_ copies) (insert markdown-overlays-bench-sample))

      ;; Full markdown-overlays-put
      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays-remove)
                   (markdown-overlays-put))
                 iters)))
        (message "  markdown-overlays-put:       %7.1fms/call" ms))

      (message "%s" sep)
      (message "  Buffer extraction sub-stages:")

      ;; Individual extraction functions
      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-inline-codes nil))
                 iters)))
        (message "    inline-codes:              %7.1fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-links nil))
                 iters)))
        (message "    links:                     %7.1fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-headers nil))
                 iters)))
        (message "    headers:                   %7.1fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-bolds nil))
                 iters)))
        (message "    bolds:                     %7.1fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-italics nil))
                 iters)))
        (message "    italics:                   %7.1fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--markdown-strikethroughs nil))
                 iters)))
        (message "    strikethroughs:            %7.1fms/call" ms)))

    (message "%s" sep)

    ;; --- Table cell processing ---

    (message "  Table cell processing (--replace-markup sub-stages):")

    (let ((cell-text "**bold** and *italic* `code` [link](url) ~~struck~~"))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--replace-markup
                    cell-text markdown-overlays--cell-inline-code-regexp
                    '(1) 'font-lock-doc-markup-face))
                 (* iters copies))))
        (message "    code spans:                %7.3fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--replace-markup
                    cell-text markdown-overlays--cell-bold-italic-regexp
                    '(1) '(:weight bold :slant italic)))
                 (* iters copies))))
        (message "    bold-italic:               %7.3fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--replace-markup
                    cell-text markdown-overlays--cell-bold-regexp
                    '(1 2) 'bold))
                 (* iters copies))))
        (message "    bold:                      %7.3fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--replace-markup
                    cell-text markdown-overlays--cell-italic-regexp
                    '(2 3) 'italic t 1))
                 (* iters copies))))
        (message "    italic:                    %7.3fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--replace-markup
                    cell-text markdown-overlays--cell-strikethrough-regexp
                    '(1) '(:strike-through t) t))
                 (* iters copies))))
        (message "    strikethrough:             %7.3fms/call" ms))

      (let ((ms (markdown-overlays-bench--time-call
                 (lambda ()
                   (markdown-overlays--process-cell-content cell-text))
                 (* iters copies))))
        (message "    full process-cell-content: %7.3fms/call" ms)))

    (message "%s" sep)

    ;; --- Full table rendering ---

    (let ((table-copies 20))
      (message "  Table rendering (%d tables × %d iters):"
               table-copies iters)
      (with-temp-buffer
        (dotimes (_ table-copies)
          (insert markdown-overlays-bench-table-sample "\n"))
        (let ((markdown-overlays-prettify-tables t))
          (let ((ms (markdown-overlays-bench--time-call
                     (lambda ()
                       (markdown-overlays-remove)
                       (markdown-overlays-put))
                     iters)))
            (message "    full put (with tables):    %7.1fms/call" ms)))))

    (message "%s" sep)
    (message "Done.\n")))

(provide 'markdown-overlays-bench)

;;; markdown-overlays-bench.el ends here
