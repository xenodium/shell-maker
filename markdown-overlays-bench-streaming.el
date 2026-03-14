;;; bench-streaming.el --- Streaming table benchmark  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Simulates agent-shell's streaming pattern: rows arrive one at a time,
;; `markdown-overlays-put` is called after each chunk.
;;
;; Usage:
;;   emacs --batch -L <path-to-lib> -l bench-streaming -f bench-streaming-run
;;
;; Measures: per-chunk time, cumulative time, GC count/time, peak chunk.

;;; Code:

(require 'markdown-overlays)
(require 'markdown-overlays-tables)

(defconst bench-streaming--num-tables 10
  "Number of tables to generate.")

(defconst bench-streaming--num-rows 10
  "Number of data rows per table.")

(defconst bench-streaming--num-cols 10
  "Number of columns per table.")

(defun bench-streaming--make-header (ncols)
  "Generate a markdown table header row with NCOLS columns."
  (concat "| "
          (mapconcat (lambda (i) (format "Col_%d" i))
                     (number-sequence 1 ncols) " | ")
          " |\n"))

(defun bench-streaming--make-separator (ncols)
  "Generate a markdown table separator row with NCOLS columns."
  (concat "|"
          (mapconcat (lambda (_) "------") (number-sequence 1 ncols) "|")
          "|\n"))

(defun bench-streaming--make-data-row (ncols row-num)
  "Generate a markdown table data row with NCOLS columns at ROW-NUM."
  (concat "| "
          (mapconcat (lambda (c)
                       (pcase (mod c 5)
                         (0 (format "**bold_%d**" row-num))
                         (1 (format "*italic_%d*" c))
                         (2 (format "`code_%d`" row-num))
                         (3 (format "plain_%d_%d" row-num c))
                         (4 (format "~~struck_%d~~" c))))
                     (number-sequence 1 ncols) " | ")
          " |\n"))

(defun bench-streaming--generate-chunks ()
  "Return a list of (CHUNK . DESCRIPTION) pairs for the streaming simulation.
Each chunk is a string to append.  The full sequence builds up
`bench-streaming--num-tables' tables row by row."
  (let ((chunks nil))
    (dotimes (t-idx bench-streaming--num-tables)
      (push (cons (bench-streaming--make-header bench-streaming--num-cols)
                  (format "table %d header" (1+ t-idx)))
            chunks)
      (push (cons (bench-streaming--make-separator bench-streaming--num-cols)
                  (format "table %d separator" (1+ t-idx)))
            chunks)
      (dotimes (r bench-streaming--num-rows)
        (push (cons (bench-streaming--make-data-row bench-streaming--num-cols (1+ r))
                    (format "table %d row %d" (1+ t-idx) (1+ r)))
              chunks)
      )
      (push (cons "\n" (format "table %d trailing newline" (1+ t-idx)))
            chunks))
    (nreverse chunks)))

(defun bench-streaming-run ()
  "Run the streaming benchmark and report results."
  (interactive)
  (let* ((chunks (bench-streaming--generate-chunks))
         (nchunks (length chunks))
         (markdown-overlays-prettify-tables t)
         (chunk-times (make-vector nchunks 0.0))
         (gc-before (garbage-collect))
         (gc-count-before (if (fboundp 'gc-count) (gc-count) 0))
         (total-start (float-time))
         (idx 0))

    (with-temp-buffer
      ;; Simulate streaming: append chunk, call put on full buffer
      (dolist (chunk-pair chunks)
        (goto-char (point-max))
        (insert (car chunk-pair))
        (let ((t0 (float-time)))
          (markdown-overlays-remove)
          (markdown-overlays-put)
          (aset chunk-times idx (- (float-time) t0)))
        (setq idx (1+ idx)))

      (let* ((total-elapsed (- (float-time) total-start))
             (gc-count-after (if (fboundp 'gc-count) (gc-count) 0))
             (total-ms (* 1000.0 total-elapsed))
             (chunk-ms-list (append chunk-times nil))
             (avg-ms (/ (seq-reduce #'+ chunk-ms-list 0.0) nchunks))
             (max-ms (seq-max (seq-map (lambda (x) (* 1000.0 x)) chunk-ms-list)))
             (min-ms (seq-min (seq-map (lambda (x) (* 1000.0 x)) chunk-ms-list)))
             (p50 (bench-streaming--percentile chunk-ms-list 50))
             (p95 (bench-streaming--percentile chunk-ms-list 95))
             (p99 (bench-streaming--percentile chunk-ms-list 99))
             (final-overlays (length (overlays-in (point-min) (point-max))))
             (final-chars (buffer-size))
             (sep (make-string 65 ?-)))

        (message "\n%s" sep)
        (message "STREAMING TABLE BENCHMARK")
        (message "%s" sep)
        (message "  Tables: %d × %d rows × %d cols"
                 bench-streaming--num-tables
                 bench-streaming--num-rows
                 bench-streaming--num-cols)
        (message "  Chunks: %d  (each triggers full markdown-overlays-put)"
                 nchunks)
        (message "  Final buffer: %d chars, %d overlays"
                 final-chars final-overlays)
        (message "%s" sep)
        (message "  TOTAL time:     %8.1f ms" total-ms)
        (message "  Per-chunk avg:  %8.2f ms" (* 1000.0 avg-ms))
        (message "  Per-chunk min:  %8.2f ms" min-ms)
        (message "  Per-chunk max:  %8.2f ms" max-ms)
        (message "  P50:            %8.2f ms" p50)
        (message "  P95:            %8.2f ms" p95)
        (message "  P99:            %8.2f ms" p99)
        (message "%s" sep)
        (message "  GCs during bench: %d"
                 (- gc-count-after gc-count-before))
        (message "%s" sep)

        ;; Per-table breakdown: time for last row of each table
        (message "  Per-table completion chunk times:")
        (let ((rows-per-table (+ bench-streaming--num-rows 3)))
          (dotimes (t-idx bench-streaming--num-tables)
            (let* ((last-chunk-idx (1- (* (1+ t-idx) rows-per-table)))
                   (ms (* 1000.0 (aref chunk-times last-chunk-idx))))
              (message "    Table %2d (chunk %3d): %7.2f ms"
                       (1+ t-idx) last-chunk-idx ms))))

        (message "%s" sep)
        (message "Done.\n")))))

(defun bench-streaming--percentile (time-list pct)
  "Return the PCTth percentile from TIME-LIST (seconds), as milliseconds."
  (let* ((sorted (sort (copy-sequence time-list) #'<))
         (n (length sorted))
         (idx (min (1- n) (floor (* n (/ pct 100.0))))))
    (* 1000.0 (nth idx sorted))))

(provide 'bench-streaming)

;;; bench-streaming.el ends here
