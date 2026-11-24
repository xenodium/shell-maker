;;; shell-maker-prompt-tests.el --- Tests for prompt marker insertion -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'ert)
(require 'shell-maker)

(defun shell-maker-prompt-test--call-with-shell (fn)
  "Call FN in a comint buffer holding a live `Agent> ' prompt.
FN receives the buffer's process.  The prompt is emitted through
`shell-maker--output-filter', so `comint-last-prompt' and the process
mark start out where a real shell leaves them."
  (let ((buffer (generate-new-buffer " *shell-maker-prompt-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local comint-prompt-regexp "^Agent> ")
          (let ((process (start-process "shell-maker-prompt-test" buffer "cat")))
            (set-process-query-on-exit-flag process nil)
            (unwind-protect
                (cl-letf (((symbol-function 'shell-maker--process) (lambda () process)))
                  (shell-maker--output-filter process "Agent> ")
                  (funcall fn process))
              (delete-process process))))
      (kill-buffer buffer))))

(ert-deftest shell-maker-prompt-test-end-of-prompt-marker-never-rewinds-mark ()
  "Marker synthesized above a live prompt leaves the mark at the prompt's end.
Regression: the marker moved the process mark to `point-max', which under
a narrowing that ends before the live prompt is the prompt's own start.
comint then read the `PROMPT> ' text as part of the next submitted
message, and whatever was typed at the prompt stopped counting as input."
  (shell-maker-prompt-test--call-with-shell
   (lambda (process)
     (let ((inhibit-read-only t))
       (goto-char (point-max))
       (insert "unsent input"))
     (save-restriction
       ;; A caller replaying history above the live prompt, which
       ;; flips the prompt-start marker's insertion type so the prompt
       ;; is pushed down rather than stranded inside the new text.
       (set-marker-insertion-type (car comint-last-prompt) t)
       (narrow-to-region (point-min) (car comint-last-prompt))
       (shell-maker-insert-end-of-prompt-marker))
     (should (= (marker-position (process-mark process))
                (marker-position (cdr comint-last-prompt))))
     (should (equal (buffer-substring-no-properties (process-mark process)
                                                    (point-max))
                    "unsent input")))))

(ert-deftest shell-maker-prompt-test-end-of-prompt-marker-advances-mark ()
  "Marker appended after submitted input advances the mark past it.
This is the submission path, where the marker closes the command the
mark was just moved over."
  (shell-maker-prompt-test--call-with-shell
   (lambda (process)
     (goto-char (point-max))
     (insert "hello")
     ;; `comint-send-input' leaves the mark past the submitted input.
     (set-marker (process-mark process) (point-max))
     (shell-maker-insert-end-of-prompt-marker)
     (should (= (marker-position (process-mark process)) (point-max)))
     (should (string-suffix-p "<shell-maker-end-of-prompt>"
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))))

(provide 'shell-maker-prompt-tests)

;;; shell-maker-prompt-tests.el ends here
