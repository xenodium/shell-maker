;;; shell-maker-history-tests.el --- Tests for history extraction -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'shell-maker)

;;; shell-maker--parse-history-chunk

(ert-deftest shell-maker-history-test-parse-chunk-basic ()
  "Parse a chunk with command and response."
  (let ((result (shell-maker--parse-history-chunk
                 "hello<shell-maker-end-of-prompt>Hi there")))
    (should (equal result '("hello" . "Hi there")))))

(ert-deftest shell-maker-history-test-parse-chunk-empty ()
  "Empty chunk returns nil."
  (should-not (shell-maker--parse-history-chunk "")))

(ert-deftest shell-maker-history-test-parse-chunk-command-only ()
  "Chunk with command but no response."
  (let ((result (shell-maker--parse-history-chunk
                 "hello<shell-maker-end-of-prompt>")))
    (should (equal (car result) "hello"))
    (should-not (cdr result))))

(ert-deftest shell-maker-history-test-parse-chunk-trims-whitespace ()
  "Whitespace around command and response is trimmed."
  (let ((result (shell-maker--parse-history-chunk
                 "  hello  <shell-maker-end-of-prompt>  world  ")))
    (should (equal result '("hello" . "world")))))

(ert-deftest shell-maker-history-test-parse-chunk-failed-command ()
  "Failed commands are excluded."
  (should-not (shell-maker--parse-history-chunk
               "hello<shell-maker-end-of-prompt><shell-maker-failed-command>")))

(ert-deftest shell-maker-history-test-parse-chunk-interrupted-command ()
  "Interrupted commands are kept."
  (let ((result (shell-maker--parse-history-chunk
                 "hello<shell-maker-end-of-prompt><shell-maker-interrupted-command>")))
    (should (equal (car result) "hello"))))

(ert-deftest shell-maker-history-test-parse-chunk-strips-markers ()
  "Internal markers are stripped from response."
  (let ((result (shell-maker--parse-history-chunk
                 "hello<shell-maker-end-of-prompt>world<shell-maker-something>")))
    (should (equal result '("hello" . "world")))))

;;; shell-maker--extract-history

(ert-deftest shell-maker-history-test-extract-basic ()
  "Extract history from a buffer with two exchanges."
  (with-temp-buffer
    (insert "Agent> hello\n<shell-maker-end-of-prompt>Hi there\n"
            "Agent> bye\n<shell-maker-end-of-prompt>Goodbye\n")
    (let ((result (shell-maker--extract-history "^Agent> ")))
      (should (= (length result) 2))
      (should (equal (car result) '("hello" . "Hi there")))
      (should (equal (nth 1 result) '("bye" . "Goodbye"))))))

(ert-deftest shell-maker-history-test-extract-empty-buffer ()
  "Empty buffer returns nil."
  (with-temp-buffer
    (should-not (shell-maker--extract-history "^Agent> "))))

(ert-deftest shell-maker-history-test-extract-single-exchange ()
  "Extract history from a buffer with one exchange."
  (with-temp-buffer
    (insert "Agent> hello\n<shell-maker-end-of-prompt>Hi there\n")
    (let ((result (shell-maker--extract-history "^Agent> ")))
      (should (= (length result) 1))
      (should (equal (car result) '("hello" . "Hi there"))))))

(ert-deftest shell-maker-history-test-extract-multiline-response ()
  "Extract history with a multiline response."
  (with-temp-buffer
    (insert "Agent> hello\n<shell-maker-end-of-prompt>line one\nline two\n")
    (let ((result (shell-maker--extract-history "^Agent> ")))
      (should (= (length result) 1))
      (should (equal (cdr (car result)) "line one\nline two")))))

(ert-deftest shell-maker-history-test-extract-skips-failed ()
  "Failed commands are excluded from history."
  (with-temp-buffer
    (insert "Agent> good\n<shell-maker-end-of-prompt>response\n"
            "Agent> bad\n<shell-maker-end-of-prompt><shell-maker-failed-command>\n"
            "Agent> also good\n<shell-maker-end-of-prompt>another response\n")
    (let ((result (shell-maker--extract-history "^Agent> ")))
      (should (= (length result) 2))
      (should (equal (car (car result)) "good"))
      (should (equal (car (nth 1 result)) "also good")))))

(ert-deftest shell-maker-history-test-extract-preserves-point ()
  "Extracting history should not move point."
  (with-temp-buffer
    (insert "Agent> hello\n<shell-maker-end-of-prompt>Hi\n")
    (goto-char (point-min))
    (let ((pos (point)))
      (shell-maker--extract-history "^Agent> ")
      (should (= (point) pos)))))

(provide 'shell-maker-history-tests)

;;; shell-maker-history-tests.el ends here
