;;; markdown-overlays-blocks-tests.el --- Tests for code block fontification -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'markdown-overlays)

;;; Helpers

(defun markdown-overlays-blocks-tests--code-overlays ()
  "Return overlays with category `markdown-overlays' and a `face' property.
Sorted by start position."
  (sort (seq-filter (lambda (ov)
                      (and (eq (overlay-get ov 'category) 'markdown-overlays)
                           (overlay-get ov 'face)
                           (not (equal (overlay-get ov 'face) '(:box t)))))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b)
          (< (overlay-start a) (overlay-start b)))))

;;; Tests

(ert-deftest markdown-overlays-blocks-test-face-run-overlays ()
  "Overlays should span contiguous face runs, not individual characters."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun foo (x)\n  x)\n```\n")
    (markdown-overlays-put)
    (let ((ovs (markdown-overlays-blocks-tests--code-overlays)))
      ;; Should have overlays for the code.
      (should (> (length ovs) 0))
      ;; The "defun" keyword should be covered by a single overlay.
      (goto-char (point-min))
      (search-forward "defun")
      (let* ((defun-start (match-beginning 0))
             (defun-end (match-end 0))
             (defun-ov (seq-find (lambda (ov)
                                   (and (<= (overlay-start ov) defun-start)
                                        (>= (overlay-end ov) defun-end)))
                                 ovs)))
        (should defun-ov)))))

(ert-deftest markdown-overlays-blocks-test-correct-faces ()
  "Keyword faces from font-lock should be applied."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun foo (x)\n  x)\n```\n")
    (markdown-overlays-put)
    (let ((ovs (markdown-overlays-blocks-tests--code-overlays)))
      ;; "defun" should get font-lock-keyword-face.
      (should (seq-find (lambda (ov)
                          (eq (overlay-get ov 'face)
                              'font-lock-keyword-face))
                        ovs)))))

(ert-deftest markdown-overlays-blocks-test-unknown-language-fallback ()
  "Unknown languages should get a single fallback face overlay."
  (with-temp-buffer
    (insert "```unknownlang99\nhello world\n```\n")
    (markdown-overlays-put)
    (let ((ovs (markdown-overlays-blocks-tests--code-overlays)))
      (should (= (length ovs) 1))
      (should (eq (overlay-get (car ovs) 'face)
                  'font-lock-doc-markup-face)))))

(provide 'markdown-overlays-blocks-tests)

;;; markdown-overlays-blocks-tests.el ends here
