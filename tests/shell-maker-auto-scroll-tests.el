;;; shell-maker-auto-scroll-tests.el --- Tests for auto-scroll decisions -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'shell-maker)

(ert-deftest shell-maker-auto-scroll-test-inhibit-skips-auto-scroll ()
  "Binding `shell-maker-inhibit-auto-scroll' disables auto-scroll at end of buffer."
  (with-temp-buffer
    (insert "Agent> hello")
    (should (shell-maker--should-auto-scroll-p))
    (let ((shell-maker-inhibit-auto-scroll t))
      (should-not (shell-maker--should-auto-scroll-p)))))

(ert-deftest shell-maker-auto-scroll-test-inhibit-skips-visibility-check ()
  "Binding `shell-maker-inhibit-auto-scroll' skips asking redisplay.
Each visibility check lays out the window, which bulk updates would
otherwise pay for on every insertion."
  (with-temp-buffer
    (insert "Agent> hello")
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _) (list 'window)))
              ((symbol-function 'pos-visible-in-window-p)
               (lambda (&rest _)
                 (error "Unexpected visibility check"))))
      (should-error (shell-maker--should-auto-scroll-p))
      (let ((shell-maker-inhibit-auto-scroll t))
        (should-not (shell-maker--should-auto-scroll-p))))))

(provide 'shell-maker-auto-scroll-tests)

;;; shell-maker-auto-scroll-tests.el ends here
