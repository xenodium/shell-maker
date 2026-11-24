;;; shell-maker-buffer-tests.el --- Tests for shell buffer resolution -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'map)
(require 'shell-maker)

(defvar shell-maker-buffer-test--inputs nil
  "Commands submitted to test shells, most recent first.")

(defun shell-maker-buffer-test--start (buffer-name)
  "Start a test shell named BUFFER-NAME and return its buffer.
Each submitted command is pushed onto `shell-maker-buffer-test--inputs'
and answered synchronously with \"reply\"."
  (shell-maker-start-v2
   :config (make-shell-maker-config
            :name "SmBufTest"
            :prompt "SmBufTest> "
            :prompt-regexp "^SmBufTest> "
            :execute-command
            (lambda (input shell)
              (push input shell-maker-buffer-test--inputs)
              (funcall (map-elt shell :write-output) "reply")
              (funcall (map-elt shell :finish-output) t)))
   :buffer-name buffer-name
   :no-focus t
   :alias-commands nil))

(defmacro shell-maker-buffer-test--with-root (&rest body)
  "Evaluate BODY with shell files under a temp dir, killing test shells after."
  (declare (indent 0))
  `(let ((shell-maker-root-path (make-temp-file "shell-maker-buffer-test" t))
         (shell-maker-prompt-before-killing-buffer nil)
         (shell-maker-buffer-test--inputs nil)
         (buffers-before (buffer-list)))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer buffers-before)
           (kill-buffer buffer)))
       (delete-directory shell-maker-root-path t))))

(ert-deftest shell-maker-buffer-test-submit-after-rename-buffer ()
  "Submitting in a shell renamed via `rename-buffer' stays in that shell.
Regression: the shell was looked up by its creation-time name, so a
plain `rename-buffer' made the lookup create an empty process-less
buffer under the old name and submission failed with
\(wrong-type-argument processp nil)."
  (shell-maker-buffer-test--with-root
    (let ((shell (shell-maker-buffer-test--start "*smbuftest-a*")))
      (with-current-buffer shell
        (rename-buffer "renamed")
        (shell-maker-submit :input "hello"))
      (should (equal shell-maker-buffer-test--inputs '("hello")))
      (should-not (get-buffer "*smbuftest-a*"))
      (should (string-match-p "reply"
                              (with-current-buffer shell
                                (buffer-string)))))))

(ert-deftest shell-maker-buffer-test-renamed-shell-ignores-name-reuse ()
  "A renamed shell keeps writing to itself after its old name is reused.
Regression: the stale creation-time name resolved to the newer shell,
so the renamed shell's replies landed in the wrong buffer."
  (shell-maker-buffer-test--with-root
    (let* ((first (shell-maker-buffer-test--start "*smbuftest-a*"))
           (second (progn
                     (with-current-buffer first
                       (rename-buffer "renamed"))
                     (shell-maker-buffer-test--start "*smbuftest-a*"))))
      (with-current-buffer first
        (shell-maker-submit :input "hello"))
      (should (equal shell-maker-buffer-test--inputs '("hello")))
      (should (string-match-p "reply"
                              (with-current-buffer first
                                (buffer-string))))
      (should-not (string-match-p "reply"
                                  (with-current-buffer second
                                    (buffer-string)))))))

(ert-deftest shell-maker-buffer-test-buffer-name-follows-rename ()
  "The shell's name is its current name inside it, the default elsewhere."
  (shell-maker-buffer-test--with-root
    (let ((shell (shell-maker-buffer-test--start "*smbuftest-a*")))
      (with-current-buffer shell
        (rename-buffer "renamed")
        (should (equal (shell-maker-buffer-name shell-maker--config)
                       "renamed"))
        (shell-maker-set-buffer-name shell "renamed again")
        (should (equal (shell-maker-buffer-name shell-maker--config)
                       "renamed again")))
      (with-temp-buffer
        (should (equal (shell-maker-buffer-name
                        (buffer-local-value 'shell-maker--config shell))
                       "*smbuftest*"))))))

(provide 'shell-maker-buffer-tests)

;;; shell-maker-buffer-tests.el ends here
