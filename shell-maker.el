;;; shell-maker.el --- Interaction mode for making comint shells  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/shell-maker
;; Version: 0.85.1
;; Package-Requires: ((emacs "27.1"))

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

;; This is a comint-based generic package used for building concrete
;; shells.
;;
;; Much inspiration comes from IELM
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(defconst shell-maker-version "0.85.1")

(require 'comint)
(require 'goto-addr)
(require 'json)
(require 'map)
(require 'seq)
(require 'shell)
(require 'view)

(eval-when-compile
  (require 'cl-lib)
  (declare-function json-pretty-print "ext:json" (begin end &optional minimize)))

(defcustom shell-maker-display-function #'pop-to-buffer-same-window
  "Function to display the shell.  Set to `display-buffer' or custom function."
  :type '(choice (function-item :tag "Pop to Buffer" pop-to-buffer-same-window)
                 (function-item :tag "Display Buffer" display-buffer)
                 function)
  :group 'shell-maker)

(defcustom shell-maker-read-string-function (lambda (prompt history)
                                              (read-string prompt nil history))
  "Function to read strings from user.

To use `completing-read', it can be done with something like:

\(setq `shell-maker-read-string-function'
      (lambda (prompt history)
        (completing-read prompt (symbol-value history) nil nil nil history)))"
  :type 'function
  :group 'shell-maker)

(defcustom shell-maker-logging nil
  "Logging disabled by default (slows things down).

Enable it for troubleshooting issues."
  :type 'boolean
  :group 'shell-maker)

(defcustom shell-maker-prompt-before-killing-buffer t
  "If non-nil, confirm killing buffer without saving."
  :type 'boolean
  :group 'shell-maker)

(defcustom shell-maker-transcript-default-path nil
  "Default path to save transcripts to."
  :type 'directory
  :group 'shell-maker)

(defcustom shell-maker-transcript-default-filename
  (lambda ()
    "transcript.txt")
  "Default file name to save transcripts to.

As a function, so it can also logic to generate a name.

For example:

\(lambda ()
    (format-time-string \"%F-%T-transcript.txt\"))"
  :type 'function
  :group 'shell-maker)

(defcustom shell-maker-root-path user-emacs-directory
  "Root path location to store internal shell files."
  :type 'directory
  :group 'shell-maker)

(defcustom shell-maker-curl-executable "curl"
  "Path to the curl executable.
Can be a simple command name like \"curl\" if it's in PATH,
or an absolute path like \"/usr/local/bin/curl\"."
  :type 'string
  :group 'shell-maker)

(defcustom shell-maker-forget-file-after-clear nil
  "If non-nil, reset file path after clear command."
  :type 'boolean
  :group 'shell-maker)

(defvar-local shell-maker--input nil)

(defvar-local shell-maker--current-request-id 0)

(defvar shell-maker--show-invisible-markers nil)

(defconst shell-maker--prompt-rear-nonsticky
  '(field inhibit-line-move-field-capture read-only font-lock-face)
  "Text properties set on the prompt and don't want to leak past it.")

(cl-defstruct
    shell-maker-config
  name
  prompt
  prompt-regexp
  validate-command
  execute-command
  on-command-finished
  redact-log-output)

(defvar-local shell-maker--busy nil)

(defvar-local shell-maker--config nil)

(defvar-local shell-maker--file nil)

(defvar-local shell-maker--request-process nil)

(defvar-local shell-maker--buffer-name-override nil)

(defmacro shell-maker--with-buffer-if (wrap buffer &rest body)
  "If WRAP, wrap BODY `with-current-buffer' BUFFER."
  `(if ,wrap
       (with-current-buffer ,buffer ,@body)
     ,@body))

(defmacro shell-maker--with-temp-buffer-if (wrap &rest body)
  "If WRAP, wrap BODY `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(if ,wrap
       (with-temp-buffer ,@body)
     ,@body))

(defvar-keymap shell-maker-mode-map
  :parent comint-mode-map
  :doc "Base keymap for `shell-maker' based modes."
  "S-<return>" #'newline
  "<remap> <rename-buffer>" #'shell-maker-rename-buffer
  "<remap> <save-buffer>" #'shell-maker-save-session-transcript
  "C-M-h" #'shell-maker-mark-output)

(defun shell-maker-start (config &optional no-focus welcome-function new-session buffer-name mode-line-name)
  "Start a shell with CONFIG.

Specify NO-FOCUS if started shell should not be focused.

Set WELCOME-FUNCTION to create and show a welcome message.

Set NEW-SESSION to start a new session.

Set BUFFER-NAME to override the buffer name.

Set MODE-LINE-NAME to override the mode line name."
  (shell-maker--with-temp-buffer-if new-session ;; Avoid picking up buffer-local vars from current buffer
    (let* ((old-point)
           (namespace (downcase (shell-maker-config-name config)))
           (welcome-message)
           (default-directory (cond ((file-directory-p default-directory)
                                     default-directory)
                                    ((file-directory-p (expand-file-name "~/"))
                                     (expand-file-name "~/"))
                                    (t
                                     (error "Could not set default directory")))))
      (unless buffer-name
        (setq buffer-name (shell-maker-buffer-default-name
                           (shell-maker-config-name config))))
      (when new-session
        (setq buffer-name (generate-new-buffer-name buffer-name)))
      ;; Alias with concrete shell symbols.
      (fset (intern (concat namespace "-shell-clear-buffer")) #'shell-maker-clear-buffer)
      (fset (intern (concat namespace "-shell-previous-input")) #'comint-previous-input)
      (fset (intern (concat namespace "-shell-next-input")) #'comint-next-input)
      (fset (intern (concat namespace "-shell-submit")) #'shell-maker-submit)
      (fset (intern (concat namespace "-shell-save-session-transcript"))
            #'shell-maker-save-session-transcript)
      (fset (intern (concat namespace "-shell-search-history")) #'shell-maker-search-history)
      (fset (intern (concat namespace "-shell-newline")) #'newline)
      (fset (intern (concat namespace "-shell-rename-buffer")) #'shell-maker-rename-buffer)
      (fset (intern (concat namespace "-shell-delete-interaction-at-point")) #'shell-maker-delete-interaction-at-point)
      (fset (intern (concat namespace "-shell-restore-session-from-transcript")) #'shell-maker-restore-session-from-transcript)
      (eval
       (macroexpand
        `(define-derived-mode ,(shell-maker-major-mode config) comint-mode
           ,(or mode-line-name (shell-maker-config-name config))
           ,(format "Major mode for %s shell." (shell-maker-config-name config))
           (keymap-set shell-maker-mode-map "<remap> <comint-delchar-or-maybe-eof>" #'delete-char)
           (keymap-set shell-maker-mode-map "<remap> <comint-send-input>" #'shell-maker-submit)
           (keymap-set shell-maker-mode-map "<remap> <comint-interrupt-subjob>" #'shell-maker-interrupt)
           (keymap-set shell-maker-mode-map "<remap> <comint-history-isearch-backward-regexp>" #'shell-maker-search-history))))

      (unless (comint-check-proc buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (funcall (shell-maker-major-mode config))
          (setq-local shell-maker--busy nil)
          (unless (equal (shell-maker-buffer-name config)
                         buffer-name)
            (setq-local shell-maker--buffer-name-override buffer-name))
          (unless (zerop (buffer-size))
            (setq old-point (point)))
          (when welcome-function
            (setq welcome-message
                  (funcall welcome-function config)))
          (when welcome-message
            (insert welcome-message)
            (insert (propertize "\n<shell-maker-failed-command>\n"
                                'invisible (not shell-maker--show-invisible-markers))))
          (shell-maker--initialize config)))
      (unless no-focus
        (funcall shell-maker-display-function buffer-name))
      (when old-point
        (push-mark old-point))
      (get-buffer buffer-name))))

(defun shell-maker-define-major-mode (config &optional mode-map)
  "Define the major mode for the shell using CONFIG.

Optionally use MODE-MAP."
  (if mode-map
      (eval `(define-derived-mode ,(shell-maker-major-mode config) comint-mode
               ,(shell-maker-config-name config)
               ,(format "Major mode for %s shell." (shell-maker-config-name config))
               (use-local-map ,mode-map)))
    (let ((mode-map-symbol (intern (format "%s-shell-mode-map"
                                           (downcase (shell-maker-config-name config))))))
      (when (boundp mode-map-symbol)
        (makunbound mode-map-symbol))
      (eval `(defvar-keymap ,mode-map-symbol
               :parent shell-maker-mode-map))
      (eval `(define-derived-mode ,(shell-maker-major-mode config) comint-mode
               ,(shell-maker-config-name config)
               ,(format "Major mode for %s shell." (shell-maker-config-name config))
               (use-local-map ,mode-map-symbol))))))

(defun shell-maker-welcome-message (config)
  "Return a welcome message to be printed using CONFIG."
  (format
   "\n\n\n     Welcome to %s shell\n\n\n       Type %s and press %s for details.\n\n       Like this package? Consider ✨%s✨\n\n\n\n\n"
   (propertize (shell-maker-config-name config)
               'font-lock-face 'font-lock-comment-face)
   (propertize "help" 'font-lock-face 'italic)
   (shell-maker--propertize-key-binding "-shell-submit" config)
   (shell-maker-make-button-text "sponsoring"
                                 (lambda ()
                                   (browse-url "https://github.com/sponsors/xenodium")
                                   (message "Thank you!")))))

(defun shell-maker-local-config ()
  "Return the shell buffer local config."
  shell-maker--config)

(defun shell-maker--initialize (config)
  "Initialize shell using CONFIG."
  (unless (eq major-mode (shell-maker-major-mode config))
    (user-error "Not in a shell"))
  (setq-local shell-maker--config (copy-sequence config))
  (visual-line-mode +1)
  (goto-address-mode +1)
  ;; Prevents fontifying streamed response as prompt.
  (setq comint-prompt-regexp
        (shell-maker-prompt-regexp config))
  (add-to-list 'kill-buffer-query-functions #'shell-maker-kill-buffer-query)
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender (lambda (_proc input)
                              (setq shell-maker--input input)))
  (setq comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'shell-maker--get-old-input)
  (setq-local comint-completion-addsuffix nil)
  (setq-local imenu-generic-expression
              `((nil ,(concat (shell-maker-prompt-regexp config) "\\(.*\\)") 1)))
  (shell-maker--read-input-ring-history config)
  (unless (or (comint-check-proc (shell-maker-buffer config))
              (get-buffer-process (shell-maker-buffer config)))
    (condition-case nil
        (start-process (shell-maker-process-name config)
                       (shell-maker-buffer config) "hexl")
      (file-error (start-process
                   (shell-maker-process-name config)
                   (shell-maker-buffer config) "cat")))
    (set-process-query-on-exit-flag (shell-maker--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (shell-maker--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (shell-maker--output-filter (shell-maker--process)
                                (shell-maker-prompt config))
    (set-marker comint-last-input-start (shell-maker--pm))
    (set-process-filter (get-buffer-process
                         (shell-maker-buffer config))
                        'shell-maker--output-filter)
    (set-buffer-modified-p nil)))

(cl-defun shell-maker--write-reply (&key config reply failed on-output)
  "Write REPLY to CONFIG prompt.  Set FAILED to record failure.

Use ON-OUTPUT function to monitor output text."
  (unless config
    (error "Missing config"))
  (unless reply
    (error "Missing reply"))
  (let ((inhibit-read-only t)
        (shell-buffer (shell-maker-buffer config))
        (output (concat reply
                        (if failed
                            (propertize "\n<shell-maker-failed-command>\n"
                                        'invisible (not shell-maker--show-invisible-markers))
                          "")
                        (shell-maker-prompt shell-maker--config))))
    (with-current-buffer shell-buffer
      (if (eobp) ;; auto-scroll
          (progn
            (goto-char (point-max))
            (shell-maker--output-filter (shell-maker--process) output))
        (save-excursion
          (goto-char (point-max))
          (shell-maker--output-filter (shell-maker--process) output)))))
  (when on-output
    (funcall on-output reply)))

(cl-defun shell-maker-submit (&key input on-output on-finished)
  "Submit current input.

Optionally, insert INPUT into shell.

If invoked programmatically, get notified:

Use ON-OUTPUT: function to monitor command response text.

Of the form:

 (lambda (response)
  (message \"Command: %s\" response))

Use ON-FINISHED: function to monitor when command is finished.

Of the form:

 (lambda (input output success)
  (message \"Finished: %s\" success))."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (unless (shell-maker-point-at-last-prompt-p)
    (goto-char (point-max)))
  (let* ((shell-buffer (shell-maker-buffer shell-maker--config))
         (called-interactively (called-interactively-p #'interactive))
         (shell-maker--input))
    (with-current-buffer shell-buffer
      (when input
        (goto-char (point-max))
        (insert input)))
    (comint-send-input) ;; Sets shell-maker--input
    (when (shell-maker--clear-input-for-execution :input shell-maker--input
                                                  :on-output on-output)
      (if called-interactively
          (shell-maker--eval-input-on-buffer-v2 :input shell-maker--input
                                                :config shell-maker--config)
        (shell-maker--eval-input-on-buffer-v2 :input shell-maker--input
                                              :config shell-maker--config
                                              :on-output on-output
                                              :on-finished on-finished)))))

(defun shell-maker-point-at-last-prompt-p ()
  "Return non-nil if point is at last prompt."
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (save-excursion
    (let ((point (point)))
      (goto-char (point-max))
      (when (re-search-backward comint-prompt-regexp nil t)
        (>= point (match-end 0))))))

(defun shell-maker-clear-buffer ()
  "Clear the current shell buffer."
  (interactive)
  (when shell-maker-forget-file-after-clear
    (setq shell-maker--file nil))
  (when (shell-maker--process)
    (comint-clear-buffer)))

(defun shell-maker-search-history ()
  "Search previous input history."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let ((candidate (completing-read
                    "History: "
                    (delete-dups
                     (seq-filter
                      (lambda (item)
                        (not (string-empty-p item)))
                      (ring-elements comint-input-ring))) nil t)))
    (delete-region (comint-line-beginning-position) (point-max))
    (insert candidate)))

(defun shell-maker-last-output ()
  "Get the last command output from the shell."
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (let* ((pmark (progn (goto-char (process-mark proc))
                           (forward-line 0)
                           (point-marker)))
             (output (buffer-substring-no-properties comint-last-input-end pmark))
             (items (split-string output "<shell-maker-end-of-prompt>")))
        (if (> (length items) 1)
            (nth 1 items)
          (nth 0 items))))))

;; Thanks to https://www.n16f.net/blog/making-ielm-more-comfortable
(defun shell-maker--read-input-ring-history (config)
  "Read input ring history from file using CONFIG."
  (let ((path (shell-maker-history-file-path config))
        (ring))
    (make-directory
     (file-name-directory path) t)
    (setq-local comint-input-ring-file-name nil)
    (setq-local comint-input-ignoredups t)
    (setq ring (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents path)
                   (read (current-buffer)))))
    (unless (ring-p ring)
      (setq ring (make-ring (min 1500 comint-input-ring-size))))
    (setq comint-input-ring ring)))

(defun shell-maker--write-input-ring-history (config)
  "Write input ring history to file using CONFIG."
  (let ((path (shell-maker-history-file-path config))
        (ring comint-input-ring)
        (print-length nil)
        (print-level nil))
    (make-directory
     (file-name-directory path) t)
    (with-temp-file path
      (insert (prin1-to-string (or ring
                                   (make-ring (min 1500 comint-input-ring-size))))))))

(defun shell-maker-busy ()
  "Non-nil if shell is currently busy.

Error if invoked from non-shell buffer."
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (error "Not in a shell"))
  shell-maker--busy)

(defun shell-maker--output-at-point ()
  "Output at point range with cons of start and end."
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let ((current-pos (point))
        (revert-pos)
        (start)
        (end)
        (prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point))))
    (when (>= (point) prompt-pos)
      (goto-char prompt-pos)
      (forward-line 0))
    (save-excursion
      (unless
          (cond
           ((re-search-backward "<shell-maker-end-of-prompt>" nil t)
            (forward-char (length "<shell-maker-end-of-prompt>"))
            t)
           ((re-search-backward
             (shell-maker-prompt-regexp shell-maker--config) nil t)
            (if (re-search-forward "<shell-maker-end-of-prompt>" nil t)
                t
              (end-of-line))
            t)
           (t
            nil))
        (setq revert-pos t))
      (setq start (point)))
    (save-excursion
      (unless (re-search-forward
               (shell-maker-prompt-regexp shell-maker--config)  nil t)
        (goto-char current-pos)
        (setq revert-pos t))
      (beginning-of-line)
      (setq end (point)))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (cons start end)))

(defun shell-maker-narrow-to-prompt ()
  "Narrow buffer to the command line (and any following command output) at point."
  (interactive)
  (let ((begin (shell-maker--prompt-begin-position)))
    (narrow-to-region
     begin
     (save-excursion
       (goto-char (shell-maker--prompt-end-position))
       (re-search-forward (shell-maker-prompt-regexp shell-maker--config) nil t)
       (if (= begin (shell-maker--prompt-begin-position))
           (point-max)
         (shell-maker--prompt-begin-position))))))


(defun shell-maker-delete-interaction-at-point ()
  "Delete interaction (request and response) at point."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (save-excursion
    (save-restriction
      (let ((inhibit-read-only t)
            (prompt-pos (save-excursion
                          (goto-char (process-mark
                                      (get-buffer-process (current-buffer))))
                          (point))))
        ;; Go to previous response if at last/empty prompt.
        (when (>= (point) prompt-pos)
          (goto-char prompt-pos)
          (forward-line -1)
          (end-of-line))
        ;; Removing `insert-in-front-hooks' from text, prior
        ;; to deleting region, ensures comint runs neither
        ;; `comint--mark-as-output' nor `comint--mark-yanked-as-output'
        ;; if user undoes the deletion, which breaks `comint' navigation.
        (remove-text-properties (point-min)
                                (point-max)
                                '(insert-in-front-hooks nil))
        (shell-maker-narrow-to-prompt)
        (delete-region (point-min) (point-max)))))
  (end-of-line))

(defun shell-maker--prompt-end-position ()
  "Based on `shell--prompt-end-position'."
  (save-excursion
    (goto-char (shell-maker--prompt-begin-position))
    (unless (comint-next-prompt 1)
      (error "No end found"))
    (point)))

(defun shell-maker-mark-output ()
  "If at latest prompt, mark last output.
Otherwise mark current output at location."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let ((current-pos (point))
        (revert-pos)
        (start)
        (end)
        (prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point))))
    (when (>= (point) prompt-pos)
      (goto-char prompt-pos)
      (forward-line -1)
      (end-of-line))
    (save-excursion
      (save-restriction
        (shell-maker-narrow-to-prompt)
        (unless
            (cond
             ((re-search-backward "<shell-maker-end-of-prompt>" nil t)
              (forward-char (length "<shell-maker-end-of-prompt>"))
              t)
             ((re-search-backward
               (shell-maker-prompt-regexp shell-maker--config) nil t)
              (if (re-search-forward "<shell-maker-end-of-prompt>" nil t)
                  t
                (end-of-line))
              t)
             (t
              nil))
          (setq revert-pos t))
        (setq start (point))))
    (save-excursion
      (save-restriction
        (shell-maker-narrow-to-prompt)
        (setq end (point-max))))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (set-mark (1- end))
    (goto-char (1+ start))))

(defun shell-maker--prompt-begin-position ()
  "Based on `shell--prompt-begin-position'."
  (save-excursion
    (let ((old-point (point)))
      (max
       (save-excursion
         (call-interactively #'comint-previous-prompt)
         (re-search-backward comint-prompt-regexp nil t)
         (point))
       (save-excursion
         (re-search-backward comint-prompt-regexp nil t)
         (point))
       (save-excursion
         (call-interactively #'comint-next-prompt)
         (re-search-backward comint-prompt-regexp nil t)
         (if (<= (point) old-point)
             (point)
           (point-min)))))))

(defun shell-maker-save-output ()
  "If at latest prompt, save last output.
Otherwise save current output at location."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let ((orig-point (point))
        (orig-region-active (region-active-p))
        (orig-region-start (region-beginning))
        (orig-region-end (region-end)))
    (unwind-protect
        (progn
          (shell-maker-mark-output)
          (write-region (region-beginning)
                        (region-end)
                        (read-file-name "Write file: ")))
      (if orig-region-active
          (progn
            (set-mark orig-region-start)
            (goto-char orig-region-end))
        (setq mark-active nil)
        (goto-char orig-point)))))

(defun shell-maker-interrupt (treat-as-failure)
  "Interrupt current request.

With prefix TREAT-AS-FAILURE, mark as failed."
  (interactive "P")
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (with-current-buffer (shell-maker-buffer shell-maker--config)
    ;; Increment id, so in-flight request is ignored.
    (shell-maker--increment-request-id)
    (goto-char (point-max))
    (unless treat-as-failure
      (shell-maker--output-filter (shell-maker--process)
                                  (propertize "\n<shell-maker-interrupted-command>\n"
                                              'invisible (not shell-maker--show-invisible-markers))))
    (when (process-live-p shell-maker--request-process)
      (kill-process shell-maker--request-process))
    (when shell-maker--busy
      (message "%s: interrupted!"
               (shell-maker-config-name shell-maker--config)))
    (comint-send-input) ;; Sets shell-maker--input
    (shell-maker--output-filter
     (shell-maker--process)
     (concat "\n" (shell-maker-prompt shell-maker--config)))
    (setq shell-maker--busy nil)))

(cl-defun shell-maker--clear-input-for-execution (&key input on-output)
  "Clear INPUT prior to shell execution.

Invokes optional ON-OUTPUT with error output.

Return t if INPUT us cleared.  nil otherwise."
  (unless input
    (error "Missing input"))
  (unless shell-maker--busy
    (setq shell-maker--busy t)
    (cond
     ((string-equal "help" (string-trim input))
      ;; TODO: output help to on-output also.
      (shell-maker--print-help)
      (setq shell-maker--busy nil)
      nil)
     ((string-equal "clear" (string-trim input))
      (call-interactively #'shell-maker-clear-buffer)
      (shell-maker--output-filter (shell-maker--process)
                                  (shell-maker-prompt shell-maker--config))
      (setq shell-maker--busy nil)
      (set-buffer-modified-p nil)
      nil)
     ((string-equal "config" (string-trim input))
      (shell-maker--write-reply :config shell-maker--config
                                :reply (shell-maker--dump-config shell-maker--config)
                                :on-output on-output)
      (setq shell-maker--busy nil)
      nil)
     ((not (shell-maker--curl-version-supported))
      (shell-maker--write-reply :config shell-maker--config
                                :reply "\nYou need curl version 7.76 or newer.\n\n"
                                :failed t
                                :on-output on-output)
      (setq shell-maker--busy nil)
      nil)
     ((and (shell-maker-config-validate-command
            shell-maker--config)
           (funcall (shell-maker-config-validate-command
                     shell-maker--config) input))
      (let ((error (concat "\n"
                           (funcall (shell-maker-config-validate-command
                                     shell-maker--config) input)
                           "\n\n")))
        (shell-maker--write-reply :config shell-maker--config
                                  :reply error
                                  :on-output on-output
                                  :failed t)
        (shell-maker--notify-on-command-finished
         :config shell-maker--config
         :input input
         :output error
         :success nil))
      (setq shell-maker--busy nil)
      nil)
     ((string-empty-p (string-trim input))
      (shell-maker--output-filter (shell-maker--process)
                                  (concat "\n" (shell-maker-prompt shell-maker--config)))
      (setq shell-maker--busy nil)
      nil)
     (t
      t))))

(defun shell-maker--announce-response (buffer)
  "Announce response if BUFFER is not active."
  (unless (eq buffer (window-buffer (selected-window)))
    (message "%s responded" (buffer-name buffer))))

(cl-defun shell-maker--execute-command-sync (&key command filter)
  "Execute COMMAND list (command + params).

FILTER: An optional function filter command output.  Use it for convertions.

  (lambda (raw-text)
    ;; Must return either a string
    ;; or
    ;; an alist of the form:
    \='((:filtered . \"filtered response string\")
        (:pending . \"pending string\")))

Return filtered response."
  (unless command
    (error "Missing mandatory :command param"))
  (unless filter
    (setq filter #'identity))
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (stderr-file (make-temp-file "stderr-"))
           (exit-status (apply #'call-process (seq-first command) nil (list buffer stderr-file) nil (cdr command)))
           (data (buffer-substring-no-properties (point-min) (point-max)))
           (filtered (funcall filter (list (cons :pending data))))
           (text (or (map-elt filtered :filtered)
                     (map-elt filtered :pending)
                     (when (stringp filtered)
                       filtered)
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (string-trim (buffer-string)))
                     "")))
      (list
       (cons :exit-status exit-status)
       (cons :output text)))))

(cl-defun shell-maker--execute-command-async (&key command filter on-output on-incoming-requests on-finished log)
  "Execute COMMAND list (command + params) asynchronously.

FILTER: An optional function filter command output.  Use it for convertions.

  (lambda (raw-text)
    ;; Must return either a string
    ;; or
    ;; an alist of the form:
    \='((:filtered . \"filtered response string\")
        (:pending . \"pending string\"))

ON-OUTPUT: A function to notify of output.

  (lambda (response))

ON-FINISHED: A function to notify when command is finished.

  (lambda (success)).

ON-INCOMING-REQUESTS: To handle incoming requests.

  (lambda (incoming-requests))

LOG: A function to log to.

  (lambda (format &rest))."
  (unless command
    (error "Missing mandatory :command param"))
  (unless filter
    (setq filter #'identity))
  (unless log
    (error "Missing mandatory :log param"))
  (let* ((process-name (make-temp-name "shell-maker--execute-command-async-"))
         (logs)
         (state))
    (cl-flet ((flush-logs ()
                (apply log (list logs)))
              (log (format &rest args)
                (when format
                  (setq logs (concat logs (apply #'format (append (list format) args)) "\n")))))
      (log "Async Command v2")
      (log "%s" command)
      (setq shell-maker--request-process
            (make-process
             :name process-name
             :buffer nil
             :command command
             :filter (lambda (_process raw-output)
                       ;; Start - Comment out to get stack traces.
                       (condition-case err
                       ;; End - Comment out to get stack traces.
                           (progn
                             (log "Filter pending")
                             (log "Filter output")
                             (log "%s" raw-output)
                             (log "Filter combined")
                             (log "%s" raw-output)
                             (setf (map-elt state :pending)
                                   (concat (map-elt state :pending)
                                           raw-output))
                             (let ((filtered (funcall filter state)))
                               (map-elt filtered :filtered)
                               (cond ((null filtered)
                                      (log "Ignored nil filtered"))
                                     ((map-elt filtered :incoming-requests)
                                      (when on-incoming-requests
                                        (funcall on-incoming-requests
                                                 (map-elt filtered :incoming-requests)))
                                      ;; Override state with latest filtered values.
                                      (mapc (lambda (item)
                                              (setf (map-elt state (car item))
                                                    (cdr item)))
                                            filtered))
                                     ((and (consp filtered) ;; partial extraction
                                           (or (seq-contains-p (map-keys filtered) :filtered)
                                               (seq-contains-p (map-keys filtered) :pending)))
                                      ;; Override state with latest filtered values.
                                      (mapc (lambda (item)
                                              (setf (map-elt state (car item))
                                                    (cdr item)))
                                            filtered)
                                      (when (and on-output (map-elt state :filtered))
                                        (funcall on-output (map-elt state :filtered)))
                                      (setf (map-elt state :output)
                                            (concat (map-elt state :output)
                                                    (map-elt state :filtered)))
                                      (setf (map-elt state :filtered) nil))
                                     ((stringp filtered)
                                      (setf (map-elt state :filtered)
                                            (concat (map-elt state :filtered) filtered))
                                      (when (and on-output (map-elt state :filtered))
                                        (funcall on-output (map-elt state :filtered)))
                                      (setf (map-elt state :output)
                                            (concat (map-elt state :output)
                                                    (map-elt state :filtered)))
                                      (setf (map-elt state :filtered) nil))
                                     (t
                                      (setf (map-elt state :filtered)
                                            (concat (map-elt state :filtered)
                                                    (format "\"%s\"" filtered)))
                                      (setf (map-elt state :output)
                                            (concat (map-elt state :output)
                                                    (map-elt state :filtered)))
                                      (when on-output
                                        (funcall on-output
                                                 (concat "\n\n:filter output must be either a string, "
                                                         "nil, or an alist of the form: \n\n"
                                                         "'((:filtered . \"...\"))\n"
                                                         "  (:pending . \"{...\")\n\n"
                                                         (format "But received (%s):\n\n" (type-of filtered))
                                                         (format "\"%s\"" filtered))))))))
                         ;; Start - Comment out to get stack traces.
                         (error
                          (when on-output
                            (funcall on-output (format "\n\n%s" err)))))
                         ;; End - Comment out to get stack traces.
                       )
             :stderr (make-pipe-process
                      :name (concat process-name "-stderr")
                      :filter (lambda (_process raw-output)
                                (log "Stderr")
                                (log "%s" raw-output)
                                (setf (map-elt state :filtered)
                                      (concat (map-elt state :filtered)
                                              raw-output))
                                (when on-output
                                  (funcall on-output (string-trim (map-elt state :filtered))))
                                (setf (map-elt state :filtered) nil))
                      :sentinel (lambda (process _event)
                                  (kill-buffer (process-buffer process))))
             :sentinel (lambda (process _event)
                         ;; Start - Comment out to get stack traces.
                         (condition-case err
                         ;; End - Comment out to get stack traces.
                             (let ((exit-status (process-exit-status process)))
                               (log "Sentinel")
                               (log "Exit status: %d" exit-status)
                               (log "Exit state: %s" state)
                               (when (and on-finished
                                          ;; Don't finish shell request if
                                          ;; there are pending incoming requests
                                          (null (map-elt state :incoming-requests)))
                                 (funcall on-finished (list
                                                       (cons :exit-status exit-status)
                                                       (cons :output (map-elt state :output)))))
                               (setf (map-elt state :filtered) nil)
                               (flush-logs))
                           ;; Start - Comment out to get stack traces.
                           (error
                            (when on-output
                              (funcall on-output (format "\n\n%s" err)))))
                           ;; End - Comment out to get stack traces.
                         )))
      shell-maker--request-process)))

(cl-defun shell-maker-make-http-request (&key async url data encoding timeout proxy
                                              headers fields filter on-output
                                              on-incoming-requests on-finished shell)
  "Make HTTP request at URL.

Optionally set:

ASYNC: Non-nil if request should be asynchronous.
DATA: Any data to be posted.
ENCODING: Defaults to ='utf-8 (as per `coding-system-for-write').
HEADERS: As a list of strings

  (\"header1: value1\")
   \"header2: value2\")

FIELDS: As a list of strings

  (\"field1=value1\")
   \"field2=value2\")

TIMEOUT: defaults to 600ms.
FILTER: An optional function filter command output.  Use it for convertions.

  (lambda (raw-text)
    ;; Must return either a string
    ;; or
    ;; an alist of the form:
    \='((:filtered . \"filtered response string\")
        (:pending . \"pending string\")))

ON-OUTPUT: (lambda (output))
ON-FINISHED: (lambda (result))."
  (unless url
    (error "Missing mandatory :url param"))
  (let ((result (shell-maker-execute-command
                 :async async
                 :command (shell-maker-make--curl-command :url url
                                                          :data data
                                                          :encoding encoding
                                                          :timeout timeout
                                                          :headers headers
                                                          :fields fields
                                                          :proxy proxy)
                 :filter filter
                 :on-output on-output
                 :on-incoming-requests on-incoming-requests
                 :on-finished on-finished
                 :shell shell)))
    (when (and (listp result)
               (map-elt result :exit-status))
      (list
       (cons :success (eq (map-elt result :exit-status) 0))
       (cons :output (map-elt result :output))))))

(cl-defun shell-maker-make--curl-command (&key url data encoding timeout headers fields proxy)
  "Build curl command list using URL.

Optionally, add:

DATA: To send.
ENCODING: Defaults to ='utf-8 (as per `coding-system-for-write').
HEADERS: As a list of strings

  (\"header1: value1\")
   \"header2: value2\")

FIELDS: As a list of strings

  (\"field1=value1\")
   \"field2=value2\")

PROXY: Set when needing an http proxy.

and TIMEOUT: defaults to 600ms."
  (unless encoding
    (setq encoding 'utf-8))
  (unless timeout
    (setq timeout 600))
  (let ((data-file (when data
                     (shell-maker--temp-file "curl-data"))))
    (when data
      (with-temp-file data-file
        (setq-local coding-system-for-write encoding)
        (insert (shell-maker--json-encode data))))
    (append (list shell-maker-curl-executable url
                  "--fail-with-body"
                  "--no-progress-meter"
                  "-m" (number-to-string timeout))
            (when proxy
              (list "--proxy" proxy))
            (apply #'append
                   (mapcar (lambda (header)
                             (list "-H" header))
                           headers))
            (apply #'append
                   (mapcar (lambda (field)
                             (list "-F" field))
                           fields))
            (when data
              (list "-d" (format "@%s" data-file))))))

(cl-defun shell-maker-execute-command (&key async command filter on-output on-incoming-requests on-finished shell log)
  "Execute COMMAND list (command + params).

ASYNC: Optionally execute COMMAND asynchronously.

FILTER: An optional function filter command output.  Use it for conversions.

  (lambda (raw-text)
    ;; Must return either a string
    ;; or
    ;; an alist of the form:
    \='((:filtered . \"filtered response string\")
        (:pending . \"pending string\")))

For directing output use:

ON-OUTPUT: (lambda (output))

ON-FINISHED: (lambda (result))

or use send to the shell using the object exposed via :execute-command

SHELL: The shell context to write command output to.

LOG: A function to log to (lambda (format &rest))."
  (unless command
    (error "Missing mandatory :command param"))
  (unless (or log (map-elt shell :log))
    (setq log (lambda (_format &rest _args))))
  (if async
      (shell-maker--execute-command-async
       :command command
       :filter filter
       :log (or log (map-elt shell :log))
       :on-output (lambda (output)
                    (when (map-elt shell :write-output)
                      (funcall (map-elt shell :write-output) output))
                    (when on-output
                      (funcall on-output output)))
       :on-incoming-requests (lambda (incoming-requests)
                    (when on-incoming-requests
                      (funcall on-incoming-requests incoming-requests)))
       :on-finished (lambda (result)
                      (when (map-elt shell :finish-output)
                        (funcall (map-elt shell :finish-output)
                                 (equal 0 (map-elt result :exit-status))))
                      (when on-finished
                        (funcall on-finished result))))
    (when (or shell
              on-output
              on-finished)
      (error ":shell, :on-output or :on-finished need :async t"))
    (shell-maker--execute-command-sync
     :command command
     :filter filter)))

;; TODO: Remove and rely on shell-maker-execute-command.
(defun shell-maker-async-shell-command (command streaming extract-response callback error-callback &optional preprocess-response)
  "Run shell COMMAND asynchronously (deprecated).

Use `shell-maker-execute-command'.

Set STREAMING to enable it.  Calls PREPROCESS-RESPONSE prior to invoking
EXTRACT-RESPONSE to extract the response and feeds it to CALLBACK or
ERROR-CALLBACK accordingly."
  (let* ((buffer (shell-maker-buffer shell-maker--config))
         (request-id (shell-maker--increment-request-id))
         (output-buffer (generate-new-buffer " *temp*"))
         (config shell-maker--config)
         (request-process (condition-case err
                              (apply #'start-process (append (list
                                                              (shell-maker-buffer-name shell-maker--config)
                                                              (buffer-name output-buffer))
                                                             command))
                            (error
                             (with-current-buffer buffer
                               (funcall error-callback (error-message-string err)))
                             nil)))
         (preparsed)
         (remaining-text)
         (process-connection-type nil))
    (when request-process
      (setq shell-maker--request-process request-process)
      (shell-maker--log config "Async Command v1")
      (shell-maker--log config command)
      (when streaming
        (set-process-filter
         request-process
         (lambda (process output)
           (condition-case nil
               (when (and (eq request-id (with-current-buffer buffer
                                           (shell-maker--current-request-id)))
                          (buffer-live-p buffer))
                 (shell-maker--log config "Filter output")
                 (shell-maker--log config output)
                 (setq remaining-text (concat remaining-text output))
                 (when preprocess-response
                   (setq remaining-text (funcall preprocess-response remaining-text)))
                 (setq preparsed (shell-maker--preparse-json remaining-text))
                 (if (car preparsed)
                     (mapc (lambda (obj)
                             (with-current-buffer buffer
                               (funcall callback (funcall extract-response obj) t)))
                           (car preparsed))
                   (with-current-buffer buffer
                     (let ((curl-exit-code (when (string-match (rx "curl: (" (group (one-or-more digit)) ")")
                                                               (cdr preparsed))
                                             (string-to-number (match-string 1 (cdr preparsed))))))
                       (cond ((eq 0 curl-exit-code)
                              (funcall callback (cdr preparsed) t))
                             ((numberp curl-exit-code)
                              (funcall error-callback (string-trim (cdr preparsed))))))))
                 (setq remaining-text (cdr preparsed)))
             (error (delete-process process))))))
      (set-process-sentinel
       request-process
       (lambda (process _event)
         (condition-case nil
             (let ((active (and (eq request-id (with-current-buffer buffer
                                                 (shell-maker--current-request-id)))
                                (buffer-live-p buffer)))
                   (output (with-current-buffer (process-buffer process)
                             (buffer-string)))
                   (exit-status (process-exit-status process)))
               (shell-maker--log config "Response (%s)" (if active "active" "inactive"))
               (shell-maker--log config "Exit status: %d" exit-status)
               (shell-maker--log config output)
               (with-current-buffer buffer
                 (if (= exit-status 0)
                     (funcall callback
                              (if (string-empty-p (string-trim output))
                                  output
                                (funcall extract-response output))
                              nil)
                   (if-let ((error (if (string-empty-p (string-trim output))
                                       output
                                     (funcall extract-response output))))
                       (funcall error-callback error)
                     (funcall error-callback output)))))
           (kill-buffer output-buffer)
           (error (delete-process process))))))))

(defun shell-maker--json-parse-string-filtering (json regexp)
  "Attempt to parse JSON.  If unsuccessful, attempt after removing REGEXP."
  (let ((json-object nil)
        (curl-lines-removed-str json))
    ;; Try parsing JSON string as is
    (condition-case nil
        (setq json-object (json-read-from-string json))
      (error nil))
    ;; If parsing fails, remove curl lines and try again
    (when (null json-object)
      (setq curl-lines-removed-str (replace-regexp-in-string regexp "" json))
      (condition-case nil
          (setq json-object (json-read-from-string curl-lines-removed-str))
        (error nil)))
    json-object))

(defun shell-maker--increment-request-id ()
  "Increment variable `shell-maker--current-request-id'."
  (unless (or (boundp 'shell-maker--current-request-id)
              (eq major-mode (shell-maker-major-mode shell-maker--config)))
    (error "Not in a shell"))
  (if (= shell-maker--current-request-id most-positive-fixnum)
      (setq shell-maker--current-request-id 0)
    (setq shell-maker--current-request-id (1+ shell-maker--current-request-id))))

(defun shell-maker--current-request-id ()
  "Access variable `shell-maker--current-request-id' with right mode ensured."
  (if (or (boundp 'shell-maker--current-request-id)
          (eq major-mode (shell-maker-major-mode shell-maker--config)))
      shell-maker--current-request-id
    (error "Not in a shell")))

(defun shell-maker--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark
               (get-buffer-process
                (shell-maker-buffer shell-maker--config))) pos))

(defun shell-maker--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process
                 (shell-maker-buffer shell-maker--config))))

(defun shell-maker--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun shell-maker--json-encode (obj)
  "Serialize OBJ to json.  Use fallback if `json-serialize' isn't available."
  (if (fboundp 'json-serialize)
      (json-serialize obj)
    (json-encode obj)))

(defun shell-maker--split-text (text)
  "Splits TEXT text into chunks.

RESPONSE is of the form:

\"
data: text1
data: text2
text3
\"

returned list is of the form:

  (((:key . \"data:\")
    (:value . \"text1\"))
   ((:key . \"data:\")
    (:value . \"text2\"))
   ((:key . nil)
    (:value . \"text3\")))"
  (if (string-prefix-p "{" text) ;; starts with { keep whole.
      (list `((:key . nil)
              (:value . ,text)))
    (let ((lines (split-string text "\n"))
          (result '()))
      (dolist (line lines)
        (if (string-match (rx (group (+ (not (any " " ":"))) ":")
                              (group (* nonl)))
                          line)
            (let* ((key (match-string 1 line))
                   (value (string-trim (match-string 2 line))))
              (push (list (cons :key key)
                          (cons :value value)) result))
          (when-let* ((value (string-trim line))
                      (non-empty (not (string-empty-p value))))
            (push (list (cons :key nil)
                        (cons :value value)) result))))
      (reverse result))))

(defun shell-maker--curl-version-supported ()
  "Return t if curl version is 7.76 or newer, nil otherwise."
  (let ((curl-version-string (shell-command-to-string (concat shell-maker-curl-executable " --version "))))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.76" version)))))

(defun shell-maker--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (error nil))
    (condition-case _err
        (json-read-from-string json)
      (error nil))))

(cl-defun shell-maker--write-partial-reply (&key config reply on-output)
  "Write partial REPLY to CONFIG shell.

Use ON-OUTPUT function to monitor output text."
  (unless config
    (error "Missing config"))
  (unless reply
    (error "Missing reply"))
  (let ((inhibit-read-only t)
        (shell-buffer (shell-maker-buffer config))
        (auto-scroll (eobp)))
    (with-current-buffer shell-buffer
      (if auto-scroll
          (progn
            (goto-char (point-max))
            (shell-maker--output-filter (shell-maker--process) reply))
        (save-excursion
          (goto-char (point-max))
          (shell-maker--output-filter (shell-maker--process) reply))))
    (when on-output
      (funcall on-output reply))))

(cl-defun shell-maker-write-output (&key config output on-output)
  "Write OUTPUT to CONFIG shell buffer.

Must be called from within the shell buffer.

Use ON-OUTPUT function to monitor output text."
  (shell-maker--write-partial-reply :config config
                                    :reply (or output "<nil-message>")
                                    :on-output on-output))

(cl-defun shell-maker-finish-output (&key config success on-output)
  "Finish output for CONFIG shell buffer.

Must be called from within the shell buffer.

SUCCESS indicates whether the command succeeded.
Use ON-OUTPUT function to monitor output text."
  (setq shell-maker--busy nil)
  (let ((auto-scroll (eobp)))
    (shell-maker--write-reply :config config
                              :reply (save-excursion
                                       (goto-char (point-max))
                                       (cond ((looking-back "\n\n" nil) "")
                                             ((looking-back "\n" nil) "\n")
                                             (t "\n\n")))
                              :on-output on-output
                              :failed (not success))
    (when auto-scroll
      (goto-char (point-max)))))

(defmacro shell-maker-with-auto-scroll-edit (&rest body)
  "Execute BODY, preserving point unless already at end of buffer."
  (save-restriction)
  `(let ((new-location))
     (if (eobp)
         (progn
           (goto-char (point-max))
           (set-marker comint-last-output-start (point))
           ,@body
           (let ((proc (get-buffer-process (current-buffer)))
                 (point (point)))
             (when (and proc (> point (process-mark proc)))
               (set-marker (process-mark proc) point))
             (setq new-location point))
           (goto-char (point-max)))
       (save-excursion
         (goto-char (point-max))
         (set-marker comint-last-output-start (point))
         ,@body
         (let ((proc (get-buffer-process (current-buffer)))
               (point (point)))
           (when (and proc (> point (process-mark proc)))
             (set-marker (process-mark proc) point))
           (setq new-location point))))
     (unless comint-use-prompt-regexp
       (with-silent-modifications
         (add-text-properties comint-last-output-start new-location
                              `(rear-nonsticky
                                ,shell-maker--prompt-rear-nonsticky
                                front-sticky
                                (field inhibit-line-move-field-capture)
                                field output
                                inhibit-line-move-field-capture t))))))

(defun shell-maker--preparse-json (json)
  "Preparse JSON and return a cons of parsed objects vs unparsed text."
  (let ((parsed)
        (remaining)
        (loc))
    ;; TODO: Remove and rely on preprocess-response
    ;; from `shell-maker-async-shell-command'.
    (setq json (replace-regexp-in-string (rx bol "data:") "" json))
    (with-temp-buffer
      (erase-buffer)
      (insert json)
      (goto-char (point-min))
      (setq loc (point))
      (while (when-let
                 ((data (ignore-errors (json-read))))
               (setq parsed (append parsed (list data)))
               (setq loc (point))))
      (setq remaining (buffer-substring-no-properties loc (point-max)))
      (cons parsed
            (string-trim remaining)))))

(defun shell-maker--command-and-response-at-point ()
  "Extract the current command and response in buffer."
  (save-excursion
    (save-restriction
      (shell-maker-narrow-to-prompt)
      (let ((items (shell-maker--extract-history
                    (buffer-string)
                    (shell-maker-prompt shell-maker--config))))
        (cl-assert (or (seq-empty-p items)
                       (eq (length items) 1)))
        (seq-first items)))))

(defun shell-maker--log (config format &rest args)
  "Write FORMAT with ARGS, using CONFIG."
  (unless format
    (setq format "<nil>"))
  (when args
    (setq format (apply #'format format args)))
  (when (and shell-maker-logging config)
    (when (shell-maker-config-redact-log-output config)
      (setq format
            (funcall (shell-maker-config-redact-log-output config) format)))
    (with-current-buffer (get-buffer-create (format "*%s-log*"
                                                    (shell-maker-process-name config)))
      (goto-char (point-max))
      (insert format))))

(defun shell-maker--temp-file (&rest components)
  "Create an absolute temp file path for COMPONENTS."
  (let* ((components (cons "shell-maker" components))
         (relative-path (apply #'file-name-concat components))
         (temp-file (expand-file-name relative-path temporary-file-directory)))
    (make-directory (file-name-directory temp-file) t)
    temp-file))

(defun shell-maker--process nil
  "Get shell buffer process."
  (get-buffer-process (shell-maker-buffer shell-maker--config)))

(defun shell-maker-save-session-transcript ()
  "Save shell transcript to file."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (if shell-maker--file
      (let ((content (buffer-string))
            (path shell-maker--file))
        (with-temp-buffer
          (insert content)
          (write-file path nil))
        (set-buffer-modified-p nil))
    (when-let ((path (read-file-name "Write file: "
				     (when shell-maker-transcript-default-path
                                       (file-name-as-directory shell-maker-transcript-default-path))
				     nil nil (funcall shell-maker-transcript-default-filename)))
               (content (buffer-string)))
      (with-temp-buffer
        (insert content)
        (write-file path t))
      (setq shell-maker--file path)
      (set-buffer-modified-p nil))))

(defun shell-maker-restore-session-from-transcript (&optional history)
  "Restore session from file transcript (or HISTORY)."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let* ((dir (when shell-maker-transcript-default-path
                (file-name-as-directory shell-maker-transcript-default-path)))
         (path (unless history
                 (read-file-name "Restore from: " dir nil t)))
         (config shell-maker--config)
         (history (or history
                      (with-temp-buffer
                        (insert-file-contents path)
                        (shell-maker--extract-history
                         (buffer-string) (shell-maker-prompt-regexp config)))))
         (execute-command (shell-maker-config-execute-command
                           config))
         (validate-command (shell-maker-config-validate-command
                            config))
         (entry)
         (failed))
    ;; Momentarily overrides request handling to replay all commands
    ;; read from file so comint treats all commands/outputs like
    ;; any other command.
    (unwind-protect
        (progn
          (setf (shell-maker-config-validate-command config) nil)
          (setf (shell-maker-config-execute-command config)
                (lambda (_command shell)
                  (when entry
                    (unless (consp entry)
                      (setq failed t)
                      (user-error "Invalid transcript"))
                    (funcall (map-elt shell :write-output) (cdr entry))
                    (funcall (map-elt shell :finish-output) t)
                    (setq entry (car history))
                    (setq history (cdr history))
                    (when entry
                      (goto-char (point-max))
                      (insert (car entry))
                      (shell-maker-submit)))))
          (goto-char (point-max))
          (comint-clear-buffer)
          (setq entry (car history))
          (setq history (cdr history))
          (when entry
            (unless (consp entry)
              (setq failed t)
              (user-error "Invalid transcript"))
            (goto-char (point-max))
            (insert (car entry))
            (shell-maker-submit)))
      (if failed
          (setq shell-maker--file nil)
        (setq shell-maker--file path))
      (setq shell-maker--busy nil)
      (setf (shell-maker-config-validate-command config)
            validate-command)
      (setf (shell-maker-config-execute-command config)
            execute-command)))
  (goto-char (point-max)))

(defun shell-maker-next-command-and-response (&optional backwards)
  "Move to next prompt and return interaction.  Return a command/response cons.

If BACKWARDS is non-nil, move backwards."
  (when-let* ((point-before (point))
              (point-after (save-excursion
                             (comint-previous-prompt (if backwards 1 -1))
                             ;; Point could be away from current prompt.
                             (when (eq (line-number-at-pos point-before)
                                       (line-number-at-pos (point)))
                               (comint-previous-prompt (if backwards 1 -1)))
                             ;; Try going back again if on the last response.
                             (when-let* ((going-back backwards)
                                         (response (cdr (shell-maker--command-and-response-at-point)))
                                         (same (equal (string-trim response)
                                                      (string-trim (shell-maker-last-output)))))
                               (comint-previous-prompt (if backwards 1 -1)))
                             (point)))
              (moved (and (not (eq point-before point-after))
                          (not (eq (line-number-at-pos point-before)
                                   (line-number-at-pos point-after))))))
    (goto-char point-after)
    (shell-maker--command-and-response-at-point)))

(defun shell-maker-history ()
  "Get all buffer commands along with respective outputs."
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (shell-maker--extract-history
   (buffer-string)
   (shell-maker-prompt-regexp shell-maker--config)))

(cl-defun shell-maker-append-history (&key items)
  "Append ITEMS to current shell buffer history."
  (shell-maker-restore-session-from-transcript
   (append (shell-maker-history)
           items)))

(defun shell-maker--extract-history (text prompt-regexp)
  "Extract all commands and respective output in TEXT with PROMPT-REGEXP.

Returns a list of (command . output) cons."
  (let ((result))
    (mapc (lambda (item)
            (let* ((values (split-string item "<shell-maker-end-of-prompt>"))
                   (lines (split-string item "\n"))
                   (prompt (string-trim (nth 0 values)))
                   (response (string-trim (progn
                                            (if (> (length values) 1)
                                                (nth 1 values)
                                              (string-join
                                               (cdr lines) "\n"))))))
              (when (or (not (string-match "<shell-maker-failed-command>" response))
                        (string-match "<shell-maker-interrupted-command>" response))
                (setq response (string-trim (replace-regexp-in-string
                                             "<shell-maker-[^>]+>" ""
                                             response)))
                (setq prompt (string-trim (replace-regexp-in-string
                                           "<shell-maker-[^>]+>" ""
                                           prompt)))
                (when (or (not (string-empty-p prompt))
                          (not (string-empty-p response)))
                  (push (cons (if (string-empty-p prompt)
                                  nil
                                prompt)
                              (if (string-empty-p response)
                                  nil
                                response))
                        result)))))
          (split-string text prompt-regexp))
    (nreverse result)))

(defun shell-maker--output-filter (process string)
  "Copy of `comint-output-filter' but avoids fontifying non-prompt text.

Uses PROCESS and STRING same as `comint-output-filter'."
  (when-let ((oprocbuf (process-buffer process)))
    (with-current-buffer oprocbuf
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (goto-char (point-max))
          (set-marker comint-last-output-start (point))
          (insert string)
          (set-marker (process-mark process) (point))
          (goto-char (process-mark process))
          (unless comint-use-prompt-regexp
            (with-silent-modifications
              (add-text-properties comint-last-output-start (point)
                                   `(rear-nonsticky
                                     ,shell-maker--prompt-rear-nonsticky
                                     front-sticky
                                     (field inhibit-line-move-field-capture)
                                     field output
                                     inhibit-line-move-field-capture t))))
          (when-let* ((prompt-start (save-excursion (forward-line 0) (point)))
                      (inhibit-read-only t)
                      (prompt (string-match
                               comint-prompt-regexp
                               (buffer-substring prompt-start (point)))))
            (with-silent-modifications
              (or (= (point-min) prompt-start)
                  (get-text-property (1- prompt-start) 'read-only)
                  (put-text-property (1- prompt-start)
                                     prompt-start 'read-only 'fence))
              (add-text-properties prompt-start (point)
                                   '(read-only t front-sticky (read-only))))
            (when comint-last-prompt
              (font-lock--remove-face-from-text-property
               (car comint-last-prompt)
               (cdr comint-last-prompt)
               'font-lock-face
               'comint-highlight-prompt))
            (setq comint-last-prompt
                  (cons (copy-marker prompt-start) (point-marker)))
            (font-lock-append-text-property prompt-start (point)
                                            'font-lock-face
                                            'comint-highlight-prompt)
            (add-text-properties prompt-start (point)
                                 `(rear-nonsticky
                                   ,shell-maker--prompt-rear-nonsticky))))))))

(defun shell-maker-buffer (config)
  "Get buffer from CONFIG."
  (get-buffer-create (shell-maker-buffer-name config)))

(defun shell-maker-buffer-name (config)
  "Get buffer name from CONFIG."
  (if shell-maker--buffer-name-override
      shell-maker--buffer-name-override
    (shell-maker-buffer-default-name (shell-maker-config-name config))))

(defun shell-maker-buffer-default-name (name)
  "Make default buffer name from NAME."
  (concat "*" (downcase name) "*"))

(defun shell-maker-major-mode (config)
  "Get major mode from CONFIG."
  (unless config
    (error "No shell-maker config available"))
  (intern (concat (downcase (shell-maker-config-name config)) "-shell-mode")))

(defun shell-maker-major-mode-map (config)
  "Get major mode map from CONFIG."
  (intern (concat (downcase (shell-maker-config-name config)) "-shell-mode-map")))

(defun shell-maker-process-name (config)
  "Get process name from CONFIG."
  (downcase (shell-maker-config-name config)))

(defun shell-maker-history-file-path (config)
  "Get history file path from CONFIG."
  (concat
   (file-name-as-directory
    (shell-maker-files-path config))
   "history"))

(defun shell-maker-files-path (config)
  "Get shell internal files path from CONFIG."
  (expand-file-name (concat
                     (file-name-as-directory
                      (downcase (shell-maker-config-name config))))
                    shell-maker-root-path))

(defun shell-maker-prompt (config)
  "Get prompt from CONFIG."
  (if (shell-maker-config-prompt config)
      (shell-maker-config-prompt config)
    (concat (shell-maker-config-name config) "> ")))

(defun shell-maker-rename-buffer ()
  "Rename current shell buffer."
  (interactive)
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (let ((new-name (string-trim
                   (read-string "Rename buffer: " (buffer-name (current-buffer))))))
    (shell-maker-set-buffer-name (current-buffer) new-name)))

(defun shell-maker-set-buffer-name (buffer new-name)
  "Set the BUFFER NEW-NAME."
  (with-current-buffer buffer
    (when (string-empty-p new-name)
      (user-error "Name shouldn't be empty"))
    (rename-buffer new-name t)
    (setq shell-maker--buffer-name-override (buffer-name (current-buffer)))))

(defun shell-maker-set-prompt (prompt prompt-regexp)
  "Set internal config's PROMPT and PROMPT-REGEXP."
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  ;; Make a copy so pointed config isn't modified.
  (let ((config (copy-sequence shell-maker--config)))
    (setf (shell-maker-config-prompt config)
          prompt)
    (setf (shell-maker-config-prompt-regexp config)
          prompt-regexp)
    (setq shell-maker--config config))
  ;; Prevents fontifying streamed response as prompt.
  (setq comint-prompt-regexp prompt-regexp)
  (setq-local imenu-generic-expression
              `((nil ,(concat "\\(" prompt-regexp "\\)" "\\(.*\\)") 2))))

(defun shell-maker-prompt-regexp (config)
  "Get prompt regexp from CONFIG."
  (if (shell-maker-config-prompt-regexp config)
      (shell-maker-config-prompt-regexp config)
    (concat "^" (shell-maker-prompt config))))

(defun shell-maker--print-help ()
  "Print help."
  (shell-maker-echo
   (let ((rows))
     (mapatoms
      (lambda (symbol)
        (when (and (string-match (concat "^" (downcase (shell-maker-config-name
                                                        shell-maker--config)) "-shell")
                                 (symbol-name symbol))
                   (commandp symbol))
          (push `(,(string-join
                    (seq-filter
                     (lambda (item)
                       (not (string-match "menu" item)))
                     (mapcar
                      (lambda (keys)
                        (propertize (key-description keys)
                                    'font-lock-face 'font-lock-string-face))
                      (or
                       (where-is-internal
                        (symbol-function symbol)
                        comint-mode-map
                        nil nil (command-remapping 'comint-next-input))
                       (where-is-internal
                        (symbol-function symbol)
                        (symbol-value
                         (shell-maker-major-mode-map shell-maker--config))
                        nil nil (command-remapping symbol))
                       (where-is-internal
                        symbol (symbol-value
                                (shell-maker-major-mode-map shell-maker--config))
                        nil nil (command-remapping symbol))))) " or ")
                  ,(propertize
                    (symbol-name symbol)
                    'font-lock-face 'font-lock-doc-face)
                  ,(car
                    (split-string
                     (or (documentation symbol t) "")
                     "\n")))
                rows))))
     (shell-maker--indent-text
      2
      (format "
Type your input and press %s to submit.

Type %s and press %s to clear all content.

%s shell is based on %s. Check out the current %s for all enabled features.

%s"
              (shell-maker--propertize-key-binding "-shell-submit" shell-maker--config)
              (propertize "clear" 'font-lock-face 'italic)
              (shell-maker--propertize-key-binding "-shell-submit" shell-maker--config)
              (propertize (shell-maker-config-name shell-maker--config)
                          'font-lock-face 'font-lock-comment-face)
              (shell-maker--actionable-text "comint-mode"
                                            (lambda ()
                                              (describe-function 'comint-mode)))
              (shell-maker--actionable-text "major mode"
                                            (lambda ()
                                              (describe-mode)))
              (shell-maker--align-docs
               ;; Commands with keybinding listed first.
               (sort rows
                     (lambda (a b)
                       (cond
                        ((and (string-empty-p (nth 0 a))
                              (string-empty-p (nth 0 b)))
                         nil)
                        ((string= (nth 0 a) "") nil)
                        ((string= (nth 0 b) "") t)
                        (t (string> (nth 0 a) (nth 0 b)))))) 3))))))

(defun shell-maker-kill-buffer-query ()
  "Added to `kill-buffer-query-functions' to prevent losing unsaved transcripts."
  (when (and shell-maker-prompt-before-killing-buffer
             shell-maker--config
             (buffer-modified-p)
             (y-or-n-p (format "Save transcript for %s?" (buffer-name))))
    (shell-maker-save-session-transcript))
  t)

(defun shell-maker-echo (text &optional keep-in-history)
  "Echo TEXT to shell.

If KEEP-IN-HISTORY, don't mark to ignore it."
  (interactive "P")
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in a shell"))
  (with-current-buffer (shell-maker-buffer shell-maker--config)
    (goto-char (point-max))
    (shell-maker--output-filter (shell-maker--process)
                                (concat
                                 text
                                 (if keep-in-history
                                     ""
                                   (propertize "\n<shell-maker-failed-command>\n"
                                               'invisible (not shell-maker--show-invisible-markers)))))
    (comint-send-input) ;; Sets shell-maker--input
    (shell-maker--output-filter
     (shell-maker--process)
     (concat "\n" (shell-maker-prompt shell-maker--config)))))

(defun shell-maker--align-docs (rows space-count)
  "Align columns in ROWS using SPACE-COUNT."
  (let ((first-col-width (apply #'max
                                (mapcar (lambda (x)
                                          (length (nth 0 x)))
                                        rows)))
        (space-str (make-string space-count ?\s)))
    (mapconcat (lambda (row)
                 (format (format "%%-%ds%s%%s\n%%-%ds%s%%s"
                                 first-col-width space-str first-col-width space-str)
                         (nth 0 row) (nth 1 row) "" (nth 2 row)))
               rows "\n\n")))

(defun shell-maker-align-columns (rows)
  "Align columns in ROWS."
  (let* ((columns (length (car rows)))
         (max-widths (cl-mapcar (lambda (column)
                                  (apply #'max
                                         (mapcar (lambda (row)
                                                   (length (format "%s" (nth column row))))
                                                 rows)))
                                (number-sequence 0 (1- columns))))
         (fmt (mapconcat
               (lambda (w)
                 (format "%%-%ds" w))
               max-widths "   ")))
    (mapconcat
     (lambda (row) (apply #'format fmt row))
     rows "\n")))

(defun shell-maker--make-ret-binding-map (fun)
  "Make (kbd \"RET\") binding map to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun shell-maker--actionable-text (text fun)
  "Make actionable TEXT invoking FUN."
  (propertize text
              'font-lock-face 'link
              'keymap (shell-maker--make-ret-binding-map
                       (lambda ()
                         (interactive)
                         (funcall fun)))))

(defun shell-maker--indent-text (n-spaces text)
  "Indent TEXT by N-SPACES."
  (replace-regexp-in-string "^" (make-string n-spaces ?\s) text t))

(defun shell-maker--propertize-key-binding (symbol-suffix config)
  "Propertize SYMBOL-SUFFIX using CONFIG."
  (mapconcat
   (lambda (keys)
     (propertize (key-description keys)
                 'font-lock-face 'font-lock-string-face))
   (where-is-internal
    (symbol-function (intern (concat (downcase (shell-maker-config-name config)) symbol-suffix)))
    (symbol-value (shell-maker-major-mode-map config))) " or "))

(defun shell-maker--buffers-with-local-var (var)
  "Get a list of buffers with a local value for VAR."
  (delq nil
        (mapcar (lambda (buffer)
                  (when (local-variable-p var buffer)
                    buffer))
                (buffer-list))))

(defun shell-maker--dump-config (config)
  "Dump CONFIG to a string."
  (concat
   "\nbuffer: " (buffer-name (current-buffer))
   "\nname: " (shell-maker-config-name config)
   "\nprompt: " (shell-maker-config-prompt config)
   "\nprompt-regexp: " (shell-maker-config-prompt-regexp config)
   (propertize "\n<shell-maker-failed-command>\n"
               'invisible (not shell-maker--show-invisible-markers))
   "\n\n"))

(defun shell-maker-make-button-text (text action)
  "Make button with TEXT and ACTION."
  (with-temp-buffer
    (insert-text-button text
                        'action
                        (lambda (_)
                          (funcall action)))
    (buffer-string)))

(cl-defun shell-maker--eval-input-on-buffer-v2 (&key input config on-output on-finished)
  "Evaluate INPUT in CONFIG's shell buffer.

Use ON-OUTPUT: function to monitor command response text.

Of the form:

 (lambda (response)
  (message \"Command: %s\" response))

Use ON-FINISHED: function to monitor when command is finished.

Of the form:

 (lambda (input output success)
  (message \"Finished: %s\" success))."
  (unless config
    (error "Missing mandatory :config param"))
  (unless input
    (error "Missing mandatory :input param"))
  (if shell-maker-logging
      (shell-maker--output-filter (shell-maker--process)
                                  "<shell-maker-end-of-prompt>")
    (shell-maker--output-filter (shell-maker--process)
                                (propertize "<shell-maker-end-of-prompt>"
                                            'invisible (not shell-maker--show-invisible-markers))))
  (shell-maker--write-partial-reply :config config
                                    :reply "\n")
  (let* ((request-id (shell-maker--increment-request-id))
         (shell-buffer (shell-maker-buffer shell-maker--config))
         (executor (shell-maker-config-execute-command config))
         (history (butlast
                   (shell-maker--extract-history
                    (with-current-buffer shell-buffer
                      (buffer-string))
                    (shell-maker-prompt-regexp config))))
         (full-output))
    (funcall executor input
             ;; shell attributes exposed to command executors.
             (list
              (cons :history history)
              (cons :log (lambda (format &rest args)
                           (apply #'shell-maker--log (append (list config format) args))))
              (cons :buffer shell-buffer)
              (cons :write-output (lambda (output &optional force)
                                    (setq output (or output "<nil-message>"))
                                    (when-let ((active (or force
                                                           (and (eq request-id (with-current-buffer shell-buffer
                                                                                 (shell-maker--current-request-id)))
                                                                (buffer-live-p shell-buffer)))))
                                      (with-current-buffer shell-buffer
                                        (shell-maker-write-output :config config
                                                                  :output output
                                                                  :on-output on-output)))
                                    (setq full-output (concat full-output output))))
              (cons :finish-output (lambda (success)
                                     (when-let ((active (and (buffer-live-p shell-buffer)
                                                             (eq request-id (with-current-buffer shell-buffer
                                                                              (shell-maker--current-request-id))))))
                                       (with-current-buffer shell-buffer
                                         (shell-maker-finish-output :config config
                                                                    :success success
                                                                    :on-output on-output)))
                                     ;; Do not execute anything requiring a shell buffer
                                     ;; after this point, as on-finished or on-finished
                                     ;; subscribers may kill the shell buffers.
                                     ;; Use let-bound values to save anything that may require
                                     ;; the shell buffer.
                                     (when on-finished
                                       (funcall on-finished input full-output success))
                                     (with-current-buffer shell-buffer
                                       (shell-maker--notify-on-command-finished
                                        :config config
                                        :input input
                                        :output full-output
                                        :success success))))))))

(cl-defun shell-maker--notify-on-command-finished (&key config input output success)
  "Notify CONFIG's :on-command-finished observer of INPUT, OUTPUT, and SUCCESS."
  (when (shell-maker-config-on-command-finished config)
    (let* ((params (func-arity (shell-maker-config-on-command-finished config)))
           (params-max (cdr params)))
      (cond ((= params-max 2)
             (funcall (shell-maker-config-on-command-finished config)
                      input
                      output))
            ((= params-max 3)
             (funcall (shell-maker-config-on-command-finished config)
                      input
                      output
                      success))
            (t
             (message (concat ":on-command-finished expects "
                              "(lambda (command output)) or "
                              "(lambda (command output success))")))))))

(provide 'shell-maker)

;;; shell-maker.el ends here
