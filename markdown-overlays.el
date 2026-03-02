;;; markdown-overlays.el --- Overlays to prettify Markdown buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
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
;; Setup:
;;
;; (require 'markdown-overlays)
;;
;; Put all supported Markdown overlays with:
;;
;;  (markdown-overlays-put)
;;
;; Remove all supported Markdown overlays with:
;;
;;  (markdown-overlays-remove)

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'org-faces)
(require 'url-parse)
(require 'url-util)

(defcustom markdown-overlays-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-insert-dividers nil
  "Whether or not to display a divider between requests and responses."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-render-latex nil
  "Whether or not to render LaTeX blocks (experimental).

Experimental.  Please report issues."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-language-mapping '(("elisp" . "emacs-lisp")
                                                ("objective-c" . "objc")
                                                ("objectivec" . "objc")
                                                ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
  :group 'markdown-overlays)

(defvar markdown-overlays--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+" "#"))) ;; languages like: emacs-lisp C++ C#
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (zero-or-more whitespace)
       (group "```") (or "\n" eol)))

(defun markdown-overlays-remove ()
  "Remove all Markdown overlays."
  (remove-overlays (point-min) (point-max) 'category 'markdown-overlays))

(defun markdown-overlays-put ()
  "Put all Markdown overlays.
Return an alist with details of all overlays added:

  `source-blocks' - fenced code blocks
  `inline-codes'  - inline code spans
  `links'         - markdown links
  `headers'       - markdown headers
  `bolds'         - bold text
  `italics'       - italic text
  `strikethroughs' - strikethrough text
  `images'         - markdown image references
  `image-file-paths' - bare image file paths on their own line
  `avoided-ranges' - list of (START . END) cons cells covering
                     source blocks and inline code spans"
  (let* ((source-blocks (markdown-overlays--source-blocks))
         (source-block-ranges (seq-map (lambda (block)
                                  (cons (car (map-elt block 'start))
                                        (cdr (map-elt block 'end))))
                                source-blocks))
         (inline-codes (markdown-overlays--markdown-inline-codes source-block-ranges))
         (inline-code-ranges (seq-map (lambda (inline)
                                        (map-elt inline 'body))
                                      inline-codes))
         (avoid-ranges (append inline-code-ranges
                               source-block-ranges))
         (links (markdown-overlays--markdown-links avoid-ranges))
         (images (markdown-overlays--markdown-images avoid-ranges))
         (image-file-paths (markdown-overlays--image-file-paths avoid-ranges))
         (headers (markdown-overlays--markdown-headers avoid-ranges))
         (bolds (markdown-overlays--markdown-bolds avoid-ranges))
         (italics (markdown-overlays--markdown-italics avoid-ranges))
         (strikethroughs (markdown-overlays--markdown-strikethroughs avoid-ranges)))
    (markdown-overlays-remove)
    (dolist (block source-blocks)
      (markdown-overlays--fontify-source-block
       (car (map-elt block 'start))
       (cdr (map-elt block 'start))
       (buffer-substring-no-properties (car (map-elt block 'language))
                                       (cdr (map-elt block 'language)))
       (car (map-elt block 'language))
       (cdr (map-elt block 'language))
       (car (map-elt block 'body))
       (cdr (map-elt block 'body))
       (car (map-elt block 'end))
       (cdr (map-elt block 'end))))
    (when markdown-overlays-insert-dividers
      (dolist (divider (markdown-overlays--divider-markers))
        (markdown-overlays--fontify-divider (car divider) (cdr divider))))
    (dolist (link links)
      (markdown-overlays--fontify-link
       (map-elt link 'start)
       (map-elt link 'end)
       (car (map-elt link 'title))
       (cdr (map-elt link 'title))
       (car (map-elt link 'url))
       (cdr (map-elt link 'url))))
    (when markdown-overlays-render-images
      (dolist (image images)
        (markdown-overlays--fontify-image
         (map-elt image 'start)
         (map-elt image 'end)
         (car (map-elt image 'url))
         (cdr (map-elt image 'url))))
      (dolist (image-file-path image-file-paths)
        (markdown-overlays--fontify-image-file-path
         (map-elt image-file-path 'start)
         (map-elt image-file-path 'end)
         (car (map-elt image-file-path 'path))
         (cdr (map-elt image-file-path 'path)))))
    (dolist (header headers)
      (markdown-overlays--fontify-header
       (map-elt header 'start)
       (map-elt header 'end)
       (car (map-elt header 'level))
       (cdr (map-elt header 'level))
       (car (map-elt header 'title))
       (cdr (map-elt header 'title))
       (map-elt header 'needs-trailing-newline)))
    (dolist (bold bolds)
      (markdown-overlays--fontify-bold
       (map-elt bold 'start)
       (map-elt bold 'end)
       (car (map-elt bold 'text))
       (cdr (map-elt bold 'text))))
    (dolist (italic italics)
      (markdown-overlays--fontify-italic
       (map-elt italic 'start)
       (map-elt italic 'end)
       (car (map-elt italic 'text))
       (cdr (map-elt italic 'text))))
    (dolist (strikethrough strikethroughs)
      (markdown-overlays--fontify-strikethrough
       (map-elt strikethrough 'start)
       (map-elt strikethrough 'end)
       (car (map-elt strikethrough 'text))
       (cdr (map-elt strikethrough 'text))))
    (dolist (inline-code inline-codes)
      (markdown-overlays--fontify-inline-code
       (car (map-elt inline-code 'body))
       (cdr (map-elt inline-code 'body))))
    (when markdown-overlays-render-latex
      (require 'org)
      ;; Silence org-element warnings.
      (let ((major-mode 'org-mode))
        (save-excursion
          (dolist (range (markdown-overlays--invert-ranges
                          avoid-ranges
                          (point-min)
                          (point-max)))
            (org-format-latex
             (concat org-preview-latex-image-directory "markdown-overlays")
             (car range) (cdr range)
             temporary-file-directory
             'overlays nil 'forbuffer org-preview-latex-default-process)))))
    `((source-blocks . ,source-blocks)
      (inline-codes . ,inline-codes)
      (links . ,links)
      (images . ,images)
      (image-file-paths . ,image-file-paths)
      (headers . ,headers)
      (bolds . ,bolds)
      (italics . ,italics)
      (strikethroughs . ,strikethroughs)
      (avoided-ranges . ,avoid-ranges))))

(defun markdown-overlays--match-source-block ()
  "Return a matched source block by the previous search/regexp operation."
  (list
   'start (cons (match-beginning 1)
                (match-end 1))
   'end (cons (match-beginning 4)
              (match-end 4))
   'language (when (and (match-beginning 2)
                        (match-end 2))
               (cons (match-beginning 2)
                     (match-end 2)))
   'body (cons (match-beginning 3) (match-end 3))))

(defun markdown-overlays--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              markdown-overlays--source-block-regexp
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push (markdown-overlays--match-source-block)
                markdown-blocks))))
    (nreverse markdown-blocks)))

(defun markdown-overlays--put (overlay &rest props)
  "Set multiple properties on OVERLAY via PROPS."
  (unless (= (mod (length props) 2) 0)
    (error "Props missing a property or value"))
  (while props
    (overlay-put overlay 'category 'markdown-overlays)
    (overlay-put overlay (pop props) (pop props))))

(defun markdown-overlays--resolve-internal-language (language)
  "Resolve external Markdown LANGUAGE to Emacs internal.

For example \"elisp\" -> \"emacs-lisp\"."
  (when language
    (or (map-elt markdown-overlays-language-mapping
                 (downcase (string-trim language)))
        (when (intern (concat (downcase (string-trim language))
                              "-mode"))
          (downcase (string-trim language))))))

(defun markdown-overlays--fontify-source-block (quotes1-start
                                                quotes1-end
                                                lang
                                                lang-start
                                                lang-end
                                                body-start
                                                body-end
                                                quotes2-start
                                                quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Overlay beginning "```" with a copy block button.
  (markdown-overlays--put
   (make-overlay quotes1-start quotes1-end)
   'evaporate t
   'display
   (propertize "📋 "
               'pointer 'hand
               'keymap (markdown-overlays--make-ret-binding-map
                        (lambda ()
                          (interactive)
                          (kill-ring-save body-start body-end)
                          (message "Copied")))))
  ;; Hide end "```" altogether.
  (markdown-overlays--put
   (make-overlay quotes2-start quotes2-end)
   'evaporate t
   'invisible 't)
  (unless (eq lang-start lang-end)
    (markdown-overlays--put
     (make-overlay lang-start lang-end)
     'evaporate t
     'face '(:box t))
    (markdown-overlays--put
     (make-overlay lang-end (1+ lang-end))
     'evaporate t
     'display "\n\n"))
  (let ((lang-mode (intern (concat (or
                                    (markdown-overlays--resolve-internal-language lang)
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    (if (and markdown-overlays-highlight-blocks
             (fboundp lang-mode))
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *markdown-overlays-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil)
                        (inhibit-message t))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ body-start pos)
                                        (+ body-start (1+ pos))))
            (markdown-overlays--put
             overlay
             'evaporate t
             'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (markdown-overlays--put
       (make-overlay body-start body-end)
       'evaporate t
       'face 'font-lock-doc-markup-face))))

(defun markdown-overlays--fontify-divider (start end)
  "Display text between START and END as a divider."
  (markdown-overlays--put
   (make-overlay start end)
   'evaporate t
   'display
   (concat (propertize (concat (make-string (window-body-width) ? ) "")
                       'face '(:underline t)) "\n")))

(defun markdown-overlays--markdown-links (&optional avoid-ranges)
  "Extract markdown links with AVOID-RANGES."
  (let ((links '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (seq "["
                       (group (one-or-more (not (any "]"))))
                       "]"
                       "("
                       (group (one-or-more (not (any ")"))))
                       ")"))
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (title-start (match-beginning 1))
                 (title-end (match-end 1))
                 (url-start (match-beginning 2))
                 (url-end (match-end 2)))
            (unless (or (eq (char-before begin) ?!)
                        (seq-find (lambda (avoided)
                                    (and (>= begin (car avoided))
                                         (<= end (cdr avoided))))
                                  avoid-ranges))
              (push
               (list
                'start begin
                'end end
                'title (cons title-start title-end)
                'url (cons url-start url-end))
               links))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete link match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse links)))

(defun markdown-overlays--markdown-headers (&optional avoid-ranges)
  "Extract markdown headers with AVOID-RANGES."
  (let ((headers '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (zero-or-more space) (group (one-or-more "#"))
                  (one-or-more space)
                  (group (one-or-more (not (any "\n")))) eol)
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (level-start (match-beginning 1))
                 (level-end (match-end 1))
                 (title-start (match-beginning 2))
                 (title-end (match-end 2)))
            (unless (seq-find (lambda (avoided)
                                (and (>= begin (car avoided))
                                     (<= end (cdr avoided))))
                              avoid-ranges)
              (push
               (list
                'start begin
                'end end
                'level (cons level-start level-end)
                'title (cons title-start title-end)
                'needs-trailing-newline (save-excursion
                                          (goto-char end)
                                          (and (not (eobp))
                                               (not (looking-at-p "\n[ \t]*$"))
                                               (not (looking-at-p "\n\n")))))
               headers))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete header match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse headers)))

(defun markdown-overlays--open-local-link (url)
  "Open URL as a local file link if possible.
Return non-nil if handled, nil otherwise."
  (when-let ((parsed (markdown-overlays--parse-local-link url)))
    (find-file (map-elt parsed :file))
    (when (map-elt parsed :line)
      (goto-char (point-min))
      (forward-line (1- (map-elt parsed :line))))
    t))

(defun markdown-overlays--open-link (url)
  "Open URL.  Use local navigation for file links, `browse-url' otherwise."
  (unless (markdown-overlays--open-local-link url)
    (browse-url url)))

(defun markdown-overlays--fontify-link (start end title-start title-end url-start url-end)
  "Fontify a markdown link.
Use START END TITLE-START TITLE-END URL-START URL-END."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay start title-start)
   'evaporate t
   'invisible 't)
  ;; Show title as link
  (markdown-overlays--put
   (make-overlay title-start title-end)
   'evaporate t
   'face 'link)
  ;; Make RET open the URL
  (define-key (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                            (lambda () (interactive)
                              (markdown-overlays--open-link
                               (buffer-substring-no-properties url-start url-end))))
                (define-key map (kbd "RET")
                            (lambda () (interactive)
                              (markdown-overlays--open-link
                               (buffer-substring-no-properties url-start url-end))))
                (markdown-overlays--put
                 (make-overlay title-start title-end)
                 'evaporate t
                 'keymap map)
                map)
              [remap self-insert-command] 'ignore)
  ;; Hide markup after
  (markdown-overlays--put
   (make-overlay title-end end)
   'evaporate t
   'invisible 't))

(defun markdown-overlays--markdown-bolds (&optional avoid-ranges)
  "Extract markdown bolds with AVOID-RANGES."
  (let ((bolds '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or line-start (syntax whitespace))
                  (group
                   (or (seq "**" (group (one-or-more (not (any "\n*")))) "**")
                       (seq "__" (group (one-or-more (not (any "\n_")))) "__")))
                  (or (syntax punctuation) (syntax whitespace) line-end))
              nil t)
        (if-let ((begin (match-beginning 1))
                 (end (match-end 1))
                 (text-start (or (match-beginning 2)
                                 (match-beginning 3)))
                 (text-end (or (match-end 2)
                               (match-end 3))))
            (unless (seq-find (lambda (avoided)
                                (and (>= begin (car avoided))
                                     (<= end (cdr avoided))))
                              avoid-ranges)
              (push
               (list
                'start begin
                'end end
                'text (cons text-start text-end))
               bolds))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete bold match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse bolds)))

(defun markdown-overlays--markdown-italics (&optional avoid-ranges)
  "Extract markdown italics with AVOID-RANGES."
  (let ((italics '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (group (or bol (one-or-more (any "\n \t")))
                             (group "*")
                             (group (one-or-more (not (any "\n*")))) "*")
                      (group (or bol (one-or-more (any "\n \t")))
                             (group "_")
                             (group (one-or-more (not (any "\n_")))) "_")))
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (start-pos (or (match-beginning 2)
                                (match-beginning 5)))
                 (text-start (or (match-beginning 3)
                                 (match-beginning 6)))
                 (text-end (or (match-end 3)
                               (match-end 6))))
            (unless (seq-find (lambda (avoided)
                                (and (>= begin (car avoided))
                                     (<= end (cdr avoided))))
                              avoid-ranges)
              (push
               (list
                'start start-pos
                'end end
                'text (cons text-start text-end))
               italics))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete italic match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse italics)))

(defun markdown-overlays--fontify-header (_start end level-start level-end title-start title-end &optional needs-trailing-newline)
  "Fontify a markdown header.
Use START END LEVEL-START LEVEL-END TITLE-START TITLE-END and
NEEDS-TRAILING-NEWLINE."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay level-start title-start)
   'evaporate t
   'invisible 't)
  ;; Show title as header
  (markdown-overlays--put
   (make-overlay title-start title-end)
   'evaporate t
   'face
   (cond ((eq (- level-end level-start) 1)
          'org-level-1)
         ((eq (- level-end level-start) 2)
          'org-level-2)
         ((eq (- level-end level-start) 3)
          'org-level-3)
         ((eq (- level-end level-start) 4)
          'org-level-4)
         ((eq (- level-end level-start) 5)
          'org-level-5)
         ((eq (- level-end level-start) 6)
          'org-level-6)
         ((eq (- level-end level-start) 7)
          'org-level-7)
         ((eq (- level-end level-start) 8)
          'org-level-8)
         (t
          'org-level-1)))
  (when (and needs-trailing-newline (< end (point-max)))
    (markdown-overlays--put
     (make-overlay (1+ end) (1+ end))
     'evaporate t
     'before-string "\n")))

(defun markdown-overlays--fontify-bold (start end text-start text-end)
  "Fontify a markdown bold.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay start text-start)
   'evaporate t
   'invisible 't)
  ;; Show title as bold
  (markdown-overlays--put
   (make-overlay text-start text-end)
   'evaporate t
   'face 'bold)
  ;; Hide markup after
  (markdown-overlays--put
   (make-overlay text-end end)
   'evaporate t
   'invisible 't))

(defun markdown-overlays--fontify-italic (start end text-start text-end)
  "Fontify a markdown italic.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay start text-start)
   'evaporate t
   'invisible 't)
  ;; Show title as italic
  (markdown-overlays--put
   (make-overlay text-start text-end)
   'evaporate t
   'face 'italic)
  ;; Hide markup after
  (markdown-overlays--put
   (make-overlay text-end end)
   'evaporate t
   'invisible 't))

(defun markdown-overlays--markdown-strikethroughs (&optional avoid-ranges)
  "Extract markdown strikethroughs with AVOID-RANGES."
  (let ((strikethroughs '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "~~" (group (one-or-more (not (any "\n~")))) "~~")
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (text-start (match-beginning 1))
                 (text-end (match-end 1)))
            (unless (seq-find (lambda (avoided)
                                (and (>= begin (car avoided))
                                     (<= end (cdr avoided))))
                              avoid-ranges)
              (push
               (list
                'start begin
                'end end
                'text (cons text-start text-end))
               strikethroughs))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete strikethrough match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse strikethroughs)))

(defun markdown-overlays--fontify-strikethrough (start end text-start text-end)
  "Fontify a markdown strikethrough.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay start text-start)
   'evaporate t
   'invisible 't)
  ;; Show title as strikethrough
  (markdown-overlays--put
   (make-overlay text-start text-end)
   'evaporate t
   'face '(:strike-through t))
  ;; Hide markup after
  (markdown-overlays--put
   (make-overlay text-end end)
   'evaporate t
   'invisible 't))

(defun markdown-overlays--markdown-inline-codes (&optional avoid-ranges)
  "Get a list of all inline markdown code in buffer with AVOID-RANGES."
  (let ((codes '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "`\\([^`\n]+\\)`"
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (body-start (match-beginning 1))
                 (body-end (match-end 1)))
            (if-let ((avoided (seq-find (lambda (avoided)
                                          (not (or (> begin (cdr avoided))
                                                   (< end (car avoided)))))
                                        avoid-ranges)))
                ;; Match overlaps an avoid range — skip past it and retry
                (goto-char (1+ (cdr avoided)))
              (push
               (list
                'body (cons body-start body-end)) codes))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete inline code match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse codes)))

(defun markdown-overlays--fontify-inline-code (body-start body-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (markdown-overlays--put
   (make-overlay (1- body-start)
                 body-start)
   'evaporate t
   'invisible 't)
  (markdown-overlays--put
   (make-overlay body-end
                 (1+ body-end))
   'evaporate t
   'invisible 't)
  (markdown-overlays--put
   (make-overlay body-start body-end)
   'evaporate t
   'face 'font-lock-doc-markup-face))

(defun markdown-overlays--invert-ranges (ranges min max)
  "Invert a list of RANGES within the interval [MIN, MAX].
Each range is a cons of start and end integers."
  (let ((result nil)
        (start min))
    (dolist (range ranges)
      (when (< start (car range))
        (push (cons start (car range)) result))
      (setq start (cdr range)))
    (when (< start max)
      (push (cons start max) result))
    result))

(defun markdown-overlays--make-ret-binding-map (fun)
  "Make (kbd \"RET\") binding map to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun markdown-overlays--divider-markers ()
  "Return locations of all recognized divider lines."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (rx line-start
                       (* (any " \t"))
                       ;; TODO: Remove shell-maker specific regex.
                       (or "<shell-maker-end-of-prompt>"
                           (or (seq "***" (* "*"))
                               (seq "---" (* "-"))
                               (seq "___" (* "_"))))
                       (* (any " \t"))
                       line-end))
          matches)
      (while (re-search-forward pattern nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches))
      (nreverse matches))))

(cl-defun markdown-overlays-make-local-file-link (filename &key line)
  "Convert FILENAME to a Markdown link.

Returns a string like '[file.txt](file:///absolute/path/to/file.txt)'
if the file exists, nil otherwise.

When LINE is non-nil, appends #L<line> to both title and URI.

For example:

  (markdown-overlays-make-local-file-link \"foo.el\")
    => \"[foo.el](file:///absolute/path/foo.el)\"

  (markdown-overlays-make-local-file-link \"foo.el\" :line 10)
    => \"[foo.el#L10](file:///absolute/path/foo.el#L10)\""
  (when (file-exists-p filename)
    (let ((file-uri (concat "file://" (expand-file-name filename)))
          (basename (file-name-nondirectory (expand-file-name filename)))
          (suffix (if line (format "#L%d" line) "")))
      (format "[%s%s](%s%s)" basename suffix file-uri suffix))))

(defun markdown-overlays-expand-local-links (markdown)
  "Expand file:// links in MARKDOWN to code blocks with file contents.

Transforms links like [file.txt](file:///absolute/path/to/file.txt)
into:

```file.txt
Content of file.txt
```"
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](file://\\([^)]+\\))" nil t)
      (let* ((filename (match-string 1))
             (filepath (match-string 2))
             (match-start (match-beginning 0))
             (match-end (match-end 0)))
        (when (file-exists-p filepath)
          (goto-char match-start)
          (delete-region match-start match-end)
          (insert (format "\n```%s\n%s\n```\n"
                          filename (with-temp-buffer
                                     (insert-file-contents filepath)
                                     (buffer-string)))))))
    (buffer-string)))

(defun markdown-overlays--parse-local-link (url)
  "Parse URL as a local file link.
Return alist with :file and :line if URL points to an existing file.

For example:

  \"foo.el#L10\"              => ((:file . \"/abs/foo.el\") (:line . 10))
  \"file:src/bar.el:5\"       => ((:file . \"/abs/src/bar.el\") (:line . 5))
  \"file:///tmp/baz.el#L20\"  => ((:file . \"/tmp/baz.el\") (:line . 20))
  \"file:///tmp/baz.el\"      => ((:file . \"/tmp/baz.el\") (:line . nil))
  \"https://example.com\"     => nil"
  (when-let ((match
              (cond
               ;; file:///absolute/path with optional #L123 or :123
               ((string-match
                 (rx bos "file://"
                     (group (+? anything))
                     (optional (or (seq "#L" (group (one-or-more digit)))
                                   (seq ":" (group (one-or-more digit)))))
                     eos)
                 url)
                (cons (match-string 1 url)
                      (or (match-string 2 url) (match-string 3 url))))
               ;; file:relative/path with optional #L123 or :123
               ((string-match
                 (rx bos "file:"
                     (group (not (any "/")) (+? anything))
                     (optional (or (seq "#L" (group (one-or-more digit)))
                                   (seq ":" (group (one-or-more digit)))))
                     eos)
                 url)
                (cons (match-string 1 url)
                      (or (match-string 2 url) (match-string 3 url))))
               ;; path#L123 (GitHub-style line)
               ((string-match
                 (rx bos
                     (group (one-or-more (not (any ":#"))))
                     "#L" (group (one-or-more digit))
                     eos)
                 url)
                (cons (match-string 1 url) (match-string 2 url)))
               ;; path:123 (colon line number)
               ((string-match
                 (rx bos
                     (group (one-or-more (not (any ":#"))))
                     ":" (group (one-or-more digit))
                     eos)
                 url)
                (cons (match-string 1 url) (match-string 2 url)))))
             (filepath (expand-file-name (car match))))
    (when (file-exists-p filepath)
      (list (cons :file filepath)
            (cons :line (when (cdr match)
                          (string-to-number (cdr match))))))))

;;; Images

(defvar markdown-overlays-render-images t
  "Whether or not to render inline images.
When non-nil, markdown image syntax and bare image file paths are
displayed as images.")

(defvar markdown-overlays-image-max-width 0.4
  "Maximum width in pixels for inline images.
An integer value is used as pixels directly.  A float between 0 and
1 is treated as a ratio of the window body width.")

(defun markdown-overlays--markdown-images (&optional avoid-ranges)
  "Extract markdown image references with AVOID-RANGES."
  (let ((images '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "!"
                  "["
                  (group (*? (not (any "]"))))
                  "]"
                  "("
                  (group (one-or-more (not (any ")"))))
                  ")")
              nil t)
        (when-let* ((begin (match-beginning 0))
                    (end (match-end 0))
                    (title-start (match-beginning 1))
                    (title-end (match-end 1))
                    (url-start (match-beginning 2))
                    (url-end (match-end 2))
                    ((not (seq-find (lambda (avoided)
                                      (and (>= begin (car avoided))
                                           (<= end (cdr avoided))))
                                    avoid-ranges))))
          (push
           (list
            'start begin
            'end end
            'title (cons title-start title-end)
            'url (cons url-start url-end))
           images))))
    (nreverse images)))

(defun markdown-overlays--image-file-paths (&optional avoid-ranges)
  "Extract bare image file paths on their own line with AVOID-RANGES.
Matches local paths and file:// URIs ending in a supported image
extension.  Does not match http:// or https:// URLs.  Only paths
that appear alone on a line (ignoring surrounding whitespace) are
matched."
  (when-let* ((extensions image-file-name-extensions)
              (ext-re (concat
                       "\\."
                       (regexp-opt extensions t))))
    (let ((paths '())
          (case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^[ \t]*\\(\\(?:file://\\|[/~.]\\)[^ \t\n]*"
                        ext-re "\\)[ \t]*$")
                nil t)
          (let ((begin (match-beginning 0))
                (end (match-end 0))
                (path-start (match-beginning 1))
                (path-end (match-end 1)))
            (when (and (not (seq-find (lambda (avoided)
                                        (and (>= begin (car avoided))
                                             (<= end (cdr avoided))))
                                      avoid-ranges))
                       (image-supported-file-p
                        (buffer-substring-no-properties path-start path-end))
                       (markdown-overlays--resolve-image-url
                        (buffer-substring-no-properties path-start path-end)))
              (push
               (list
                'start begin
                'end end
                'path (cons path-start path-end))
               paths)))))
      (nreverse paths))))

(defun markdown-overlays--resolve-image-url (url)
  "Resolve image URL to a local file path or nil.
Handles file:// URIs, absolute paths, relative paths, and ~/ paths."
  (when-let* ((path (cond
                     ((string-prefix-p "file://" url)
                      (url-unhex-string
                       (url-filename (url-generic-parse-url url))))
                     ((string-prefix-p "file:" url)
                      (substring url (length "file:")))
                     ((or (file-name-absolute-p url)
                          (string-prefix-p "~" url)
                          (string-prefix-p "./" url)
                          (string-prefix-p "../" url))
                      url)))
              (expanded (expand-file-name path))
              ((file-exists-p expanded)))
    expanded))

(defun markdown-overlays--image-max-width ()
  "Return the max image width in pixels.
Resolves `markdown-overlays-image-max-width' which can be an
integer (pixels) or a float (ratio of window body width)."
  (if (floatp markdown-overlays-image-max-width)
      (let ((window (or (get-buffer-window (current-buffer))
                        (frame-first-window))))
        (round (* markdown-overlays-image-max-width
                  (window-body-width window t))))
    markdown-overlays-image-max-width))

(defun markdown-overlays--fontify-image (start end url-start url-end)
  "Fontify a markdown image between START and END.
URL-START and URL-END delimit the image URL."
  (when-let* ((url (buffer-substring-no-properties url-start url-end))
              (path (markdown-overlays--resolve-image-url url))
              ((image-supported-file-p path))
              ((display-graphic-p))
              (image (create-image path nil nil
                                   :max-width (markdown-overlays--image-max-width))))
    (markdown-overlays--put
     (make-overlay start end)
     'evaporate t
     'display image
     'keymap (markdown-overlays--make-ret-binding-map
              (lambda ()
                (interactive)
                (find-file path))))))

(defun markdown-overlays--fontify-image-file-path (start end path-start path-end)
  "Fontify a bare image file path between START and END.
PATH-START and PATH-END delimit the path text."
  (when-let* ((raw (buffer-substring-no-properties path-start path-end))
              (path (markdown-overlays--resolve-image-url raw))
              ((display-graphic-p))
              (image (create-image path nil nil
                                   :max-width (markdown-overlays--image-max-width))))
    (markdown-overlays--put
     (make-overlay start end)
     'evaporate t
     'display image
     'keymap (markdown-overlays--make-ret-binding-map
              (lambda ()
                (interactive)
                (find-file path))))))

(provide 'markdown-overlays)

;;; markdown-overlays.el ends here
