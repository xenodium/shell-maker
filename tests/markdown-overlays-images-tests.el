;;; markdown-overlays-images-tests.el --- Tests for image extraction -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'markdown-overlays)

;;; Helpers

(defvar markdown-overlays-images-tests--temp-png nil
  "Path to a temporary PNG file for testing.")

(defun markdown-overlays-images-tests--setup ()
  "Create a temporary PNG for tests that need an existing file."
  (unless (and markdown-overlays-images-tests--temp-png
               (file-exists-p markdown-overlays-images-tests--temp-png))
    (setq markdown-overlays-images-tests--temp-png
          (make-temp-file "md-overlay-test-" nil ".png"))
    ;; Minimal valid PNG (1x1 transparent pixel).
    (with-temp-file markdown-overlays-images-tests--temp-png
      (set-buffer-multibyte nil)
      (insert
       "\x89PNG\r\n\x1a\n"
       "\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01"
       "\x08\x06\x00\x00\x00\x1f\x15\xc4\x89"
       "\x00\x00\x00\nIDATx\x9cc\x00\x01\x00\x00\x05\x00\x01"
       "\r\n\xb4\x00\x00\x00\x00IEND\xaeB`\x82")))
  markdown-overlays-images-tests--temp-png)

;;; markdown-overlays--markdown-images

(ert-deftest markdown-overlays-images-test-basic-image ()
  "Match a basic markdown image."
  (with-temp-buffer
    (insert "![alt text](image.png)\n")
    (let ((images (markdown-overlays--markdown-images)))
      (should (= (length images) 1))
      (let ((img (car images)))
        (should (= (map-elt img 'start) 1))
        (should (= (map-elt img 'end) 23))
        (should (string= (buffer-substring-no-properties
                           (car (map-elt img 'title))
                           (cdr (map-elt img 'title)))
                          "alt text"))
        (should (string= (buffer-substring-no-properties
                           (car (map-elt img 'url))
                           (cdr (map-elt img 'url)))
                          "image.png"))))))

(ert-deftest markdown-overlays-images-test-empty-alt ()
  "Match a markdown image with empty alt text."
  (with-temp-buffer
    (insert "![](image.png)\n")
    (let ((images (markdown-overlays--markdown-images)))
      (should (= (length images) 1))
      (should (string= (buffer-substring-no-properties
                          (car (map-elt (car images) 'title))
                          (cdr (map-elt (car images) 'title)))
                        "")))))

(ert-deftest markdown-overlays-images-test-multiple-images ()
  "Match multiple markdown images."
  (with-temp-buffer
    (insert "![a](one.png)\n![b](two.png)\n")
    (let ((images (markdown-overlays--markdown-images)))
      (should (= (length images) 2)))))

(ert-deftest markdown-overlays-images-test-avoided-in-code-block ()
  "Images inside a code block should be skipped."
  (with-temp-buffer
    (insert "```\n![alt](image.png)\n```\n")
    (let* ((avoid-ranges (list (cons 1 (point-max))))
           (images (markdown-overlays--markdown-images avoid-ranges)))
      (should (= (length images) 0)))))

(ert-deftest markdown-overlays-images-test-avoided-in-inline-code ()
  "Images inside an inline code range should be skipped."
  (with-temp-buffer
    (insert "`![alt](image.png)`\n")
    (let* ((avoid-ranges (list (cons 1 (point-max))))
           (images (markdown-overlays--markdown-images avoid-ranges)))
      (should (= (length images) 0)))))

(ert-deftest markdown-overlays-images-test-not-confused-with-link ()
  "A plain link [text](url) should not be matched as an image."
  (with-temp-buffer
    (insert "[not an image](url.png)\n")
    (let ((images (markdown-overlays--markdown-images)))
      (should (= (length images) 0)))))

(ert-deftest markdown-overlays-images-test-image-among-text ()
  "Image embedded in surrounding text should be matched."
  (with-temp-buffer
    (insert "before ![alt](pic.png) after\n")
    (let ((images (markdown-overlays--markdown-images)))
      (should (= (length images) 1))
      (should (string= (buffer-substring-no-properties
                          (car (map-elt (car images) 'url))
                          (cdr (map-elt (car images) 'url)))
                        "pic.png")))))

;;; markdown-overlays--image-file-paths

(ert-deftest markdown-overlays-image-file-paths-test-absolute ()
  "Match an absolute path to an existing image."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert png "\n")
      (let ((paths (markdown-overlays--image-file-paths)))
        (should (= (length paths) 1))
        (should (string= (buffer-substring-no-properties
                            (car (map-elt (car paths) 'path))
                            (cdr (map-elt (car paths) 'path)))
                          png))))))

(ert-deftest markdown-overlays-image-file-paths-test-file-uri ()
  "Match a file:// URI to an existing image."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert (concat "file://" png) "\n")
      (let ((paths (markdown-overlays--image-file-paths)))
        (should (= (length paths) 1))))))

(ert-deftest markdown-overlays-image-file-paths-test-nonexistent ()
  "A path to a nonexistent file should not match."
  (with-temp-buffer
    (insert "/tmp/does-not-exist-ever.png\n")
    (let ((paths (markdown-overlays--image-file-paths)))
      (should (= (length paths) 0)))))

(ert-deftest markdown-overlays-image-file-paths-test-not-on-own-line ()
  "A path embedded in a line with other text should not match."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert (concat "see " png " here\n"))
      (let ((paths (markdown-overlays--image-file-paths)))
        (should (= (length paths) 0))))))

(ert-deftest markdown-overlays-image-file-paths-test-with-whitespace ()
  "A path on its own line with surrounding whitespace should match."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert (concat "  " png "  \n"))
      (let ((paths (markdown-overlays--image-file-paths)))
        (should (= (length paths) 1))))))

(ert-deftest markdown-overlays-image-file-paths-test-avoided-in-code-block ()
  "Paths inside a code block should be skipped."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert (concat "```\n" png "\n```\n"))
      (let* ((avoid-ranges (list (cons 1 (point-max))))
             (paths (markdown-overlays--image-file-paths avoid-ranges)))
        (should (= (length paths) 0))))))

(ert-deftest markdown-overlays-image-file-paths-test-no-https ()
  "https:// URLs should not be matched."
  (with-temp-buffer
    (insert "https://example.com/image.png\n")
    (let ((paths (markdown-overlays--image-file-paths)))
      (should (= (length paths) 0)))))

(ert-deftest markdown-overlays-image-file-paths-test-no-http ()
  "http:// URLs should not be matched."
  (with-temp-buffer
    (insert "http://example.com/image.png\n")
    (let ((paths (markdown-overlays--image-file-paths)))
      (should (= (length paths) 0)))))

;;; markdown-overlays--resolve-image-url

(ert-deftest markdown-overlays-resolve-test-absolute-path ()
  "Resolve an absolute path to an existing file."
  (let ((png (markdown-overlays-images-tests--setup)))
    (should (string= (markdown-overlays--resolve-image-url png)
                      (expand-file-name png)))))

(ert-deftest markdown-overlays-resolve-test-file-uri ()
  "Resolve a file:// URI."
  (let ((png (markdown-overlays-images-tests--setup)))
    (should (string= (markdown-overlays--resolve-image-url
                       (concat "file://" png))
                      (expand-file-name png)))))

(ert-deftest markdown-overlays-resolve-test-file-colon ()
  "Resolve a file: (no //) path."
  (let ((png (markdown-overlays-images-tests--setup)))
    (should (string= (markdown-overlays--resolve-image-url
                       (concat "file:" png))
                      (expand-file-name png)))))

(ert-deftest markdown-overlays-resolve-test-tilde ()
  "Resolve a ~/ path."
  (let* ((home (expand-file-name "~"))
         (png (markdown-overlays-images-tests--setup))
         (relative (concat "~/" (file-relative-name png home))))
    (should (string= (markdown-overlays--resolve-image-url relative)
                      (expand-file-name png)))))

(ert-deftest markdown-overlays-resolve-test-nonexistent ()
  "Return nil for nonexistent file."
  (should-not (markdown-overlays--resolve-image-url "/tmp/no-such-file.png")))

(ert-deftest markdown-overlays-resolve-test-https ()
  "Return nil for https URLs."
  (should-not (markdown-overlays--resolve-image-url "https://example.com/img.png")))

(ert-deftest markdown-overlays-resolve-test-http ()
  "Return nil for http URLs."
  (should-not (markdown-overlays--resolve-image-url "http://example.com/img.png")))

;;; markdown-overlays-put integration

(ert-deftest markdown-overlays-images-test-put-returns-images ()
  "markdown-overlays-put should return parsed images in the alist."
  (with-temp-buffer
    (insert "![alt](pic.png)\n")
    (let ((result (markdown-overlays-put)))
      (should (= (length (map-elt result 'images)) 1)))))

(ert-deftest markdown-overlays-images-test-put-returns-image-file-paths ()
  "markdown-overlays-put should return parsed image-file-paths in the alist."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert png "\n")
      (let ((result (markdown-overlays-put)))
        (should (= (length (map-elt result 'image-file-paths)) 1))))))

(ert-deftest markdown-overlays-images-test-put-excludes-images-in-code-block ()
  "Images inside code blocks should not appear in the returned alist."
  (with-temp-buffer
    (insert "```\n![alt](pic.png)\n```\n")
    (let ((result (markdown-overlays-put)))
      (should (= (length (map-elt result 'images)) 0)))))

(ert-deftest markdown-overlays-images-test-put-excludes-paths-in-code-block ()
  "Paths inside code blocks should not appear in the returned alist."
  (let ((png (markdown-overlays-images-tests--setup)))
    (with-temp-buffer
      (insert (concat "```\n" png "\n```\n"))
      (let ((result (markdown-overlays-put)))
        (should (= (length (map-elt result 'image-file-paths)) 0))))))

;;; Link vs image disambiguation

(ert-deftest markdown-overlays-images-test-link-not-matched-as-image ()
  "markdown-overlays--markdown-links should not match image syntax."
  (with-temp-buffer
    (insert "![alt](image.png)\n")
    (let ((links (markdown-overlays--markdown-links)))
      (should (= (length links) 0)))))

(ert-deftest markdown-overlays-images-test-link-after-image ()
  "A link right after an image should both be matched correctly."
  (with-temp-buffer
    (insert "![alt](image.png) [click](url)\n")
    (let ((images (markdown-overlays--markdown-images))
          (links (markdown-overlays--markdown-links)))
      (should (= (length images) 1))
      (should (= (length links) 1)))))

(provide 'markdown-overlays-images-tests)

;;; markdown-overlays-images-tests.el ends here
