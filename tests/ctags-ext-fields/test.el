;;; Extension fields

(ert-deftest test-ext-abspath ()
  "Test the `ext-abspath' extension field."
  ;; When the TAG_PROC_CWD ptag is not presented, Citre should take the
  ;; directory where the tags file exists as the base dir, if the first
  ;; relative path in the tags file can be found by doing so.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file)
                   "hello" nil nil
                   :require '(ext-abspath)))))
    (should (equal (citre-get-tag-field 'ext-abspath tag)
                   (expand-test-file "src/input.lisp"))))
  ;; When the TAG_PROC_CWD ptag is presented, it's used as the base directory.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-with-cwd-ptag")
                   "hello" nil nil
                   :require '(ext-abspath)))))
    (should (equal (citre-get-tag-field 'ext-abspath tag)
                   "/project/root/src/input.lisp")))
  ;; If the tag uses absolute path, `ext-abspath' is the same as the input
  ;; field.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-absolute-path")
                   "hi" nil nil
                   :require '(ext-abspath)))))
    (should (equal (citre-get-tag-field 'ext-abspath tag)
                   "/some/other/path/input.lisp"))))

(ert-deftest test-ext-kind-full ()
  "Test the `ext-kind-full' extension field."
  ;; Guess the language based on file name, then guess the full-length kind.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-test-kind-field")
                   "hello-single-letter" nil nil
                   :require '(ext-kind-full)))))
    (should (equal (citre-get-tag-field 'ext-kind-full tag)
                   "function")))
  ;; Guess the full-length kind based on the language.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-with-language-field")
                   "hello" nil nil
                   :require '(ext-kind-full)))))
    (should (equal (citre-get-tag-field 'ext-kind-full tag)
                   "function")))
  ;; There's actually not a "q" kind in Lisp, so Citre can't guess the
  ;; full-length kind and returns the single-letter kind.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-test-kind-field")
                   "hello-nonstandard-kind" nil nil
                   :require '(ext-kind-full)))))
    (should (equal (citre-get-tag-field 'ext-kind-full tag)
                   "q")))
  ;; But if the TAG_KIND_DESCRIPTION tags are presented, Citre can guess it.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-with-kind-table")
                   "hello" nil nil
                   :require '(ext-kind-full)))))
    (should (equal (citre-get-tag-field 'ext-kind-full tag)
                   "custom")))
  ;; Return the kind field directly if it's full-length.
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file "tags-with-full-kind")
                   "hello" nil nil
                   :require '(ext-kind-full)))))
    (should (equal (citre-get-tag-field 'ext-kind-full tag)
                   "function"))))
