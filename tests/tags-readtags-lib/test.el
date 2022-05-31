;;; Basic

(ert-deftest test-main-apis-basic-test ()
  "Basic test for main APIs."
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "z" 'exact nil :require '(name input kind line))))
    (should (equal (map-get-field 'name tags)
                   '("z" "z")))
    (should (equal (map-get-field 'input tags)
                   '("src/input.h" "src/input.h")))
    (should (equal (map-get-field 'kind tags)
                   '("member" "member")))
    (should (equal (map-get-field 'line tags)
                   '(18 9)))
    (should (equal (map-get-field 'non-exist-field tags)
                   '(nil nil)))))

;;; `citre-readtags-get-tags'

(ert-deftest test-get-tags-no-match ()
  "Test `citre-readtags-get-tags' when no matching tags."
  (should (equal (citre-readtags-get-tags
                  (expand-test-file)
                  "non-exist-name" nil nil)
                 nil)))

(ert-deftest test-get-tags-match ()
  "Test MATCH argument in `citre-readtags-get-tags'."
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "parent" nil nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("parent"))))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "fpoint" 'prefix nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("fpoint2d" "fpoint3d"))))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "2d" 'suffix nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("fpoint2d" "ipoint2d"))))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "point" 'substr nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("fpoint2d" "fpoint3d" "ipoint2d" "ipoint3d"))))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "(^fpoint)\|(^area)" 'regexp nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("area" "area" "fpoint2d" "fpoint3d")))))

(ert-deftest test-get-tags-case-fold ()
  "Test CASE-FOLD argument in `citre-readtags-get-tags'."
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "PaReNt" nil nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   nil)))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "PaReNt" nil t :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("parent"))))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "^PaReNt$" 'regexp nil :require '(name))))
    (should (equal (map-get-field 'name tags)
                   nil)))
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "^PaReNt$" 'regexp t :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("parent")))))

(ert-deftest test-get-tags-filter ()
  "Test FILTER argument in `citre-readtags-get-tags'."
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               nil nil nil
               :filter (citre-readtags-filter 'kind "struct" 'eq)
               :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("fpoint2d" "fpoint3d" "ipoint2d" "ipoint3d"))))
  (should (equal (citre-readtags-get-tags
                  (expand-test-file)
                  nil nil nil
                  :filter (citre-readtags-filter 'kind "non-exist-kind" 'eq)
                  :require '(name))
                 nil)))

(ert-deftest test-get-tags-sorter ()
  "Test SORTER argument in `citre-readtags-get-tags'."
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               nil nil nil
               :filter (citre-readtags-filter 'kind "struct" 'eq)
               :sorter (citre-readtags-sorter 'line)
               :require '(name))))
    (should (equal (map-get-field 'name tags)
                   '("ipoint2d" "ipoint3d" "fpoint2d" "fpoint3d")))))

(ert-deftest test-get-tags-custom-fields ()
  "Test REQUIRE, OPTIONAL and EXCLUDE argument in `citre-readtags-get-tags'."
  ;; REQUIRE
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               "fpoint" 'prefix nil
               :require '(name input))))
    (should (cl-every #'identity (map-get-field 'name tags)))
    (should (cl-every #'identity (map-get-field 'input tags)))
    (should (cl-every #'not (map-get-field 'kind tags))))
  ;; REQUIRE non-existing fields
  (should (equal (cadr (should-error
                        (citre-readtags-get-tags
                         (expand-test-file)
                         nil nil nil
                         :require '(signature name))))
                 "Fields not found in tags file: signature"))
  ;; OPTIONAL
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               nil nil nil
               :require '(name)
               :optional '(signature))))
    (should (cl-every #'identity (map-get-field 'name tags)))
    (should (cl-some #'identity (map-get-field 'signature tags)))
    (should (cl-some #'not (map-get-field 'signature tags))))
  ;; PARSE-ALL-FIELDS
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               nil nil nil
               :parse-all-fields t)))
    (should (cl-every #'identity (map-get-field 'name tags)))
    (should (cl-every #'identity (map-get-field 'input tags)))
    (should (cl-every #'identity (map-get-field 'kind tags)))
    (should (cl-some #'identity (map-get-field 'signature tags)))
    (should (cl-some #'not (map-get-field 'signature tags))))
  ;; PARSE-ALL-FIELDS + requiring non-existing fields
  (should (equal (cadr (should-error
                        (citre-readtags-get-tags
                         (expand-test-file)
                         nil nil nil
                         :require '(signature)
                         :parse-all-fields t)))
                 "Fields not found in tags file: signature"))
  ;; PARSE-ALL-FIELDS + exclude
  (let ((tags (citre-readtags-get-tags
               (expand-test-file)
               nil nil nil
               :require '(name input)
               :exclude '(kind signature)
               :parse-all-fields t)))
    (should (cl-every #'identity (map-get-field 'name tags)))
    (should (cl-every #'identity (map-get-field 'input tags)))
    (should (cl-every #'identity (map-get-field 'line tags)))
    (should (cl-every #'not (map-get-field 'kind tags)))
    (should (cl-every #'not (map-get-field 'signature tags)))))

;;; `citre-readtags-get-pseudo-tags'

(ert-deftest test-get-pseudo-tags ()
  "Test `citre-readtags-get-pseudo-tags'."
  (let ((tag (car (citre-readtags-get-pseudo-tags
                   "TAG_PROGRAM_NAME" (expand-test-file)))))
    (should (equal (car tag) "!_TAG_PROGRAM_NAME"))
    (should (equal (cadr tag) "Universal Ctags")))
  (let ((tags (citre-readtags-get-pseudo-tags
               "TAG_PROGRAM" (expand-test-file)
               t)))
    (should (set-equal (mapcar #'car tags)
                       '("!_TAG_PROGRAM_AUTHOR"
                         "!_TAG_PROGRAM_NAME"
                         "!_TAG_PROGRAM_URL")))))

;;; `citre-get-tag-field' on ctags backend

(ert-deftest test-get-field ()
  "Test `citre-get-tag-field' on ctags backend."
  (let ((tag (car (citre-readtags-get-tags
                   (expand-test-file)
                   "fpoint2d" nil nil
                   :require '(name input line)
                   :optional '(end)))))
    (should (equal (citre-get-tag-field 'name tag) "fpoint2d"))
    (should (equal (citre-get-tag-field 'input tag) "src/input.h"))
    (should (equal (type-of (citre-get-tag-field 'line tag)) 'integer))
    (should (equal (type-of (citre-get-tag-field 'end tag)) 'integer)))
  (let ((tag (car (citre-readtags-get-tags (expand-test-file) "parent"
                                           nil nil :require '(typeref)))))
    (should (equal (citre-get-tag-field 'typeref tag) "typename:fpoint2d"))
    (should (equal (citre-get-tag-field 'typeref tag t) "fpoint2d"))))
