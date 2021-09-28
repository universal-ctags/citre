;;; Getter & Setter

(ert-deftest test-tag-core-apis ()
  "Test the constructor, getter and setter of tag."
  (let ((tag (citre-make-tag 'field1 "value1" 'field2 "value2")))
    (should (equal (citre-get-tag-field-primitive 'field1 tag)
                   "value1"))
    (should (equal (citre-get-tag-field-primitive 'field2 tag)
                   "value2"))
    (citre-set-tag-field 'field3 "value3" tag)
    (should (equal (citre-get-tag-field-primitive 'field3 tag)
                   "value3"))))

;;; Pattern field processing

(ert-deftest test-create-pattern ()
  "Test `citre-create-tag-search-pattern'."
  (should (equal (citre-create-tag-search-pattern
                  "pat")
                 "/pat/;\""))
  (should (equal (citre-create-tag-search-pattern
                  "pat" 'from-beg)
                 "/^pat/;\""))
  (should (equal (citre-create-tag-search-pattern
                  "pat" nil 'to-end)
                 "/pat$/;\""))
  (should (equal (citre-create-tag-search-pattern
                  "pat" 'from-beg 'to-end)
                 "/^pat$/;\""))
  (should (equal (citre-create-tag-search-pattern
                  "$pat$" 'from-beg 'to-end)
                 "/^$pat\\$$/;\""))
  (should (equal (citre-create-tag-search-pattern
                  "p/a\\t")
                 "/p\\/a\\\\t/;\"")))

(ert-deftest test-split-pattern ()
  "Test `citre-split-tag-pattern'."
  ;; number pattern
  (should (equal (citre-split-tag-pattern "15;\"")
                 '(15 nil)))
  ;; forward search pattern
  (should (equal (citre-split-tag-pattern "/pat\\//;\"")
                 '(nil "/pat\\//")))
  ;; backward search pattern
  (should (equal (citre-split-tag-pattern "?pat\\??;\"")
                 '(nil "?pat\\??")))
  ;; combined forward search pattern
  (should (equal (citre-split-tag-pattern "15;/pat\\//;\"")
                 '(15 "/pat\\//")))
  ;; combined backward search pattern
  (should (equal (citre-split-tag-pattern "15;?pat\\??;\"")
                 '(15 "?pat\\??")))
  (should (equal (condition-case e
                     (citre-split-tag-pattern "pat") (error e))
                 '(error "Invalid PATTERN"))))

(ert-deftest test-parse-pattern ()
  "Test `citre-parse-search-pattern'."
  (should (equal (citre-parse-search-pattern "/^pat\\\\$\\/\\$$/")
                 '("pat\\$/$" t 10)))
  (should (equal (citre-parse-search-pattern "?pat\\\\$\\?\\$?")
                 '("pat\\$?$" nil nil)))
  (should (equal (condition-case e
                     (citre-parse-search-pattern "/\\?/") (error e))
                 '(error "Invalid escape sequence"))))

;;; Extended getter

(ert-deftest test-get-field-after-colon ()
  "Test `citre-get-tag-field' with AFTER-COLON being non-nil."
  (let ((tag (citre-make-tag 'typeref "struct:structName")))
    (should (equal (citre-get-tag-field 'typeref tag 'after-colon)
                   "structName"))))

(ert-deftest test-extra-line ()
  "Test the `extra-line' extra extension field."
  (let ((tag (citre-make-tag 'line "10")))
    (should (eq (citre-get-tag-field 'extra-line tag)
                10)))
  (let ((tag (citre-make-tag 'pattern "10;/pat/;\"")))
    (should (eq (citre-get-tag-field 'extra-line tag)
                10)))
  (let ((tag (citre-make-tag 'pattern "/pat/;\"")))
    (should (eq (citre-get-tag-field 'extra-line tag)
                nil))))

(ert-deftest test-extra-lang ()
  "Test the `extra-lang' extra extension field."
  (let ((tag (citre-make-tag 'language "EmacsLisp")))
    (should (equal (citre-get-tag-field 'extra-lang tag)
                   "EmacsLisp")))
  (let ((tag (citre-make-tag 'input "input.c")))
    (should (equal (citre-get-tag-field 'extra-lang tag)
                   "C")))
  (let ((tag (citre-make-tag 'input "input.some-extension")))
    (should (equal (citre-get-tag-field 'extra-lang tag)
                   "some-extension"))))

(ert-deftest test-extra-matched-str ()
  "Test the `extra-matched-str' extra extension field."
  (let ((tag (citre-make-tag 'pattern "10;/pat/;\"")))
    (should (equal (citre-get-tag-field 'extra-matched-str tag)
                   "pat")))
  (let ((tag (citre-make-tag 'pattern "10;\"")))
    (should (equal (citre-get-tag-field 'extra-matched-str tag)
                   nil))))

;;; Show a tag

(ert-deftest test-make-tag-str ()
  "Basic test of `citre-make-tag-str'."
  (let* ((tag (citre-make-tag
               'name "name"
               'ext-kind-full "function"))
         (str (citre-make-tag-str tag nil '(name) '(annotation))))
    (should (equal str "name function"))
    (should (eq (get-text-property 0 'face str)
                'font-lock-function-name-face))
    (should (eq (get-text-property 5 'face str)
                'citre-tag-annotation-face))))

(ert-deftest test-make-tag-str-annotation ()
  "Test annotation component."
  (let ((tag (citre-make-tag 'ext-kind-full "variable"
                             'typeref "struct:structName"
                             'scope "class:className")))
    (should (equal (citre-make-tag-str tag nil '(annotation))
                   "variable/struct:structName@class:className"))
    (should (equal (citre-make-tag-str tag nil '(annotation :no-kind t))
                   "struct:structName@class:className"))
    (should (equal (citre-make-tag-str tag nil '(annotation :no-type t))
                   "variable@class:className"))
    (should (equal (citre-make-tag-str tag nil '(annotation :no-scope t))
                   "variable/struct:structName")))
  (let ((tag (citre-make-tag 'ext-kind-full "variable"
                             ;; "typename" here is a placeholder.
                             'typeref "typename:typeName"
                             'scope "class:className")))
    (should (equal (citre-make-tag-str tag nil '(annotation))
                   "variable/typeName@class:className")))
  (let ((tag (citre-make-tag 'ext-kind-full "variable"
                             'extras "reference,fileScope")))
    (should (equal (citre-make-tag-str tag nil '(annotation))
                   "variable<R>"))
    (should (equal (citre-make-tag-str tag nil '(annotation :no-reference t))
                   "variable"))
    (should (equal (citre-make-tag-str tag nil
                                       '(annotation :reference-first t))
                   "<R>variable"))))

(ert-deftest test-make-tag-str-location ()
  "Test location component."
  (let* ((file (expand-test-file "test.el"))
         (tag (citre-make-tag 'ext-abspath file
                              'line "10")))
    (should (equal (citre-make-tag-str tag nil '(location))
                   (concat file "(10)")))
    (should (equal (citre-make-tag-str tag nil '(location :no-path t))
                   "10"))
    (should (equal (citre-make-tag-str tag nil '(location :no-line t))
                   file))
    (should (equal (citre-make-tag-str tag nil
                                       `(location :no-line t
                                                  :root ,default-directory))
                   (citre-relative-path file default-directory))))
  (let* ((missing-file (expand-test-file "no-such-file.el"))
         (tag (citre-make-tag 'ext-abspath missing-file
                              'line "10")))
    (should (equal (citre-make-tag-str tag nil '(location))
                   (concat "!" missing-file "(10)")))))

(ert-deftest test-make-tag-str-content ()
  "Test content component."
  (let* ((file (expand-test-file "test.el"))
         (tag (citre-make-tag
               'pattern (citre-create-tag-search-pattern "line-content"))))
    (should (equal (citre-make-tag-str tag nil '(content)) "line-content")))
  (let* ((file (expand-test-file "file"))
         (tag (citre-make-tag 'ext-abspath file 'line "1")))
    (should (equal (citre-make-tag-str tag nil '(content :ensure t))
                   "This is a line."))))
