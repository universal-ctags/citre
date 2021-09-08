;; Filter builder

(ert-deftest test-filter-eq ()
  "Test filter builder for eq matches."
  (should (equal (citre-core-filter "hi" "hi" 'eq)
                 '(and (eq? "hi" "hi"))))
  (should (equal (citre-core-filter "HI" 'name 'eq t)
                 '(and $name (eq? "hi" (downcase $name))))))

(ert-deftest test-filter-prefix ()
  "Test filter builder for prefix matches."
  (should (equal (citre-core-filter 'name "hi" 'prefix)
                 '(and $name (prefix? $name "hi"))))
  (should (equal (citre-core-filter 'name "HI" 'prefix t)
                 '(and $name (prefix? (downcase $name) "hi")))))

(ert-deftest test-filter-suffix ()
  "Test filter builder for suffix matches."
  (should (equal (citre-core-filter 'input "py" 'suffix)
                 '(and $input (suffix? $input "py"))))
  (should (equal (citre-core-filter 'input 'language 'suffix t)
                 '(and $language $input (suffix? (downcase $input)
                                                 (downcase $language))))))

(ert-deftest test-filter-substr ()
  "Test filter builder for substr matches."
  (should (equal (citre-core-filter 'input "module" 'substr)
                 '(and $input (substr? $input "module"))))
  (should (equal (citre-core-filter 'input "MODULE" 'substr t)
                 '(and $input (substr? (downcase $input) "module")))))

(ert-deftest test-filter-regexp ()
  "Test filter builder for regexp matches."
  (should (equal (citre-core-filter 'name "^f.*point$" 'regexp)
                 '(and $name ((string->regexp "^f.*point$" :case-fold false)
                              $name))))
  (should (equal (citre-core-filter 'name "^f.*point$" 'regexp t)
                 '(and $name ((string->regexp "^f.*point$" :case-fold true)
                              $name)))))

(ert-deftest test-filter-csv-contain ()
  "Test filter builder for csv-contain matches."
  (should (equal (citre-core-filter 'extras "reference" 'csv-contain)
                 '(and $extras ((string->regexp "(^|,) ?(reference)(,|$)"
                                                :case-fold false)
                                $extras))))
  (should (equal (citre-core-filter 'extras "reference" 'csv-contain t)
                 '(and $extras ((string->regexp "(^|,) ?(reference)(,|$)"
                                                :case-fold true)
                                $extras)))))

(ert-deftest test-filter-invert&keep-missing ()
  "Test the INVERT and KEEP-MISSING arg in `citre-core-filter'."
  (should (equal (citre-core-filter 'name "hi" 'prefix nil t)
                 '(and $name (not (prefix? $name "hi")))))
  (should (equal (citre-core-filter 'name "hi" 'prefix nil nil t)
                 '(or (not $name) (prefix? $name "hi"))))
  (should (equal (citre-core-filter 'name "hi" 'prefix nil t t)
                 '(or (not $name) (not (prefix? $name "hi"))))))

(ert-deftest test-filter-field-exist ()
  "Test `citre-core-filter-field-exist'."
  (should (equal (citre-core-filter-field-exist 'pattern) '$pattern))
  (should (equal (citre-core-filter-field-exist 'pattern t) '(not $pattern))))

(ert-deftest test-filter-lang ()
  "Test `citre-core-filter-lang'."
  (should (equal (citre-core-filter-lang "Lisp")
                 '(or (and $language (eq? $language "Lisp"))
                      (and $input ((string->regexp "\\.(lsp|lisp|l|clisp|cl)$"
                                                   :case-fold false)
                                   $input))))))

(ert-deftest test-filter-kind ()
  "Test `citre-core-filter-kind'."
  (should (equal (citre-core-filter-kind "function")
                 '(and $kind ((string->regexp "^(function|f)$"
                                              :case-fold false)
                              $kind))))
  (should (equal (citre-core-filter-kind "function" t)
                 '(or (not $kind) ((string->regexp "^(function|f)$"
                                                   :case-fold false)
                                   $kind)))))

(ert-deftest test-filter-input ()
  "Test `citre-core-filter-input'."
  (should (equal (citre-core-filter-input
                  "/path/to/test.el" (expand-test-file))
                 '(or (and $input (eq? $input "/path/to/test.el"))
                      (and $input (eq? $input "test.el"))
                      (and $input ((string->regexp "(^|/)..?/test\\.el$"
                                                   :case-fold false)
                                   $input)))))
  (should (equal (citre-core-filter-input
                  "/path/not/in/cwd/to/test.el" (expand-test-file))
                 '(or (and $input (eq? $input "/path/not/in/cwd/to/test.el"))
                      (and $input ((string->regexp "(^|/)..?/test\\.el$"
                                                   :case-fold false)
                                   $input))))))

;; Sorter builder

(ert-deftest test-sorter ()
  "Test `citre-core-sorter'."
  (should (equal (citre-core-sorter 'input)
                 '(<or> (if (and $input &input) (<> $input &input) 0))))
  (should (equal (citre-core-sorter '(field input +))
                 '(<or> (if (and $input &input) (<> $input &input) 0))))
  (should (equal (citre-core-sorter '(field input -))
                 '(<or> (if (and $input &input) (<> &input $input) 0))))
  (should (equal (citre-core-sorter '(length name +))
                 '(<or> (if (and $name &name)
                            (<> (length $name) (length &name))
                          0))))
  (should (equal (citre-core-sorter '(length name -))
                 '(<or> (if (and $name &name)
                            (<> (length &name) (length $name))
                          0))))
  (should (equal (citre-core-sorter
                  `(filter ,(citre-core-filter 'name "hi" 'eq) +))
                 '(<or> (<> (if (and $name (eq\? $name "hi")) -1 1)
                            (if (and &name (eq\? &name "hi")) -1 1)))))
  (should (equal (citre-core-sorter
                  `(filter ,(citre-core-filter 'name "hi" 'eq) -))
                 '(<or> (<> (if (and $name (eq\? $name "hi")) 1 -1)
                            (if (and &name (eq\? &name "hi")) 1 -1)))))
  (should (equal (citre-core-sorter 'input '(length name +))
                 '(<or> (if (and $input &input) (<> $input &input) 0)
                        (if (and $name &name)
                            (<> (length $name) (length &name))
                          0)))))
