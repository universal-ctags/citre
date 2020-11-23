(ert-deftest test-split-pattern ()
  "Test `citre-core--split-pattern'."
  ;; number pattern
  (should (equal (citre-core--split-pattern "15;\"")
                 '(15 nil)))
  ;; forward search pattern
  (should (equal (citre-core--split-pattern "/pat\\//;\"")
                 '(nil "/pat\\//")))
  ;; backward search pattern
  (should (equal (citre-core--split-pattern "?pat\\??;\"")
                 '(nil "?pat\\??")))
  ;; combined forward search pattern
  (should (equal (citre-core--split-pattern "15;/pat\\//;\"")
                 '(15 "/pat\\//")))
  ;; combined backward search pattern
  (should (equal (citre-core--split-pattern "15;?pat\\??;\"")
                 '(15 "?pat\\??")))
  (should (equal (condition-case e
                     (citre-core--split-pattern "pat") (error e))
                 '(error "Invalid PATTERN"))))

(ert-deftest test-parse-pattern ()
  "Test `citre-core--parse-search-pattern'."
  (should (equal (citre-core--parse-search-pattern "/^pat\\\\$\\/\\$$/")
                 '("pat\\$/$" t 10)))
  (should (equal (citre-core--parse-search-pattern "?pat\\\\$\\?\\$?")
                 '("pat\\$?$" nil nil)))
  (should (equal (condition-case e
                     (citre-core--parse-search-pattern "/\\?/") (error e))
                 '(error "Invalid escape sequence"))))
