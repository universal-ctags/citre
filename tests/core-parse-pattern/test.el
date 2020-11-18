(ert-deftest test-split-pattern ()
  "Test `citre-readtags--split-pattern'."
  ;; number pattern
  (should (equal (citre-readtags--split-pattern "15;\"")
                 '(15 nil)))
  ;; forward search pattern
  (should (equal (citre-readtags--split-pattern "/pat\\//;\"")
                 '(nil "/pat\\//")))
  ;; backward search pattern
  (should (equal (citre-readtags--split-pattern "?pat\\??;\"")
                 '(nil "?pat\\??")))
  ;; combined forward search pattern
  (should (equal (citre-readtags--split-pattern "15;/pat\\//;\"")
                 '(15 "/pat\\//")))
  ;; combined backward search pattern
  (should (equal (citre-readtags--split-pattern "15;?pat\\??;\"")
                 '(15 "?pat\\??")))
  (should (equal (condition-case e
                     (citre-readtags--split-pattern "pat") (t e))
                 '(error "Invalid PATTERN"))))

(ert-deftest test-parse-pattern ()
  "Test `citre-readtags--parse-search-pattern'."
  (should (equal (citre-readtags--parse-search-pattern "/^pat\\\\$\\/\\$$/")
                 '("pat\\$/$" t 10)))
  (should (equal (citre-readtags--parse-search-pattern "?pat\\\\$\\?\\$?")
                 '("pat\\$?$" nil nil)))
  (should (equal (condition-case e
                     (citre-readtags--parse-search-pattern "/\\?/") (t e))
                 '(error "Invalid escape sequence"))))
