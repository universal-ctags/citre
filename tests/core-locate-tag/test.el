(ert-deftest test-locate-tag-by-searching ()
  "Test `citre-core-locate-tag'.
This is for situations where the search pattern is presented."
  (dolist (tagfile '("forward-pattern-tags"
                     "backward-pattern-tags"
                     "combined-backward-pattern-tags"
                     "backward-pattern-with-line-tags"))
    (let* ((tags (citre-core-get-tags
                  (expand-test-file tagfile)
                  "temp" 'exact nil
                  :require '(name pattern)
                  :optional '(line)))
           (tag (car tags)))
      (should (eq (length tags) 1))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/add-line.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 5))
        (should (equal (match-string 0)
                       "  real :: pressure(10,10), temp(")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 5))
        (should (equal (match-string 0)
                       "    real :: pressure(10,10), temp(")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 4))
        (should (equal (match-string 0)
                       "  real :: pressure(10,10), temp")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 4))
        (should (equal (match-string 0)
                       "TEMP")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/remove-all.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum)
                    (if (member tagfile '("forward-pattern-tags"
                                          "backward-pattern-tags"))
                        1 2)))))))

(ert-deftest test-locate-tag-by-linum ()
  "Test `citre-core-locate-tag'.
This is for situations where the search pattern is not provided."
  (dolist (tags '("number-pattern-tags"
                  "number-pattern-with-line-tags"))
    (let* ((tags (citre-core-get-tags
                  (expand-test-file tags)
                  "temp" 'exact nil
                  :require '(name pattern)
                  :optional '(line)))
           (tag (car tags)))
      (should (eq (length tags) 1))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/add-line.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 3)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 3)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 3)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 3)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/remove-all.f90"))
        (should (eq (citre-core-locate-tag tag 'use-linum) 2))))))
