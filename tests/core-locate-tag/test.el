(ert-deftest test-locate-tag-by-searching ()
  "Test `citre-readtags-locate-tag'.
This is for situations where the search pattern is presented."
  (dolist (tags '("forward-pattern-tags"
                  "backward-pattern-tags"
                  "combined-backward-pattern-tags"
                  "backward-pattern-with-line-tags"))
    (let* ((records (citre-readtags-get-records
                     (expand-test-file tags)
                     "temp" 'exact nil
                     :require '(name pattern)
                     :optional '(line)))
           (record (car records)))
      (should (eq (length records) 1))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/add-line.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 4)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 4)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 3)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 3))))))

(ert-deftest test-locate-tag-by-linum ()
  "Test `citre-readtags-locate-tag'.
This is for situations where the search pattern is not provided."
  (dolist (tags '("number-pattern-tags"
                  "number-pattern-with-line-tags"))
    (let* ((records (citre-readtags-get-records
                     (expand-test-file tags)
                     "temp" 'exact nil
                     :require '(name pattern)
                     :optional '(line)))
           (record (car records)))
      (should (eq (length records) 1))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/add-line.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 2)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 2)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 2)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-readtags-locate-tag record 'use-linum) 2))))))
