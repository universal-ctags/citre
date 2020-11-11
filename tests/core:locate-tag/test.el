(ert-deftest test-locate-tag ()
  "Test `citre-readtags-locate-tag'."
  (let* ((records (citre-readtags-get-records
                   (expand-test-file)
                   "pressure" 'exact nil
                   :require '(name pattern line)
                   :filter (citre-readtags-build-filter
                            'scope-name "test2" 'eq)))
         (record (car records)))
    (should (eq (length records) 1))
    (with-current-buffer (find-file-noselect
                          (expand-test-file "src/add-line.f90"))
      (should (eq (citre-readtags-locate-tag record 'use-linum) 7)))
    (with-current-buffer (find-file-noselect
                          (expand-test-file "src/change-line.f90"))
      (should (eq (citre-readtags-locate-tag record 'use-linum) 7)))
    (with-current-buffer (find-file-noselect
                          (expand-test-file "src/change-definition.f90"))
      (should (eq (citre-readtags-locate-tag record 'use-linum) 3)))))
