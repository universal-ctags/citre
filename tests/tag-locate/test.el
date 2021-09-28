;; TODO: Get rid of tags files for these tests.

(ert-deftest test-locate-tag-by-pattern ()
  "Test `citre-locate-tag'.
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
        (should (eq (citre-locate-tag tag 'use-linum) 5))
        (should (equal (match-string 0)
                       "  real :: pressure(10,10), temp(")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 5))
        (should (equal (match-string 0)
                       "    real :: pressure(10,10), temp(")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 4))
        (should (equal (match-string 0)
                       "  real :: pressure(10,10), temp")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 4))
        (should (equal (match-string 0)
                       "TEMP")))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/remove-all.f90"))
        (should (eq (citre-locate-tag tag 'use-linum)
                    (if (member tagfile '("forward-pattern-tags"
                                          "backward-pattern-tags"))
                        1 2)))))))

(ert-deftest test-locate-tag-by-name ()
  "Test `citre-locate-tag'.
This is for situations where the search pattern is not provided,
so we locate the tag by line number and name."
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
        (should (eq (citre-locate-tag tag 'use-linum) 2)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-indentation.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 5)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-matched-str.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 4)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/change-definition.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 4)))
      (with-current-buffer (find-file-noselect
                            (expand-test-file "src/remove-all.f90"))
        (should (eq (citre-locate-tag tag 'use-linum) 2))))))

(ert-deftest test-goto-tag ()
  "Test `citre-goto-tag'."
  (let* ((tags (citre-core-get-tags
                (expand-test-file "forward-pattern-tags")
                "temp" 'exact nil
                :require '(name pattern ext-abspath)
                :optional '(line)))
         (tag (car tags))
         (file (expand-test-file "src/original.f90"))
         (window (selected-window))
         (test (lambda ()
                 (should (file-equal-p (buffer-file-name) file))
                 (should (eq (line-number-at-pos) 3)))))
    (should (eq (length tags) 1))
    ;; In current window
    (citre-goto-tag tag)
    (funcall test)
    ;; In other window
    (kill-buffer)
    (citre-goto-tag tag 'other-window)
    (should (not (eq (selected-window) window)))
    (funcall test)
    ;; In other window but don't select
    (kill-buffer-and-window)
    (setq window (selected-window))
    (citre-goto-tag tag 'other-window-noselect)
    (should (eq (selected-window) window))
    (switch-to-buffer (find-file-noselect file))
    (funcall test)))
