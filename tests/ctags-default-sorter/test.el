;; TODO: test other behaviors of default sorter

(ert-deftest test-sort-by-size ()
  "Test the \"sort-by-size\" behavior of default definition filter."
  (let ((tags (citre-core-get-tags
               (expand-test-file "definition-size.tags")
               "a" 'exact nil :require '(line))))
    (should (equal (map-get-field 'line tags)
                   '(2 3))))
  (let ((tags (citre-core-get-tags
               (expand-test-file "definition-size.tags")
               "a" 'exact nil :require '(line)
               :sorter citre-definition-default-sorter)))
    (should (equal (map-get-field 'line tags)
                   '(3 2)))))
