(ert-deftest test--get-lines ()
  "Testing `citre--get-lines'."
  (should (equal
           (citre--get-lines "z" 'exact default-tags)
           '("z	src/input.h	/^  float z;$/;\"	kind:member	line:18"
             "z	src/input.h	/^  int x, y, z;$/;\"	kind:member	line:9"))))

(ert-deftest test-get-records ()
  "Testing `citre--get-records'."
  (should (equal
           (citre-get-records "z" 'exact default-tags default-directory)
           `(("z" "member" nil ,(expand-test-file "src/input.h") 18)
             ("z" "member" nil ,(expand-test-file "src/input.h") 9)))))
