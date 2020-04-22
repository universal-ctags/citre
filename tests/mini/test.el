(ert-deftest test--get-lines ()
  "Testing `citre--get-lines'."
  (should (equal
	   (with-me
	    (citre-mode 1)
	    (citre--get-lines "z" 'exact default-tags default-directory))
	   '("z	src/input.h	/^  float z;$/;\"	kind:member	line:18"
	     "z	src/input.h	/^  int x, y, z;$/;\"	kind:member	line:9"))))

(ert-deftest test-get-records ()
  "Testing `citre--get-records'."
  (should (equal
	   (with-me
	    (setq citre-tags-files (cons default-tags citre-tags-files))
	    (citre-mode 1)
	    (citre-get-records "z" 'exact default-directory))
	   `(("z" "member" nil ,(expand-test-file "src/input.h") 18)
	     ("z" "member" nil ,(expand-test-file "src/input.h") 9)))))
