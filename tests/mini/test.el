(ert-deftest test--get-lines ()
  "Testing `citre-readtags--get-lines'."
  (should (equal
           (citre-readtags--get-lines default-tags "z" 'exact)
           '("z	src/input.h	/^  float z;$/;\"	kind:member	line:18"
             "z	src/input.h	/^  int x, y, z;$/;\"	kind:member	line:9"))))

(ert-deftest test-get-records ()
  "Testing `citre-get-records'."
  (mapc (lambda (record)
	  (should (equal (citre-readtags-get-field 'name record) "z"))
	  (should (equal (citre-readtags-get-field 'kind record) "member"))
	  (let ((line (citre-readtags-get-field 'line record)))
	    (should (memq line '(9 18)))
	    (cond
	     ((eq line 18)
	      (should (equal (citre-readtags-get-field 'pattern record) "/^  float z;$/;\""))
	      )
	     ((eq line 9))
	     (should (equal (citre-readtags-get-field 'pattern record) "/^  int x, y, z;$/;\""))
	     )))
	(citre-get-records default-tags "z" 'exact :require '(name kind line pattern))))
