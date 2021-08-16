(defun defs-to-xref (defs)
  "Convert the value returned from `citre-get-definitions' to xref like string."
  (let (last-abspath)
    (with-temp-buffer
      (mapc (lambda (def)
	      (let ((abspath (gethash 'ext-abspath def)))
		(unless (equal abspath last-abspath)
		  (insert abspath)
		  (insert ?\n)
		  (setq last-abspath abspath)))
	      (insert (citre-make-tag-str def nil
					  '(location :no-path t :suffix ":")
					  '(annotation :prefix "(" :suffix ")")
					  '(content)))
	      (insert ?\n))
	    defs)
      (replace-regexp-in-string
       (concat "^" (regexp-quote (expand-file-name default-directory)))
       ""
       (buffer-substring-no-properties
	(point-min) (point-max))))))

(ert-deftest test-lang-c-header-sorting ()
  "Test `citre-lang-c--get-header-at-point' and `citre-lang-c-definition-sorter'"
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-header.h" "@"
				(lambda () (backward-word 2)) "target.tags"))
		 (get-file-contet "xref/buffer-header-a.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-header.h" "!"
				(lambda () (backward-word 2)) "target.tags"))
		 (get-file-contet "xref/buffer-header-b.xref")))
  )

(ert-deftest test-lang-c-struct-tag-sorting ()
  "Test the rule for (or \"struct\" \"union\" \"enum\") used in `citre-lang-c-definition-sorter'"
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-struct-tag.h" "@"
				(lambda () (forward-word 1) (forward-char 2)) "target.tags"))
		 (get-file-contet "xref/buffer-struct-tag.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-struct-tag.h" "!"
				(lambda () (backward-word 1)) "target.tags"))
		 (get-file-contet "xref/buffer-not-struct-tag.xref")))
  )

(ert-deftest test-lang-c-member-sorting ()
  "Test the rule for (or \"->\" \".\") used in `citre-lang-c-definition-sorter'"
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/member-sorting.c" "@arrow"
				(lambda () (backward-word 2)) "target.tags"))
		 (get-file-contet "xref/member-sorting/arrow.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/member-sorting.c" "@dotinit"
				(lambda () (backward-word 3)) "target.tags"))
		 (get-file-contet "xref/member-sorting/dotinit.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/member-sorting.c" "@dotassign"
				(lambda () (backward-word 3)) "target.tags"))
		 ;; The result should be the same as @arrow.
		 (get-file-contet "xref/member-sorting/arrow.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/member-sorting.c" "@nomember"
				(lambda () (backward-word 2)) "target.tags"))
		 (get-file-contet "xref/member-sorting/nomember.xref")))
  )

(ert-deftest test-lang-c-callable-sorting ()
  "Test the rule for `callable()' used in `citre-lang-c-definition-sorter'"
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/callable-sorting.c" "@member"
				(lambda () (backward-word 3)) "target.tags"))
		 (get-file-contet "xref/callable-sorting/member.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/callable-sorting.c" "@callable-member"
				(lambda () (backward-word 4)) "target.tags"))
		 (get-file-contet "xref/callable-sorting/callable-member.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/callable-sorting.c" "@callable-func"
				(lambda () (forward-word 1) (backward-word 1)) "target.tags"))
		 (get-file-contet "xref/callable-sorting/callable-func.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/callable-sorting.c" "@macro"
				(lambda () (forward-word 1) (backward-word 1)) "target.tags"))
		 (get-file-contet "xref/callable-sorting/macro.xref")))
  )
