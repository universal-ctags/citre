(defun defs-to-xref (defs)
  "Convert the value returned from `citre-get-definitions' to xref like string."
  (with-temp-buffer
    (mapc (lambda (def)
	    (insert (gethash 'ext-abspath def))
	    (insert ?\n)
	    (insert (gethash 'line def))
	    (insert ": ")
	    (insert (format "(%s)\n" (gethash 'ext-kind-full def))))
	  defs)
    (replace-regexp-in-string
     (concat "^" (regexp-quote (expand-file-name default-directory)))
     ""
     (buffer-substring-no-properties
      (point-min) (point-max)))))

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
