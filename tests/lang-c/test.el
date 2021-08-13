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
  "Test `citre-lang-c--get-normal-symbol' and `citre-lang-c-definition-sorter'"
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-struct-tag.h" "@"
				(lambda () (forward-word 1) (forward-char 2)) "target.tags"))
		 (get-file-contet "xref/buffer-struct-tag.xref")))
  (should (equal (defs-to-xref (get-definitions
				'c-mode "buffer/buffer-struct-tag.h" "!"
				(lambda () (backward-word 1)) "target.tags"))
		 (get-file-contet "xref/buffer-not-struct-tag.xref")))
  )
