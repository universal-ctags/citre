;;
;; common codes for testing should be put here.
;;
(defconst default-tags "target.tags")
(defconst default-test "test.el")
(defmacro with-me (&rest body)
  `(with-current-buffer (find-file-noselect default-test)
     ,@body))

(defun expand-test-file (f)
  (expand-file-name (concat default-directory f)))
