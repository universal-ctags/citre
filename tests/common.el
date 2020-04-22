;;
;; common codes for testing should be put here.
;;
(defconst default-tags "target.tags"
  "Default tags file under each test case directory.")

(defconst default-test "test.el"
  "Default test to run under each test case directory.")

(defmacro with-me (&rest body)
  "Eval BODY in the buffer of `default-test' file.
This is for use in the batch mode.  You should start Emacs with
the working directory being a test case directory to ensure this
work correctly."
  `(with-current-buffer (find-file-noselect default-test)
     ,@body))

(defun expand-test-file (file)
  "Get the absolute path of FILE.
FILE is a relative path against the test case directory.

This is for use in the batch mode.  You should start Emacs with
the working directory being a test case directory to ensure this
work correctly."
  (expand-file-name (concat default-directory file)))
