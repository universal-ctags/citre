;;; common.el --- Common code to run before each test

(defconst default-tags "target.tags"
  "Default tags file under each test case directory.")

(setq citre-readtags-program (getenv "READTAGS"))

(defun expand-test-file (&optional file)
  "Get the absolute path of FILE.
FILE is a relative path against the test case directory.  If it's
nil, the value of `default-tags' is used.

This is for use in the batch mode.  You should start Emacs with
the working directory being a test case directory to ensure this
work correctly."
  (let ((file (or file default-tags)))
    (expand-file-name (concat default-directory file))))
