;;; common.el --- Common code to run before each test

(defconst default-tags "tags"
  "Default tags file under each test case directory.")

(setq citre-readtags-program (getenv "READTAGS"))
(setq citre-readtags--dont-prompt-for-cwd t)

(defun expand-test-file (&optional file)
  "Get the absolute path of FILE.
FILE is a relative path against the test case directory.  If it's
nil, the value of `default-tags' is used.

This is for use in the batch mode.  You should start Emacs with
the working directory being a test case directory to ensure this
work correctly."
  (let ((file (or file default-tags)))
    (expand-file-name (concat default-directory file))))

(defun map-get-field (field tags)
  "Map the \"get FIELD\" operation over a list TAGS."
  (mapcar (lambda (tag) (citre-get-tag-field field tag)) tags))

(defun set-equal (a b)
  "Check if lists A and B are equal in the sense of sets."
  (let ((a (cl-remove-duplicates a :test #'equal))
        (b (cl-remove-duplicates b :test #'equal)))
    (and (equal (length a) (length b))
         (equal (length (cl-intersection a b :test #'equal))
                (length a)))))

(defun get-definitions (mode buffer-file marker extra-move &optional tags-file)
  "Call `citre-get-definitions' in the environment specified with the arguments.
This inserts the content of BUFFER-FILE in a buffer, and calls
MODE, a function for entering a major mode.

Then, MARKER and EXTRA-MOVE are used to adjust the point in the
buffer.  MARKER is a pattern string.  EXTRA-MOVE is a function
taking no argument.  MARKER is searched with `re-search-forward'
from the beginning of the buffer, then EXTRA-MOVE is called.

Finally, `citre-get-definitions' is called, which returns the
definitions of the symbol at point.

If TAGS-FILE is non-nil, use that tags file."
  (with-temp-buffer
    (insert-file-contents buffer-file)
    (setq buffer-file-name (expand-file-name buffer-file))
    (funcall mode)
    (goto-char (point-min))
    (re-search-forward marker)
    (when extra-move (funcall extra-move))
    (prog1
        (citre-get-definitions nil tags-file)
      ;; Unset the buffer file name, so when running tests interactively, Emacs
      ;; won't ask if we want to kill the buffer when finishing the
      ;; `with-temp-buffer' form.
      (setq buffer-file-name nil))))

(defun get-file-content (file)
  "Get file content of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min) (point-max))))
