;;; common.el --- Common code to run before each test

(defconst default-tags "tags"
  "Default tags file under each test case directory.")

(setq citre-readtags-program (getenv "READTAGS"))
(setq citre-core--dont-prompt-for-cwd t)

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

(defun get-definitions (mode buffer-file
			     marker extra-move
			     &optional tags-file)
  "Call `citre-get-definitions' is the environment specified with the arguments.
MODE is a function for entering a major mode.
BUFFER-FILE is a file name that content fills the buffer.
MARKER, and EXTRA-MOVE are for adjusting the point in the buffer.
MARKER is a pattern string. This function searches MARKER
with `re-search-forward` from the beginning of the buffer.
EXTRA-MOVE is a function taking no argument. After searching
MARKER, EXTRA-MOVE is called.
After adjusting the point in this way, `xref-find-definitions' is
called."
  (with-temp-buffer
    (insert-file-contents buffer-file)
    (funcall mode)
    (goto-char (point-min))
    (re-search-forward marker)
    (when extra-move
      (funcall extra-move))
    (citre-get-definitions nil tags-file)))

(defun get-file-contet (file)
  "Get file content of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min) (point-max))))
