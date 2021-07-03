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
  (mapcar (lambda (tag) (citre-core-get-field field tag)) tags))

(defun set-equal (a b)
  "Check if lists A and B are equal in the sense of sets."
  (let ((a (cl-remove-duplicates a :test #'equal))
        (b (cl-remove-duplicates b :test #'equal)))
    (and (equal (length a) (length b))
         (equal (length (cl-intersection a b :test #'equal))
                (length a)))))

(defun make-tag (&rest data)
  "Make a tag from DATA.
DATA is a sequence of FIELD VALUE pairs."
  (let ((tag (make-hash-table :test #'eq))
        (i 0)
        (len (length data)))
    (while (< (1+ (* 2 i)) len)
      (let ((field (nth (* 2 i) data))
            (val (nth (1+ (* 2 i)) data)))
        (puthash field val tag))
      (cl-incf i))
    tag))
