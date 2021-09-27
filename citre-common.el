;;; citre-common.el --- Common functions and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2021
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.1.3
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions and utilities for Citre.

;;; Code:

;;;; Libraries

(require 'subr-x)

;;;; String

(defun citre-string-after-1st-colon (string)
  "Return the part in STRING after the first colon in it.
If STRING doesn't contain a colon, return it directly."
  (if-let ((sep (string-match ":" string)))
      (substring string (1+ sep))
    string))

(defun citre-string-match-all-escaping-backslash
    (string &optional start)
  "Find all occurence of escaping backslashes in STRING.
If START is non-nil, start search at that index in STRING.
Return a list of indexes of them.

This assumes the only escape sequence containing a second
backslash is \"\\\\\"."
  (let ((result nil)
        (start (or start 0))
        (idx nil))
    (while (setq idx (string-match "\\\\" string start))
      (push idx result)
      ;; Jump over the char after the backslash to search for next escaping
      ;; sequence.  NOTE: This may cause an "args out of range" error, but only
      ;; on string containing invalid trailing backslashes.  We don't check it
      ;; for performance.
      (setq start (+ idx 2)))
    (nreverse result)))

(defun citre-upcase-first-letter (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

;;;; File

;; NOTE: On Windows, ctags uses slash as the default directory separator, and
;; it can be handled by Emacs, so for now we don't care about backslash.
(defun citre-file-name-extension (file)
  "Return the extension of FILE.
If it doesn't have an extension, return the file name without
directory.

This is faster than `file-name-extension'."
  (or (string-match "\\.\\([^./]+\\)$" file)
      (string-match "/\\([^/]+\\)$" file))
  (match-string 1 file))

(defun citre-non-dir-file-exists-p (file)
  "Return t if FILE exists and is not a directory."
  (and (file-exists-p file)
       (not (file-directory-p file))))

(defun citre-dir-exists-p (dir)
  "Return t if DIR exists and is a directory."
  (and (file-exists-p dir)
       (file-directory-p dir)))

(defun citre-current-dir ()
  "Full current directory of the buffer.
This means the directory of the buffer file, or expanded
`default-directory' if it's not a file buffer."
  (expand-file-name
   (if-let (file (buffer-file-name))
       (file-name-directory file)
     default-directory)))

;;;; Text property

(defun citre-get-property (field str)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `citre-put-property'.

What it actually does is prefix FIELD by `citre-', and get that
text property."
  (get-text-property 0 (intern (concat "citre-" (symbol-name field))) str))

(defun citre-put-property (str &rest properties)
  "Set the text property of STR.
STR is the string to be modified.  PROPERTIES form a sequence of
PROPERTY VALUE pairs for test properties to add.  Each PROPERTY
is prefixed by \"citre-\".  Propertized STR is returned."
  (let ((i 0)
        (len (length properties)))
    (while (< (1+ (* 2 i)) len)
      (let ((prop (nth (* 2 i) properties))
            (val (nth (1+ (* 2 i)) properties)))
        (put-text-property 0 (length str)
                           (intern (concat "citre-" (symbol-name prop)))
                           val str))
      (cl-incf i)))
  str)

(provide 'citre-common)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-common.el ends here
