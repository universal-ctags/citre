;;; citre-readtags.el --- A readtags abstraction layer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 04 May 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/AmaiKinono/citre
;; Version: 0

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

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defcustom citre-readtags-program nil
  "The name or path of the readtags program.
Set this if readtags is not in your PATH, or its name is not
\"readtags\".

Citre requires the readtags program provided by Universal Ctags."
  :type 'string
  :group 'citre)

;;;; Basic Helpers

(defun citre-readtags--escape-single-quote (string)
  ;; TIP: Help mode renders single quotes in docstrings as curly single quotes,
  ;; and \\=' prevents that.  Eval this defun form and use `describe-function'
  ;; to read this docstring.
  "Disable the effect of single quotes as shell string terminators in STRING.
This function assumes the situation where STRING is to be passed
to `format' function like:

  (shell-command (format \"... \\='%s\\='\" STRING))

Assume the formatted \\='%s\\=' will be operated by the command
as a string.  An attacker can pass an arbitrary command to the
shell by putting single quotes in STRING like:

  (let ((string \"arg\\='; rm -rf /\\='\"))
    (shell-command (format \"... \\='%s\\='\" STRING)))

Then \\='%s\\=' is formatted as:

  \\='arg\\='; rm -rf /\\='\\='

Now the command operates on \\='arg\\=' as a string, and the
dangerous rm -rf / comes out of the string and be executed.

To mitigate such attack, this function replaces all \\=' in
STRING with \\='\"\\='\"\\=', which disables their meaning as
string terminator.  You can use this like:

  (let* ((string \"arg\\='; rm -rf /\\='\")
         (string (citre-readtags--escape-single-quote string))))
    (shell-command (format \"... \\='%s\\='\" STRING)))

Then \\='%s\\=' is formatted as:

  \\='arg\\='\"\\='\"'; rm -rf /\\='\"\\='\"\\='

Now the rm -rf / doesn't come out of the string.

This is for use in `citre-readtags--build-shell-command', where
its ARGS are exactly in this situation.  It makes sure that
unintentional attack doesn't happen when passing symbols/strings
with single quotes to it."
  (replace-regexp-in-string "'" "'\"'\"'" string))

(defun citre-readtags--build-shell-command (&rest args)
  "Build a shell command from ARGS.
Each element of ARGS could be a string, symbol or list.  For
strings, this formats them using \"%s\"; for symbols and lists,
this formats them using \"%S\". Then, each of them is wrapped in
single quotes, and concatenated with a space between each of
them.

Before wrapping in single quotes,
`citre-readtags--escape-single-quote' is applied to each of them
to prevent certain kinds of shell injection.  See its docstring
for details.

This function is not for building shell commands in general, but
only for Citre's own use, especially for building readtags
commands."
  (string-join
   (mapcar (lambda (elt)
             (format "'%s'"
                     (citre-readtags--escape-single-quote
                      (format (if (stringp elt) "%s" "%S") elt))))
           args)
   " "))

(defun citre-readtags--split-at-1st-colon (string)
  "Split STRING at the first colon in it.
A cons cell of the part before and after the colon is returned.
If STRING doesn't contain a colon, it will be (nil . STRING)."
  (let ((sep (string-match ":" string)))
    (if sep
        (cons (substring string 0 sep)
              (substring string (1+ sep)))
      (cons nil string))))

;;;; Internals: Additional information handling

;; TODO: Enhance the error handling here.  It's not easy, all the technique I
;; found to get the exit status before the pipe is not POSIX-compatible.
(defun citre-readtags--tags-file-use-relative-path-p (tagsfile)
  "Detect if file paths in tags file TAGSFILE are relative.
TAGSFILE is the path to the tags file.  This is done by
inspecting the first line of regular tags."
  (let* ((program (or citre-readtags-program "readtags"))
         (line (shell-command-to-string
                (concat
                 (citre-readtags--build-shell-command
                  program "-t" tagsfile "-l")
                 " | head -1"))))
    (cond
     ((string-empty-p line)
      (error "Invalid tags file"))
     ((not (string-match "\t" line))
      (error "Readtags: %s" (string-trim line)))
     (t
      (not (file-name-absolute-p (nth 1 (split-string line "\t" t))))))))

(defun citre-readtags--detect-tags-file-info (tagsfile kind)
  "Detect the value of info KIND of TAGSFILE.
TAGSFILE is the path to the tags file, KIND can be `path'."
  (pcase kind
    ('path (cons (citre-readtags--tags-file-use-relative-path-p tagsfile)
                 (citre-readtags-get-pseudo-tag "TAG_PROC_CWD" tagsfile)))))

(defvar citre-readtags--tags-file-info-alist nil
  "Alist for storing informations about tags file.
Since some informations offered by tags files may be ambiguous,
we use this variable to store additional informations to
ascertain them.

This alist looks like:

  (alist of:
   tags file -> list of \"kinds of info\":
                ((time field . value field)
                 ...))

The time fields are the last update time of its corresponding
kind of info, in the style of (current-time).  The value fields
in order are:

- path: A cons pair.  Its car is t when relative paths are used
  in the tags file, and cdr is the current working directory when
  generating the tags file.")

(defmacro citre-readtags--tags-file-info (info kind &optional field)
  "Return the place form of KIND in INFO.
INFO is a valid value in `citre-readtags--tags-file-info-alist',
For possible values of KIND, see
`citre-readtags--get-tags-file-info'.  FIELD could be `time' or
`value' to return the time field or value field in KIND.  When
nil, the whole kind of info is returned."
  (let* ((n `(pcase ,kind
               ('path 0)
               (_ (error "Invalid KIND"))))
         (form `(nth ,n ,info)))
    `(pcase ,field
       ('time (car ,form))
       ('value (cdr ,form))
       ('nil ,form)
       (_ (error "Invalid FIELD")))))

;; TODO: In many situations, we require the file path is not only absolute
;; (i.e., `file-name-absolute-p' returns t), but also "canonical" (i.e., AND it
;; doesn't start with "~"). We should make this definition clear in the
;; documentations for developers, and make the requirement clear in all the
;; docstrings.
(defun citre-readtags--get-tags-file-info (tagsfile &rest kinds)
  "Return the additional info of tags file TAGSFILE.
TAGSFILE is the canonical path to the tags file, and the return
value is the additional info of it.  This return value is a valid
value in `citre--tags-file-info-alist', see its docstring for
details.

KINDS is a list of symbols representing the additional kinds of
info needed by the caller.  Presented ones are updated when:

- The tags file has not been registered in
  `citre--tags-file-info-alist'.
- The tags file has been updated since this kind of info is asked
  last time.

The possible values of KINDS are:

- `path': Whether the tags file uses relative path, and the
  current working directory when generating the tags file.

When KINDS is nil, all kinds of info are updated."
  (unless (file-exists-p tagsfile)
    (error "%s doesn't exist" tagsfile))
  (let ((recent-modification (file-attribute-modification-time
                              (file-attributes tagsfile)))
        (kinds (or kinds '(path))))
    (cl-symbol-macrolet ((info (alist-get tagsfile
                                          citre-readtags--tags-file-info-alist
                                          nil nil #'equal)))
      ;; NOTE: hard coded things here
      (unless (eq (length info) 1)
        (setf info '((nil . nil))))
      (dolist (kind kinds)
        (unless (equal recent-modification
                       (citre-readtags--tags-file-info info kind 'time))
          (setf (citre-readtags--tags-file-info info kind 'time)
                recent-modification)
          (setf (citre-readtags--tags-file-info info kind 'value)
                (citre-readtags--detect-tags-file-info tagsfile kind))))
      info)))

;;;; Internals: Tags file filtering & parsing

(defvar citre-readtags--extension-fields-alist
  '((abspath . ((input)
                (path))))
  "Alist of extension fields and their dependencies.
Its keys are extension fields offered by Citre, values are lists
of two elements:

- A list of (normal) fields the the extension field depends on.
- A list of kinds of additional info of tags file that the
  extension field depends on.")

(defun citre-readtags--get-lines
    (tagsfile &optional name match case-sensitive filter-sexp)
  "Get lines in tags file TAGSFILE using readtags.
See `citre-readtags-get-records' to know about NAME, MATCH,
CASE-SENSITIVE and FILTER-SEXP."
  (let* ((parts nil)
         (match (or match 'exact))
         (extras (concat
                  "-ne"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-sensitive "" "i"))))
    ;; Program name
    (push (or citre-readtags-program "readtags") parts)
    ;; Read from this tags file
    (push "-t" parts)
    (push tagsfile parts)
    ;; Filter expression
    (when filter-sexp
      (push "-Q" parts)
      (push filter-sexp parts))
    ;; Extra arguments
    (push extras parts)
    ;; Action
    (if (or (null name)
            (string-empty-p name))
        (push "-l" parts)
      (push "-" parts)
      (push name parts))
    (let* ((result (split-string
                    (shell-command-to-string
                     (concat
                      (apply #'citre-readtags--build-shell-command
                             (nreverse parts))
                      ;; In case the output of readtags is not terminated by
                      ;; newline, we add an extra one.
                      "; printf \"\n$?\n\""))
                    "\n" t))
           (status (car (last result)))
           (output (cl-subseq result 0 -1)))
      (if (string= status "0")
          output
        (error "Readtags: %s" (string-join output "\n"))))))

(defun citre-readtags--parse-field (field nth)
  "Parse FIELD from a line in readtags output.
FIELD is a string from the line, separated with other fields by
tabs.  NTH is the position of the field in the line, counts from
zero.

The return value is a list of cons pairs, the cars of which are
field names, cdrs are the values."
  (pcase nth
    (0 `((name . ,field)))
    (1 `((input . ,field)))
    (2 `((pattern . ,field)))
    (3 `((kind . ,(cdr (citre-readtags--split-at-1st-colon field)))))
    (_
     (let ((parts (citre-readtags--split-at-1st-colon field)))
       (pcase (car parts)
         ("line"
          `((line . ,(string-to-number (cdr parts)))))
         ("end"
          `((end . ,(string-to-number (cdr parts)))))
         ("scope"
          (let ((value (citre-readtags--split-at-1st-colon (cdr parts))))
            `((scope-kind . ,(car value))
              (scope-name . ,(cdr value)))))
         ((or "class" "struct")
          `((scope-kind . ,(car parts))
            (scope-name . ,(cdr parts))))
         (_
          `((,(intern (car parts)) . ,(cdr parts)))))))))

(defun citre-readtags--get-ext-field
    (dep-record field tagsfile-info)
  "Calculate the value of extension field FIELD.
DEP-RECORD is a hash table containing the fields that FIELD
depends on, it's generated and passed by
`citre-readtags--parse-line'.  TAGSFILE-INFO is the additional
info of the tags file."
  (let ((no-cwd-error "Can't get absolute paths.  You can:\n\
1. Regenerate the tags file with \"TAG_PROC_CWD\" pseudo tag enabled, or\n\
2. Regenerate the tags file with path in the command")
        (no-input-field-error "\"input\" field not found in DEP-RECORD"))
    (pcase field
      ('abspath
       (let ((value (citre-readtags--tags-file-info tagsfile-info 'path 'value)))
         (cond
          ((null (car value))
           (or (gethash 'input dep-record)
               (error no-input-field-error)))
          (t
           (let ((cwd (or (cdr value)
                          (error no-cwd-error)))
                 (input (or (gethash 'input dep-record)
                            (error no-input-field-error))))
             (expand-file-name input cwd))))))
      (_ (error "Invalid FIELD")))))

;; TODO: offer "kind-full" extension field.
;; TODO: should we split the "lists" in LINE by comma?
(defun citre-readtags--parse-line (line &optional tagsfile-info
                                        require optional exclude
                                        require-ext optional-ext
                                        require-ext-dep optional-ext-dep
                                        parse-all-field)
  "Parse a LINE from readtags output.
This returns a hash table called \"record\" by Citre.  Its keys
are the fields, values are their values.  It can be utilized by
`citre-get-field'.

The keyword arguments can be used to specify the fields wanted in
the returned record. REQUIRE, OPTIONAL and EXCLUDE are similar to
`citre-readtags-get-records', but extension fields can't appear
in them.  Use these for extension fields:

- REQUIRE-EXT: A list containing extension fields that must be
  presented.  If any of these fields can't be get, an error will
  occur.
- OPTIONAL-EXT: A list containing fields that's optional.  For
  any field in it, if it can be get, it will be recorded; if
  can't, it's ignored, and no error will occur.

The normal field they depend on should appear in REQUIRE-EXT-DEP
and OPTIONAL-EXT-DEP.

TAGSFILE-INFO is needed to offer additional information for these
extension fields.  It is the additional info of the tags file
containing LINE.  Such TAGSFILE-INFO should be get using
`citre-readtags--get-tags-file-info'.

Keyword arguments must satisfy certain conditions, which the
caller should take care of.  `citre--readtags-parse-line' doesn't
check them for the sake of performance.  Other than those
mentioned above, we still have:

- All lists used as keyword arguments should not contain
  duplicated elements.
- REQUIRE and EXCLUDE shouldn't intersect.
- OPTIONAL and EXCLUDE shouldn't intersect.
- OPTIONAL and EXCLUDE should not be used together."
  (let* ((elts (split-string line "\t" t))
         (record (make-hash-table :test #'eq))
         (dep-record (make-hash-table :test #'eq))
         (parse-all-field (or exclude parse-all-field))
         (require-num (length require))
         (require-counter 0)
         (optional-num (length optional))
         (optional-counter 0)
         (require-ext-dep-num (length require-ext-dep))
         (require-ext-dep-counter 0)
         (optional-ext-dep-num (length optional-ext-dep))
         (optional-ext-dep-counter 0))
    (cl-block nil
      (dotimes (i (length elts))
        (let ((results (citre-readtags--parse-field (nth i elts) i)))
          (dolist (result results)
            (let* ((field (car result))
                   (value (cdr result))
                   (in-require (memq field require))
                   (in-optional (memq field optional))
                   (in-exclude (memq field exclude))
                   (in-require-ext-dep (memq field require-ext-dep))
                   (in-optional-ext-dep (memq field optional-ext-dep)))
              (when (or in-require in-optional
                        (and parse-all-field (null in-exclude)))
                (puthash field value record)
                (when in-require
                  (cl-incf require-counter))
                (when in-optional
                  (cl-incf optional-counter)))
              (when (or in-require-ext-dep in-optional-ext-dep)
                (puthash field value dep-record)
                (when in-require-ext-dep
                  (cl-incf require-ext-dep-counter))
                (when in-optional-ext-dep
                  (cl-incf optional-ext-dep-counter)))
              (when (and (null parse-all-field)
                         (eq require-counter require-num)
                         (eq optional-counter optional-num)
                         (eq require-ext-dep-counter require-ext-dep-num)
                         (eq optional-ext-dep-counter optional-ext-dep-num))
                (cl-return)))))))
    (when (or (< require-counter require-num)
              (< require-ext-dep-counter require-ext-dep-num))
      (error "Fields not found in tags file: %s"
             (string-join
              (mapcar #'symbol-name
                      (cl-union
                       (cl-set-difference require
                                          (hash-table-keys record))
                       (cl-set-difference require-ext-dep
                                          (hash-table-keys dep-record))))
              ", ")))
    (dolist (field require-ext)
      (puthash field
               (citre-readtags--get-ext-field dep-record field tagsfile-info)
               record))
    (dolist (field optional-ext)
      (when-let ((value (ignore-errors
                          (citre-readtags--get-ext-field
                           dep-record field tagsfile-info))))
        (puthash field value record)))
    record))

;;;; APIs

(defun citre-readtags-get-pseudo-tag (name tagsfile)
  "Read the value of pseudo tag NAME in tags file TAGSFILE.
NAME should not start with \"!_\".  Run

  $ ctags --list-pseudo-tags

to know the valid NAMEs."
  (let* ((program (or citre-readtags-program "readtags"))
         (name (concat "!_" name))
         (result (split-string
                  (shell-command-to-string
                   (concat
                    (citre-readtags--build-shell-command
                     program "-t" tagsfile "-Q" `(eq? ,name $name) "-D")
                    "; printf \"\n$?\n\""))
                  "\n" t))
         (status (car (last result)))
         (output (car (cl-subseq result 0 -1))))
    (if (string= status "0")
        (when output
          (nth 1 (split-string output "\t" t)))
      (error "Readtags: %s" output))))

(cl-defun citre-readtags-get-records
    (tagsfile &optional name match case-sensitive filter-sexp
              &key require optional exclude parse-all-field)
  "Get records of tags in tags file TAGSFILE based on the arguments.

TAGSFILE is the canonical path of tags file.  The meaning of the
optional arguments are:

- NAME: If this is an non-empty string, use the NAME action.
  Otherwise use the -l action.
- MATCH: Nil or `exact' means performing exact matching in the
  NAME action, `prefix' means performing prefix matching in the
  NAME action.
- CASE-SENSITIVE: Nil means performing case-insensitive
  matching in the NAME action, non-nil means performing
  case-sensitive matching in the NAME action.
- FILTER-SEXP: Should be nil, or a postprocessor expression.
  Non-nil means filtering the tags with it using -Q option.
  Please see the requirements of postprocessor expressions below.

Requirements of postprocessor expressions are:

- Should be a symbol, or a list containing
  symbols/strings/similar lists.
- Use strings for strings, symbols for
  operators/variables/anything else.
- Use `true' for `#t', `false' for `#f', and nil or `()'
  for `()'.

Each element in the returned value is a hash table containing the
tag and some of its fields, which can be utilized by
`citre-get-field'. The fields to contain can be customized by the
key arguments:

- REQUIRE: A list containing fields that must be presented.  If
  any of these fields doesn't exist, an error will occur.
- OPTIONAL: A list containing fields that's optional.  For any
  field in it, if it's presented in a line in the tags file, it
  will be recorded; if not presented, it's ignored, and no error
  will occur.
- EXCLUDE: A list containing fields that should be excluded.  All
  other fields will be recorded.

OPTIONAL and REQUIRE should not be used together.  When both
OPTIONAL and REQUIRE are not presented, then only the fields in
REQUIRE are parsed, unless PARSE-ALL-FIELD is non-nil.

Valid field names are strings. Please notice these field names:

- \"name\": The name of the tag itself.
- \"input\": The file containing the tag.
- \"pattern\": EX command used to search the tag in the file.

Citre treats some fields specially:

- \"line\" and \"end\": Their values are converted to integers.
- \"scope\": It's splitted into 2 fields: \"scope-kind\" and
  \"scope-name\".  It's recommended to generate tags file with
  --fields=+Z, or this field will not be prefixed by \"scope:\"
  in the tags file, and there's no reliable way to know such a
  field is about scope.  Currently when this happens, the class
  and struct scope is handled properly.

Certain fields may offer ambiguous information.  To ascertain
them, Citre offers its own extension fields:

- \"abspath\": The canonical path of \"input\".  Needs \"input\".

To use an extension field, it must appear in REQUIRE or OPTIONAL."
  (when (and optional exclude)
    (error "OPTIONAL and EXCLUDE can't be used together"))
  (when (cl-intersection require exclude)
    (error "REQUIRE and EXCLUDE can't intersect"))
  (when (cl-intersection optional exclude)
    (error "OPTIONAL and EXCLUDE can't intersect"))
  (let* ((optional (cl-set-difference optional require))
         (find-field-depends
          (lambda (field)
            (car (alist-get field
                            citre-readtags--extension-fields-alist
                            nil nil #'equal))))
         (find-info-depends
          (lambda (field)
            (nth 1 (alist-get field
                              citre-readtags--extension-fields-alist
                              nil nil #'equal))))
         (ext-fields (mapcar #'car citre-readtags--extension-fields-alist))
         (require-ext (cl-intersection require ext-fields))
         (optional-ext (cl-intersection optional ext-fields))
         (require-ext-dep (cl-delete-duplicates
                           (apply #'append
                                  (mapcar find-field-depends require-ext))))
         (optional-ext-dep (cl-delete-duplicates
                           (apply #'append
                                  (mapcar find-field-depends optional-ext))))
         (require (cl-delete-duplicates
                   (cl-set-difference require ext-fields)))
         (optional (cl-delete-duplicates
                    (cl-set-difference optional ext-fields)))
         (exclude (cl-delete-duplicates exclude))
         (kinds (cl-delete-duplicates
                (apply #'append
                       (mapcar find-info-depends
                               (append require-ext optional-ext)))))
         (info (apply #'citre-readtags--get-tags-file-info
                      tagsfile kinds)))
    (when (cl-intersection exclude ext-fields)
      (error "EXCLUDE shouldn't contain extension fields"))
    (mapcar (lambda (line)
              (citre-readtags--parse-line
               line info
               require optional exclude
               require-ext optional-ext
               require-ext-dep optional-ext-dep
               parse-all-field))
            (citre-readtags--get-lines
             tagsfile name match case-sensitive filter-sexp))))

;; TODO: When format a nil field with "%s", it becomes "nil", which is not
;; suitable for showing to the user.  Currently I don't know what's the best
;; way to deal with this, but thinking from a tags file's perspective, since it
;; never produce a field with only the field name but no value, there's no
;; difference if we use nil or empty string to represent it, so it's good to
;; directly use empty string in the records, or make `citre-get-field' not
;; return nil.

;; TODO: extensible "fields calculated in real time".
(defun citre-readtags-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre--readtags-parse-line'.  FIELD can
be all valid normal and extension fields, see
`citre-readtags--parse-line'.

When FIELD is \"line\", an integer is returned, instead of a
string.

FIELD can also be:

- \"line-content\": The line containing the tag.  Leading and
  trailing whitespaces are trimmed.  Depends on the \"abspath\"
  and \"line\" fields.

When FIELD is one of these values, the result is calculated in
real time, rather than get from the record."
  (pcase field
    ('line-content
     (when-let ((path (gethash abspath record))
                (line (gethash line record)))
       (when (file-exists-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (when (eq (forward-line (1- line)) 0)
             (string-trim (buffer-substring (line-beginning-position)
                                            (line-end-position))))))))
    (_ (gethash field record))))

(provide 'citre-readtags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre-readtags.el ends here
