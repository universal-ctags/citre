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

;; This is a readtags abstraction layer.  See docs/core-layer.md to know
;; the design.  See the APIs section to know the APIs it offers.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-readtags-tables)
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

(defun citre-readtags--string-match-all (regexp string &optional start)
  "Find all occurences of REGEXP in STRING.
The return value is a list of their indexes, or nil.  If START is
non-nil, start search at that index in STRING.

This function internally calls `string-match'."
  (let ((result nil)
        (start (or start 0))
        (idx nil))
    (while (setq idx (string-match regexp string start))
      (push idx result)
      (setq start (1+ idx)))
    (nreverse result)))

(defun citre-readtags--string-match-all-escaping-backslash
    (string &optional start)
  "Find all occurence of escaping backslashes in STRING.
If START is non-nil, start search at that index in STRING.

This assumes the only escape sequence containing a second
backslash is \"\\\\\"."
  (let ((result nil)
        (start (or start 0))
        (idx nil))
    (while (setq idx (string-match "\\\\" string start))
      (push idx result)
      ;; NOTE: This may cause an "args out of range" error, but only on string
      ;; containing invalid trailing backslashes.  We don't check it for
      ;; performance.
      (setq start (cl-incf idx 2)))
    (nreverse result)))

(defun citre-readtags--count-string-match (regexp string &optional start end)
  "Count occurences of REGEXP in STRING.
If START is non-nil, start search at that index in STRING.  If
END is non-nil, end search before that index in STRING.

This function internally calls `string-match'."
  (let ((result 0)
        (start (or start 0))
        (idx nil))
    (while (and (setq idx (string-match regexp string start))
                (or (null end) (< idx end)))
      (cl-incf result)
      (setq start (1+ idx)))
    result))

;;;; Internals: Additional information handling

(defvar citre-readtags--tags-file-info-method-alist
  '((path . citre-readtags--get-path-info)
    (kind . citre-readtags--get-kind-info))
  "Alist of additional info fields and the functions to get them.
See `citre-readtags--tags-file-info-alist' to know about
additional info.

NOTICE: It's allowed for these functions to call
`citre-readtags-get-records', but notice that if asking it for
certain extension fields, it may in turn calls these functions
again, causing a max eval depth error.")

(defun citre-readtags--detect-tags-file-info (tagsfile field)
  "Detect the value of additional info FIELD of TAGSFILE.
TAGSFILE is the path to the tags file.  For valid FIELDs, see
`citre-readtags--tags-file-info-alist'."
  (if-let ((method
            (alist-get field citre-readtags--tags-file-info-method-alist)))
      (funcall method tagsfile)
    (error "Invalid FIELD")))

(defvar citre-readtags--tags-file-info-alist nil
  "Alist for storing additional info about tags files.
Since tags files can offer ambiguous info, we use this variable
to store additional info to ascertain them.

This alist looks like:

  (alist of:
   tags file -> hash table of additional info:
                (info field -> (time . value)))

TIME is the last update time of the info field, in the style
of (current-time).  Info fields and their corresponding VALUEs
are:

- `path': The value is a cons pair.  Its car is t when relative
  paths are used in the tags file, or nil when not.  Its cdr is
  the current working directory when generating the tags file.
- `kind': The value is a cons pair.  Its car is t when
  single-letter kinds are used in the tags file, or nil when not.
  Its cdr is a hash table for getting full-length kinds from
  single-letter kinds, like `citre-readtags--kind-name-table', or
  nil if the TAG_KIND_DESCRIPTION pseudo tags are not
  presented.")

(defmacro citre-readtags--tags-file-info (info field &optional key)
  "Return the place form of additional info FIELD in INFO.
INFO is a valid value in `citre-readtags--tags-file-info-alist',
For possible values of FIELD, see
`citre-readtags--get-tags-file-info'.  KEY could be `time' or
`value' to return the last update time of FIELD, or the value of
it.  When nil, (TIME . VALUE) is returned."
  (let ((form `(gethash ,field ,info)))
    `(pcase ,key
       ('time (car ,form))
       ('value (cdr ,form))
       ('nil ,form)
       (_ (error "Invalid KEY")))))

;; TODO: In many situations, we require the file path is not only absolute
;; (i.e., `file-name-absolute-p' returns t), but also "canonical" (i.e., AND it
;; doesn't start with "~"). We should make this definition clear in the
;; documentations for developers, and make the requirement clear in all the
;; docstrings.
(defun citre-readtags--get-tags-file-info (tagsfile &rest fields)
  "Return the additional info FIELDS of tags file TAGSFILE.
TAGSFILE is the canonical path to the tags file, and the return
value is the additional info of it.  This return value is a valid
value in `citre--tags-file-info-alist', see its docstring for
details.

FIELDS is a list of symbols representing the additional info
fields needed by the caller.  Presented ones are updated when:

- The tags file has not been registered in
  `citre--tags-file-info-alist'.
- The tags file has been updated since this field of info is
  asked last time.

For valid values in FIELDS, see
`citre-readtags--tags-file-info-alist'.  When FIELDS is nil, all
fields of info are updated."
  (unless (file-exists-p tagsfile)
    (error "%s doesn't exist" tagsfile))
  (let ((recent-modification (file-attribute-modification-time
                              (file-attributes tagsfile))))
    (cl-symbol-macrolet ((info (alist-get tagsfile
                                          citre-readtags--tags-file-info-alist
                                          nil nil #'equal)))
      (unless info
        (setf info (make-hash-table :test #'eq)))
      (dolist (field fields)
        (unless (equal (citre-readtags--tags-file-info info field 'time)
                       recent-modification)
          (setf (citre-readtags--tags-file-info info field)
                (cons recent-modification
                      (citre-readtags--detect-tags-file-info tagsfile field)))))
      info)))

;;;;; Info: path

(defun citre-readtags--tags-file-use-relative-path-p (tagsfile)
  "Detect if file paths in tags file TAGSFILE are relative.
TAGSFILE is the path to the tags file.  This is done by
inspecting the first line of regular tags."
  (let* ((record (car (citre-readtags-get-records
                       tagsfile nil nil nil
                       :require '(input) :lines 1))))
    (if (null record)
        (error "Invalid tags file")
      (not (file-name-absolute-p (citre-readtags-get-field 'input record))))))

(defun citre-readtags--get-path-info (tagsfile)
  "Get path info of tags file TAGSFILE.
See `citre-readtags--tags-file-info-alist' to know about the
return value.  It is a valid value field of the `path' field."
  (cons (citre-readtags--tags-file-use-relative-path-p tagsfile)
        (nth 1 (car (citre-readtags-get-pseudo-tags
                     "TAG_PROC_CWD" tagsfile)))))

;;;;; Info: kind

(defun citre-readtags--tags-file-use-single-letter-kind-p
    (tagsfile)
  "Detect if kinds in tags file TAGSFILE are single-letter.
TAGSFILE is the path to the tags file.  This is done by
inspecting the first line of regular tags."
  (let ((record (car (citre-readtags-get-records
                      tagsfile nil nil nil
                      :require '(kind) :lines 1))))
    (if (null record)
        (error "Invalid tags file")
      (eq 1 (length (citre-readtags-get-field 'kind record))))))

(defun citre-readtags--tags-file-kind-name-table (tagsfile)
  "Generate a kind name table for tags file TAGSFILE.
This is done by using the TAG_KIND_DESCRIPTION pseudo tags.  The
generated table is like `citre-readtags--kind-name-table'.

If the required pseudo tags are not presented, nil is returned."
  (let ((kind-descs (citre-readtags-get-pseudo-tags
                     "TAG_KIND_DESCRIPTION" tagsfile 'prefix))
        (prefix-len (length "!_TAG_KIND_DESCRIPTION!"))
        (table (make-hash-table :test #'equal)))
    (when kind-descs
      (dolist (kind-desc kind-descs)
        (let* ((lang (substring (car kind-desc) prefix-len))
               (kind-pair (split-string (nth 1 kind-desc) ","))
               (kind (car kind-pair))
               (kind-full (nth 1 kind-pair)))
          (unless (gethash lang table)
            (puthash lang (make-hash-table :test #'equal) table))
          (puthash kind kind-full (gethash lang table))))
      table)))

(defun citre-readtags--get-kind-info (tagsfile)
  "Get kind info of tags file TAGSFILE.
See `citre-readtags--tags-file-info-alist' to know about the
return value.  It is a valid value field of the `kind' field."
  (cons (citre-readtags--tags-file-use-single-letter-kind-p tagsfile)
        (citre-readtags--tags-file-kind-name-table tagsfile)))

;;;; Internals: Tags file filtering & parsing

;;;;; Get lines

(defun citre-readtags--get-lines
    (tagsfile &optional name match case-fold filter sorter lines)
  "Get lines in tags file TAGSFILE using readtags.
See `citre-readtags-get-records' to know about NAME, MATCH,
CASE-FOLD, FILTER, SORTER and LINES."
  (let* ((parts nil)
         (match (or match 'exact))
         (extras (concat
                  "-Ene"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-fold "i" ""))))
    ;; Program name
    (push (or citre-readtags-program "readtags") parts)
    ;; Read from this tags file
    (push "-t" parts)
    (push tagsfile parts)
    ;; Filter expression
    (when filter
      (push "-Q" parts)
      (push filter parts))
    (when sorter
      (push "-S" parts)
      (push sorter parts))
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
                      ;; "$?" is for getting the exit status.  In case the
                      ;; output of readtags is not terminated by newline, we
                      ;; add an extra one.
                      "; printf \"\n$?\n\""))
                    "\n" t))
           (status (car (last result)))
           (output (cl-subseq result 0 -1)))
      (if (string= status "0")
          (if (or (null lines) (> lines (length output)))
              output
            (cl-subseq output 0 lines))
        (error "Readtags: %s" (string-join output "\n"))))))

;;;;; Parse fields

(defun citre-readtags--read-field-value (value)
  "Translate escaped sequences in VALUE.
See man tags(5) to know about the escaped sequences.  VALUE
should be a field value in a tags file."
  (if-let ((backslash-idx (citre-readtags--string-match-all-escaping-backslash
                           value)))
      (let ((last 0)
            (i nil)
            (parts nil))
        (while (setq i (pop backslash-idx))
          (push (substring value last i) parts)
          (let ((char (aref value (1+ i))))
            (pcase char
              (?x (progn
                    (setq last (+ 4 i))
                    (push (char-to-string (string-to-number
                                           (substring value (+ 2 i) (+ 4 i))
                                           16))
                          parts)))
              (_ (progn
                   (setq last (+ 2 i))
                   (push (pcase char
                           (?t "\t") (?r "\r") (?n "\n") (?\\ "\\")
                           (?a "\a") (?b "\b") (?v "\v") (?f "\f")
                           (_ (error "Invalid escape sequence")))
                         parts))))))
        (push (substring value last) parts)
        (apply #'concat (nreverse parts)))
    value))

(defun citre-readtags--parse-field (field nth)
  "Parse FIELD from a line in readtags output.
FIELD is a substring from the line, which can be get using
`citre-readtags--split-tags-line'.  NTH is the position of the
field in the line, counts from zero.

The return value is a list of cons pairs, the cars are field
names, cdrs are the values."
  (pcase nth
    (0 `((name . ,(citre-readtags--read-field-value field))))
    (1 `((input . ,(citre-readtags--read-field-value field))))
    (2 `((pattern . ,field)))
    (3 (let* ((parts (citre-readtags--split-at-1st-colon field))
              (field-name (car parts))
              (field-name (pcase field-name
                            ('nil 'kind)
                            (_ (intern field-name))))
              (field-value (citre-readtags--read-field-value (cdr parts))))
         `((,field-name . ,field-value))))
    (_
     (let* ((parts (citre-readtags--split-at-1st-colon field))
            (field-name (car parts))
            (field-value (citre-readtags--read-field-value (cdr parts))))
       (pcase field-name
         ("scope"
          (let ((value (citre-readtags--split-at-1st-colon field-value)))
            `((scope-kind . ,(car value))
              (scope-name . ,(cdr value)))))
         ((or "class" "struct")
          `((scope-kind . ,field-name)
            (scope-name . ,field-value)))
         (_
          `((,(intern field-name) . ,field-value))))))))

;;;;; Extension fields

(defvar citre-readtags--ext-fields-dependency-alist
  '((ext-abspath   . ((input)
                      (path)))
    (ext-lang      . ((language input)
                      nil))
    (ext-kind-full . ((kind language input)
                      (kind))))
  "Alist of extension fields and their dependencies.
Its keys are extension fields offered by Citre, values are lists
of two elements:

- A list of (normal) fields the the extension field depends on.
- A list of kinds of additional info of tags file that the
  extension field depends on.")

(defvar citre-readtags--ext-fields-method-table
  #s(hash-table
     test eq
     data
     (ext-abspath
      citre-readtags--get-ext-abspath
      ext-lang
      citre-readtags--get-ext-lang
      ext-kind-full
      citre-readtags--get-ext-kind-full))
  "Hash table of extension fields and the methods to get them.
Its keys are extension fields offered by Citre, and values are
functions that return the value of the extension field.  The
arguments of the functions are:

- DEP-RECORD: A hash table containing the fields that the
  extension field depends on.
- TAGSFILE-INFO: The additional info of the tags file.  See
  `citre-readtags--tags-file-info' to know how to make use of
  it.

If the extension field can't be calculated, the functions should
signal an error, rather than return nil.

The needed DEP-RECORD and TAGSFILE-INFO are specified by
`citre-readtags--ext-fields-dependency-alist'.
`citre-readtags--get-ext-field' takes care to pass the needed
arguments to the functions.")

(defun citre-readtags--get-ext-field
    (dep-record field tagsfile-info)
  "Calculate the value of extension field FIELD.
DEP-RECORD is a hash table containing the fields that FIELD
depends on.  TAGSFILE-INFO is the additional info that FIELD
depends on."
  (if-let ((method (gethash field citre-readtags--ext-fields-method-table)))
        (funcall method dep-record tagsfile-info)
      (error "Invalid FIELD")))

;;;;;; ext-abspath

(defun citre-readtags--get-ext-abspath (dep-record tagsfile-info)
  "Return the absolute path of the input file.
This needs the `input' field to be presented in DEP-RECORD, and
if it's value is a relative path, `path' info in TAGSFILE-INFO is
used.  If the `path' info doesn't contain the current working
directory when generating the tags file, an error will be
signaled."
  (let ((path-info (citre-readtags--tags-file-info tagsfile-info
                                                   'path 'value))
        (no-cwd-error "Can't get absolute path.  You can:\n\
1. Regenerate the tags file with \"TAG_PROC_CWD\" pseudo tag enabled, or\n\
2. Regenerate the tags file using absolute paths in the command")
        (no-input-field-error "\"input\" field not found in DEP-RECORD"))
    (cond
     ((null (car path-info))
      (or (gethash 'input dep-record)
          (error no-input-field-error)))
     (t
      (let ((cwd (or (cdr path-info)
                     (error no-cwd-error)))
            (input (or (gethash 'input dep-record)
                       (error no-input-field-error))))
        (expand-file-name input cwd))))))

;;;;;; ext-lang

(defun citre-readtags--get-ext-lang (dep-record _)
  "Return the language.
If `language' field is presented in DEP-RECORD, it's returned
directly.  Otherwise, the language is guessed based on the file
extension of the `input' field in DEP-RECORD (if there's no
extension, the file name is used).  If the language can't be
guessed, the file extension is returned."
  (let ((lang (gethash 'language dep-record))
        (input (gethash 'input dep-record)))
    (cond
     (lang lang)
     (input (let ((extension (or (file-name-extension input)
                                 (file-name-nondirectory input))))
              (or (gethash (downcase extension)
                           citre-readtags--lang-extension-table)
                  extension)))
     (t (error "Ext-lang field required, but neither language field\
nor input field is found in DEP-RECORD")))))

;;;;;; ext-kind-full

(defun citre-readtags--get-ext-kind-full (dep-record tagsfile-info)
  "Return full-length kind name.
This needs the `kind' field to be presented in DEP-RECORD.  If
the tags file uses full-length kind name (told by TAGSFILE-INFO),
it's returned directly.  If not, then:

- The language is guessed first, see `citre-readtags--get-ext-lang'.
- The single-letter kind is converted to full-length, based on
  the TAG_KIND_DESCRIPTION pseudo tags, or
  `citre-readtags--kind-name-table' if it's not presented.

If this fails, the single-letter kind is returned directly."
  (let ((kind-info (citre-readtags--tags-file-info tagsfile-info
                                                   'kind 'value)))
    (if (null (car kind-info))
        (gethash 'kind dep-record)
      (if-let* ((kind (gethash 'kind dep-record))
                (lang (citre-readtags--get-ext-lang
                       dep-record tagsfile-info))
                (table (or (cdr kind-info)
                           citre-readtags--kind-name-table))
                (table (gethash lang table))
                (kind-full (gethash kind table)))
          kind-full
        kind))))

;;;;; Parse lines

(defun citre-readtags--split-tags-line (line)
  "Split LINE from a tags file into fields."
  (let* ((tab-idx (citre-readtags--string-match-all "\t" line))
         ;; This is the tab before the pattern
         (start (nth 1 tab-idx))
         (end nil)
         (pattern-delimiter nil)
         (delimiters-in-pattern 0)
         (tabs-in-pattern 0)
         (result nil))
    ;; There are at least 4 fields in a normal tag, so we can tell if something
    ;; is wrong based on the number of tabs.  Sometimes readtags exits normally
    ;; when an error actually occurs.  By doing this we can capture the error
    ;; messages in the output (as long as there aren't many tabs).
    (when (< (length tab-idx) 3)
      (error (format "Invalid LINE: %s" line)))
    (setq pattern-delimiter
          (pcase (aref line (1+ start))
            ((guard (string-match "^[0-9]+;\""
                                  (substring line (1+ start))))
             nil)
            ;; Make sure there are an even number of backslashes before a
            ;; delimiter, so we won't match escaped slashes or question marks.
            ((or ?/ (guard (string-match "^[0-9]+;/"
                                         (substring line (1+ start)))))
             ;; We always start the search on a tab, so no need to deal with
             ;; the situation where it starts with a slash.  A regexp which
             ;; deals with this would be \\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*/
             "[^\\\\]\\(\\\\\\\\\\)*/")
            ((or ?? (guard (string-match "^[0-9]+;\\?"
                                         (substring line (1+ start)))))
             "[^\\\\]\\(\\\\\\\\\\)*?")
            (_ (error "Invalid pattern field"))))
    (when pattern-delimiter
      (cl-dolist (end (nthcdr 2 tab-idx))
        (cl-incf delimiters-in-pattern
                 (citre-readtags--count-string-match
                  pattern-delimiter (substring line start end)))
        (if (eq (% delimiters-in-pattern 2) 0)
            (cl-return)
          (cl-incf tabs-in-pattern)
          (setq start end))))
    ;; We make `tab-idx' include all tabs that's not in the pattern, and also
    ;; the length of `line'. This makes it easier to split the whole line.
    (setq tab-idx (nconc (list (car tab-idx) (cadr tab-idx))
                         (nthcdr (+ 2 tabs-in-pattern) tab-idx)
                         (list (length line))))
    (setq start 0)
    (while (setq end (pop tab-idx))
      (push (substring line start end) result)
      (setq start (1+ end)))
    (nreverse result)))

(defun citre-readtags--parse-line (line &optional tagsfile-info
                                        require optional exclude
                                        require-ext optional-ext ext-dep
                                        parse-all-fields)
  "Parse a LINE from readtags output.
This returns a hash table called \"record\" by Citre.  Its keys
are the fields, values are their values.  It can be utilized by
`citre-get-field'.

Optional arguments can be used to specify the fields wanted in
the returned record. REQUIRE, OPTIONAL, EXCLUDE and
PARSE-ALL-FIELDS are similar to `citre-readtags-get-records', but
extension fields can't appear in them.  Use these for extension
fields:

- REQUIRE-EXT: A list containing extension fields that must be
  presented.  If any of these fields can't be get, an error will
  occur.
- OPTIONAL-EXT: A list containing fields that's optional.  For
  any field in it, if it can be get, it will be recorded; if
  can't, it's ignored, and no error will occur.

The normal field they depend on should appear in EXT-DEP.

TAGSFILE-INFO is needed to offer additional information for these
extension fields.  It is the additional info of the tags file
containing LINE.  Such TAGSFILE-INFO should be get using
`citre-readtags--get-tags-file-info'.

The arguments must satisfy certain conditions, which the caller
should take care of.  `citre--readtags-parse-line' doesn't check
them for the sake of performance.  Other than those mentioned
above, we still have:

- All lists specifying needed fields should not contain
  duplicated elements.
- REQUIRE and EXCLUDE shouldn't intersect.
- OPTIONAL and EXCLUDE shouldn't intersect.
- OPTIONAL and EXCLUDE should not be used together."
  (let* ((elts (citre-readtags--split-tags-line line))
         (record (make-hash-table :test #'eq :size 20))
         (dep-record (make-hash-table :test #'eq :size 10))
         (parse-all-fields (or exclude parse-all-fields))
         (require-num (length require))
         (require-counter 0)
         (optional-num (length optional))
         (optional-counter 0)
         (ext-dep-num (length ext-dep))
         (ext-dep-counter 0))
    (cl-block nil
      (dotimes (i (length elts))
        (let ((results (citre-readtags--parse-field (nth i elts) i)))
          (dolist (result results)
            (let* ((field (car result))
                   (value (cdr result))
                   (in-require (memq field require))
                   (in-optional (memq field optional))
                   (in-exclude (memq field exclude))
                   (in-ext-dep (memq field ext-dep)))
              (when (or in-require in-optional
                        (and parse-all-fields (null in-exclude)))
                (puthash field value record)
                (when in-require
                  (cl-incf require-counter))
                (when in-optional
                  (cl-incf optional-counter)))
              (when in-ext-dep
                (puthash field value dep-record)
                (cl-incf ext-dep-counter))
              (when (and (null parse-all-fields)
                         (eq require-counter require-num)
                         (eq optional-counter optional-num)
                         (eq ext-dep-counter ext-dep-num))
                (cl-return)))))))
    (when (< require-counter require-num)
      (error "Fields not found in tags file: %s"
             (string-join
              (mapcar #'symbol-name
                      (cl-set-difference require
                                          (hash-table-keys record)))
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

;;;;; Get records from tags files

(cl-defun citre-readtags--get-records
    (tagsfile &optional name match case-fold
              &key filter sorter
              require optional exclude parse-all-fields lines)
  "Get records of tags in tags file TAGSFILE based on the arguments.

This is like `citre-readtags-get-records', which actually calls
this function internally.  The difference is this is a interface
that's closer to actual readtags command line calls.  The
differences are:

- NAME: If this is a non-empty string, use the NAME action.
  Otherwise use the -l action.
- MATCH: Can only be nil, `exact' or `prefix', which translates
  to arguments controlling the NAME action.
- CASE-FOLD: Only controls the NAME action.

Notice when calling `citre-readtags-get-records' with NAME being
`substr' or `regexp', it generates a filter expression to do
that, and is merged with FILTER by a logical `and'.

For SORTER, REQUIRE, OPTIONAL, EXCLUDE, PARSE-ALL-FIELDS and
LINES, see `citre-readtags-get-records'."
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
                            citre-readtags--ext-fields-dependency-alist
                            nil nil #'equal))))
         (find-info-depends
          (lambda (field)
            (nth 1 (alist-get field
                              citre-readtags--ext-fields-dependency-alist
                              nil nil #'equal))))
         (ext-fields (mapcar #'car
                             citre-readtags--ext-fields-dependency-alist))
         (require-ext (cl-intersection require ext-fields))
         (optional-ext (cl-intersection optional ext-fields))
         (ext-dep (cl-delete-duplicates
                   (apply #'append
                          (append
                           (mapcar find-field-depends require-ext)
                           (mapcar find-field-depends optional-ext)))))
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
               require-ext optional-ext ext-dep
               parse-all-fields))
            (citre-readtags--get-lines
             tagsfile name match case-fold
             filter sorter lines))))

;;;; Internals: Extension fields from records

(defvar citre-readtags-extra-ext-fields-table
  #s(hash-table
     test eq
     data
     (extra-line
      citre-readtags--get-line-from-record
      extra-matched-str
      citre-readtags--get-matched-str-from-record))
  "Hash table for getting extra extension fields from records.
It's used by `citre-readtags-get-field'. Its keys will be valid
FIELD argument values for `citre-readtags-get-field', and values
are functions that return the value of the fields.  The arguments
of the functions are:

- RECORD: The record to get field from.

The needed fields in RECORD should appear in the docstrings of
the functions.  If the field can't be calculated, the functions
should return nil, rather than signal an error, so it feels more
like `gethash', which makes sense since the records are indeed
hash tables.

This table is intended to be extended by libraries that uses
citre-readtags.  They should not modify existing key-value pairs
in this table, and the added keys should be prefixed by the name
of the library to avoid naming conflict.")

(defun citre-readtags--get-line-from-record (record)
  "Get the line number from RECORD."
  (let ((line (car (citre-readtags--split-pattern
                    (citre-readtags-get-field 'pattern record)))))
    (or (citre-readtags-get-field 'line record) line)))

(defun citre-readtags--get-matched-str-from-record (record)
  "Get the line content from RECORD."
  (let ((pat (nth 1 (citre-readtags--split-pattern
                     (citre-readtags-get-field 'pattern record)))))
    (when pat (car (citre-readtags--parse-search-pattern pat)))))

;;;; APIs

;;;;; Build postprocessor expressions

(defun citre-readtags-build-filter (field string match
                                          &optional case-fold invert
                                          ignore-missing)
  "Return a filter expression that filters on FIELD by STRING.
MATCH could be:

- `eq': See if FIELD is STRING.
- `prefix': See if FIELD starts with STRING.
- `suffix': See if FIELD ends with STRING.
- `substr': See if FIELD contains STRING.
- `regexp': See if FIELD can be matched by regexp STRING.  \"/\"
  in STRING doesn't need to be escaped.

If CASE-FOLD is non-nil, do case-insensitive matching.  If INVERT
is non-nil, keep lines that doesn't match.  If IGNORE-MISSING is
non-nil, also keep lines where FIELD is missing."
  (let ((filter nil)
        (field (intern (concat "$" (symbol-name field)))))
    (pcase match
      ('regexp
       (setq filter
             `((string->regexp ,string :case-fold
                               ,(pcase case-fold
                                  ('nil 'false)
                                  (_ 'true)))
               ,field)))
      (_ (setq filter `(,(intern (concat (symbol-name match) "?"))
                        ,(pcase case-fold
                           ('nil field)
                           (_ `(downcase ,field)))
                        ,(pcase case-fold
                           ('nil string)
                           (_ (downcase string)))))))
    (when invert
      (setq filter `(not ,filter)))
    (when ignore-missing
      (setq filter `(or (not ,field) ,filter)))
    filter))

(defun citre-readtags-filter-match-input (tagsfile input)
  "Return a filter expression that matches the input field by INPUT.
INPUT can be canonical or relative, and it will be converted to
canonical (relative) path if the tags file TAGSFILE uses
canonical (relative) path.  TAGSFILE is a canonical path."
  (let* ((pathinfo (citre-readtags--tags-file-info
                    (citre-readtags--get-tags-file-info tagsfile 'path)
                    'path 'value))
         (tags-file-input-relative-p (car pathinfo))
         (arg-input-relative-p (not (file-name-absolute-p input)))
         (cwd (cdr pathinfo))
         (no-cwd-error "Can't get absolute path.  You can:\n\
1. Regenerate the tags file with \"TAG_PROC_CWD\" pseudo tag enabled, or\n\
2. Regenerate the tags file using absolute paths in the command"))
    (cond
     ((and tags-file-input-relative-p (not arg-input-relative-p))
      (unless cwd
        (error no-cwd-error))
      (setq input (substring input (length cwd))))
     ((and (not tags-file-input-relative-p) arg-input-relative-p)
      (unless cwd
        (error no-cwd-error))
      (setq input (concat cwd input))))
    `(eq? $input ,input)))

;; TODO: Should we convert between single-letter and full-length kinds here?
;; The implementation would be messy since it also involves the language field,
;; and we need to match the file extension if the language field is missing.
(defun citre-readtags-filter-match-kind (tagsfile kind)
  "Return a filter expression that matches the kind field by KIND.
If KIND is single-letter (or full-length), but the tags file
TAGSFILE uses full-length (or single-letter) kinds, then `true'
will be returned.  TAGSFILE is a canonical path."
  (let ((tags-file-kind-single-letter-p
         (car (citre-readtags--tags-file-info
               (citre-readtags--get-tags-file-info tagsfile 'kind)
               'kind 'value)))
        (arg-kind-single-letter-p (eq (length kind) 1)))
    (if (or (and tags-file-kind-single-letter-p
                 arg-kind-single-letter-p)
            (and (not tags-file-kind-single-letter-p)
                 (not arg-kind-single-letter-p)))
        `(eq? $kind ,kind)
      'true)))

(defun citre-readtags-build-sorter (&rest fields)
  "Return a sorter expression based on FIELDS.
The return value can be used as the :sorter argument in
`citre-readtags-get-records'.

Each element of FIELDS can be:

- A symbol.  For example, `input' means sort based on the input
  field, in ascending order.
- A list `(symbol +)' or `(symbol -)'.  For example, `(line +)'
  means sort based on the line field, in ascending order,
  and `(line -)' means in descending order.
- A list `(operator symbol +)' or `(operator symbol -)'.  For
  example, `(length name +)' means sort based on the lengths of
  the tag names, in ascending order.

When multiple elements are presented in FIELDS, they are tried in
order, until the order is decided.  For example,

  (citre-readtags-build-sorter input \\='(line -))

means sort by the file name, then the line number (in descending
order) if they are in the same file.

NOTICE: You should make sure that the used fields are not
presented only in some of the lines in the tags file, or readtags
will run into errors.  It's ok if it's missing in all lines."
  (let* ((sorter (list '<or>))
         (err-msg "Invalid element in FIELDS: %s")
         (var (lambda (field op)
                (let ((var (intern (concat op (symbol-name (nth 1 field))))))
                  (pcase (car field)
                    ('nil var)
                    (_ `(,(car field) ,var))))))
         (expr (lambda (field)
                 (pcase (nth 2 field)
                   ('+ `(<> ,(funcall var field "$")
                            ,(funcall var field "&")))
                   ('- `(<> ,(funcall var field "&")
                            ,(funcall var field "$")))
                   (_ (error (format err-msg field)))))))
    (dolist (field fields)
      (let ((field (pcase field
                     ((pred symbolp)
                      `(nil ,field +))
                     ((pred listp)
                      (pcase (length field)
                        (2 `(nil ,(car field) ,(nth 1 field)))
                        (3 field)
                        (_ (error (format err-msg field))))))))
        (push (funcall expr field) sorter)))
    (nreverse sorter)))

;;;;; Get records from tags files

(defun citre-readtags-get-pseudo-tags (name tagsfile &optional prefix)
  "Read pseudo tags matching NAME in tags file TAGSFILE.
When PREFIX is non-nil, match NAME by prefix.

NAME should not start with \"!_\".  Run

  $ ctags --list-pseudo-tags

to know the valid NAMEs.  The return value is a list, and each
element of it is another list consists of the fields separated by
tabs in a pseudo tag line."
  (let* ((program (or citre-readtags-program "readtags"))
         (name (concat "!_" name))
         (op (if prefix 'prefix? 'eq?))
         (result (split-string
                  (shell-command-to-string
                   (concat
                    (citre-readtags--build-shell-command
                     program "-t" tagsfile "-Q" `(,op $name ,name) "-D")
                    "; printf \"\n$?\n\""))
                  "\n" t))
         (status (car (last result)))
         (output (cl-subseq result 0 -1)))
    (if (string= status "0")
        (mapcar (lambda (line)
                  (split-string line "\t" t))
                output)
      (error "Readtags: %s" output))))

(cl-defun citre-readtags-get-records
    (tagsfile &optional name match case-fold
              &key filter sorter
              require optional exclude parse-all-fields lines)
  "Get records of tags in tags file TAGSFILE based on the arguments.

TAGSFILE is the canonical path of tags file.  The meaning of the
optional arguments are:

- NAME: Should be a string or nil.  If this is a non-empty
  string, filter the tags matching NAME before FILTER does its
  job.  Otherwise they are only filtered by FILTER.

- MATCH: How to match the tag name by NAME.  Can be:

  - Nil or `exact': Match tags whose name is NAME.
  - `prefix': Match tags that start with NAME.
  - `suffix': Match tags that ends with NAME.
  - `substr': Match tags that contains NAME.
  - `regexp': Match tags that match the regexp NAME.

  Nil, `exact' and `prefix' are done by the NAME action in
  readtags, others are done by the filter expression.

- CASE-FOLD: Nil means performing case-insensitive matching,
  non-nil means performing case-sensitive matching.

Filter and sorter expressions can be specified by these keyword
arguments:

- FILTER: Should be nil, or a postprocessor expression.  Non-nil
  means filtering the tags with it using -Q option.
- SORTER: Should be nil, or a postprocessor expression.  Non-nil
  means sortering the tags with it using -S option.

Requirements of postprocessor expressions are:

- Should be a symbol, or a list containing
  symbols/strings/similar lists.
- Use strings for strings, symbols for
  operators/variables/anything else.
- Use `true' for `#t', `false' for `#f', and nil or `()'
  for `()'.
- Use `(string->regexp \"PATTERN\")' or `(string->regexp
  \"PATTERN\" :case-fold true)' for `#/PATTERN/' or
  `#/PATTERN/i'.

Each element in the returned value is a hash table containing the
fields of matched tags, which can be utilized by
`citre-readtags-get-field'.  The fields to contain can be
customized by these keyword arguments:

- REQUIRE: A list containing fields that must be presented.  If
  any of these fields doesn't exist, an error will occur.
- OPTIONAL: A list containing fields that's optional.  For any
  field in it, if it's presented in a line in the tags file, it
  will be recorded; if not presented, it's ignored, and no error
  will occur.
- EXCLUDE: A list containing fields that should be excluded.  All
  other fields will be recorded.

OPTIONAL and EXCLUDE should not be used together.  When both
OPTIONAL and EXCLUDE are not presented, then only the fields in
REQUIRE are parsed, unless PARSE-ALL-FIELDS is non-nil.

Fields should be symbols.  Please notice these fields:

- `name': The name of the tag itself.
- `input': The file containing the tag.
- `pattern': EX command used to search the tag in the file.

Citre treats some fields specially:

- `scope': It's splitted into 2 fields: `scope-kind' and
  `scope-name'.  It's recommended to generate tags file with
  --fields=+Z, or this field will not be prefixed by \"scope:\"
  in the tags file, and there's no reliable way to know such a
  field is about scope.  Currently when this happens, the class
  and struct scope is handled properly.

Certain fields may offer ambiguous information.  To ascertain
them, Citre offers its own extension fields:

- `ext-abspath': The canonical path of `input'.

  Needs `input' field to be presented in the tags file.

- `ext-lang': The language.  It uses the `language' field if it's
  presented, or it guesses the language by the file extension.

  Needs `language' or `input' fields to be presented in the tags
  file.

  When this fails, the file extension (or the file name, if it
  doesn't have an extension) is used.

- `ext-kind-full': The full name of `kind'. It uses the `kind'
  field if it's not single-letter, or it guesses the full name by
  `kind' and the language (which is also guessed by `input' if
  necessary).

  Needs `kind', `language' or `input' fields to be presented in
  the tags file.

  When this fails, the single-letter kind is used.

For more on extension fields, see
`citre-readtags--ext-fields-dependency-alist' and
`citre-readtags--ext-fields-method-table'.  To use an extension
field, it must appear in REQUIRE or OPTIONAL.

Other keyword arguments are:

- LINES: When non-nil, get the first LINES of records at most."
  (let* ((name- (when (memq match '(nil exact prefix)) name))
         (match- (when (memq match '(nil exact prefix)) match))
         (filter- (when (and name (memq match '(suffix substr regexp)))
                    (citre-readtags-build-filter
                     'name name match case-fold)))
         (filter- (if (and filter- filter)
                      `(and ,filter- ,filter)
                    (or filter- filter))))
    (citre-readtags--get-records tagsfile name- match- case-fold
                                 :filter filter- :sorter sorter
                                 :require require :optional optional
                                 :exclude exclude
                                 :parse-all-fields parse-all-fields
                                 :lines lines)))

;;;;; Get fields from records

;; TODO: When format a nil field with "%s", it becomes "nil", which is not
;; suitable for showing to the user.  Currently I don't know what's the best
;; way to deal with this, but thinking from a tags file's perspective, since it
;; never produce a field with only the field name but no value, there's no
;; difference if we use nil or empty string to represent it, so it's good to
;; directly use empty string in the records, or make `citre-get-field' not
;; return nil.
(defun citre-readtags-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre--readtags-parse-line'.  FIELD can
be all valid normal and extension fields, see
`citre-readtags--parse-line'.

When FIELD is `line' or `end', an integer is returned, instead of
a string.

`citre-readtags-extra-ext-fields-table' defines some extra fields
that can be used as FIELD.  Their values are calculated in
real-time based on RECORD.  The built-in ones are:

- `extra-line': The line number of the tag.  This uses the `line'
  field directly, and if it's not presented, get the line number
  from the pattern field if it's a combined field.  If both can't
  be done, return nil.

- `extra-matched-str': The substring in the source file that's
  matched by ctags when generating the tag.  It's often the whole
  line containing the tag.  This depends on the `pattern' field,
  and returns nil if it doesn't record the matched
  string (e.g. in tags file generated using the -n option)."
  (if-let ((method
            (gethash field
                     citre-readtags-extra-ext-fields-table)))
      (funcall method record)
    (pcase field
      ((or 'line 'end) (string-to-number (gethash field record)))
      (_ (gethash field record)))))

;;;;; Helper for finding the location of a tag

;;;;;; Internals

(defvar citre-readtags--pattern-search-limit 50000
  "The limit of chars to go to search for a pattern.")

(defun citre-readtags--split-pattern (pattern)
  "Split the pattern PATTERN.
PATTERN should be the pattern field from a tag line.  It
returns (LINUM PAT) where:

- LINUM is the line number PATTERN contains (an integer), or nil
  if not presented.
- PAT is the search pattern that PATTERN contains, or nil if not
  presented."
  (let (line pat)
    (pcase pattern
      ;; Line number pattern
      ((guard (string-match "^\\([0-9]+\\);\"$" pattern))
       (setq line (string-to-number (match-string 1 pattern))))
      ;; Search/combined pattern
      ((guard (string-match "^\\([0-9]*\\);?\\([/?].*[/?]\\);\"$" pattern))
       (let ((num (match-string 1 pattern)))
         (setq line (unless (string-empty-p num) (string-to-number num))))
       (setq pat (match-string 2 pattern)))
      (_ (error "Invalid PATTERN")))
    (list line pat)))

(defun citre-readtags--parse-search-pattern (pattern)
  "Parse the search pattern PATTERN.
PATTERN looks like /pat/ or ?pat?.  It should come from the
pattern field of a tag line.

The returned value is (STR FROM-BEG TO-END), where STR is
the (literal) string that PATTERN matches.  If FROM-BEG is
non-nil, the string should begin from the beginning of a line.
The same for TO-END.

The reason we need this function is the pattern field is actually
not a regexp.  It only adds \"^\" and \"$\", and escape several
chars.  See the code of this function for the detail."
  (let* ((direction (pcase (aref pattern 0)
                      (?/ 'forward)
                      (?? 'backward)))
         ;; Remove the surrounding "/"s or "?"s.
         (pattern (substring pattern 1 -1))
         (from-beg (unless (string-empty-p pattern) (eq ?^ (aref pattern 0))))
         ;; Check if there's an unescaped trailing "$".  Don't be scared, eval:
         ;;
         ;; (rx (or (not "\\") line-start) (zero-or-more "\\\\") "$" line-end)
         ;;
         ;; to understand it.
         (to-end (string-match "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\$$" pattern))
         ;; Remove the beginning "^" and trailing "$"
         (pattern (substring pattern
                             (if from-beg 1 0)
                             (if to-end -1 nil))))
    (if-let ((backslash-idx
              (citre-readtags--string-match-all-escaping-backslash pattern)))
        (let ((last 0)
              (i nil)
              (parts nil))
          (while (setq i (pop backslash-idx))
            (push (substring pattern last i) parts)
            (setq last (+ 2 i))
            (let ((char (aref pattern (1+ i))))
              (push (pcase char
                      (?\\ "\\")
                      ((and ?$ (guard (eq i (- (length pattern) 2)))) "$")
                      ((and ?? (guard (eq direction 'backward))) "?")
                      ((and ?/ (guard (eq direction 'forward))) "/")
                      (_ (error "Invalid escape sequence")))
                    parts)))
          (push (substring pattern last) parts)
          (setq pattern (apply #'concat (nreverse parts)))))
    (list pattern from-beg to-end)))

(defun citre-readtags--find-nearest-regexp
    (regexp &optional limit case-fold)
  "Find the nearest occurence of REGEXP from current position.
By \"nearar\" we mean there are fewer lines between current
position and the occurence.

This goes to the beginning of line position of the occurence, and
returns the position there.  If it's not found, return nil and
don't go anywhere.

When LIMIT is non-nil, it's the limit of chars that the search
goes.  CASE-FOLD decides case-sensitivity."
  (let ((start (line-beginning-position))
        (case-fold-search case-fold)
        after after-lines
        before before-lines)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             regexp (when limit (+ start limit)) t)
        (beginning-of-line)
        (setq after (point))
        (setq after-lines (count-lines start after))))
    (unless (and after (<= after-lines 1))
      (save-excursion
        (beginning-of-line)
        (when (re-search-backward
               regexp (when limit (- start limit)) t)
          (beginning-of-line)
          (setq before (point))
          (setq before-lines (count-lines before start)))))
    (cond
     ((and after before)
      (goto-char (if (< before-lines after-lines) before after)))
     ((or after before)
      (goto-char (or after before))))))

;;;;;; The API

(defun citre-readtags-locate-tag (record &optional use-linum)
  "Find the tag RECORD in current buffer.
Returns the position to goto, or line number if USE-LINUM is
non-nil.  Current buffer should be the buffer visiting the file
containing the tag.

The search is helped by:

- The pattern field.
- The line field, if the pattern is not a combined
  pattern (i.e. not contatining the line number).
- The name of the tag.

This function does its best to find the tag if the file has been
changed, and even when the line including the tag itself has been
changed.  See the code for details.  If the search fails
completely, it will return the beginning position of the file.

This function has no side-effect on the buffer.  Upper components
could wrap this function to provide a desired UI for jumping to
the position of a tag."
  (pcase-let*
      ((name (citre-readtags-get-field 'name record))
       (`(,line ,pat) (citre-readtags--split-pattern
                       (citre-readtags-get-field 'pattern record)))
       (line (or (citre-readtags-get-field 'line record) line))
       (`(,str ,from-beg ,to-end)
        (when pat (citre-readtags--parse-search-pattern pat)))
       (pat-beg (if from-beg "^" ""))
       (pat-end (if to-end "$" ""))
       (lim citre-readtags--pattern-search-limit))
    ;; NOTE: No need to explicitely throw/return whether the line or pattern
    ;; exists, since this is "doing its best to search" and always need to
    ;; return a position even if the search fails.
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (when line (forward-line (1- line)))
        (when pat
          (or
           ;; Search for the whole line.
           (citre-readtags--find-nearest-regexp
            (concat pat-beg (regexp-quote str) pat-end)
            lim)
           ;; Maybe the indentation or trailing whitespaces has changed, or
           ;; something is added after.
           (citre-readtags--find-nearest-regexp
            (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
            lim)
           ;; The content is changed.  Try cutting from the end of the tag
           ;; name and search.  From now on we also use case-fold search to
           ;; deal with projects that uses a case-insensitive language and
           ;; don't have a consistant style on it.
           (when-let ((bound (when (string-match (regexp-quote name) str)
                               (match-end 0)))
                      (str (substring str 0 bound)))
             (citre-readtags--find-nearest-regexp
              (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
              lim 'case-fold))
           ;; Last try: search for the tag name.
           (citre-readtags--find-nearest-regexp (regexp-quote name)
                                                lim 'case-fold)))
        (if use-linum (line-number-at-pos) (point))))))

(provide 'citre-readtags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre-readtags.el ends here
