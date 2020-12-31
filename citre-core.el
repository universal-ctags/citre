;;; citre-core.el --- A readtags abstraction layer -*- lexical-binding: t -*-

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

(require 'citre-core-tables)
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

(defun citre-core--escape-single-quote (string)
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
         (string (citre-core--escape-single-quote string))))
    (shell-command (format \"... \\='%s\\='\" STRING)))

Then \\='%s\\=' is formatted as:

  \\='arg\\='\"\\='\"'; rm -rf /\\='\"\\='\"\\='

Now the rm -rf / doesn't come out of the string.

This is for use in `citre-core--build-shell-command', where its
ARGS are exactly in this situation.  It makes sure that
unintentional attack doesn't happen when passing symbols/strings
with single quotes to it."
  (replace-regexp-in-string "'" "'\"'\"'" string))

(defun citre-core--build-shell-command (&rest args)
  "Build a shell command from ARGS.
Each element of ARGS could be a string, symbol or list.  For
strings, this formats them using \"%s\"; for symbols and lists,
this formats them using \"%S\". Then, each of them is wrapped in
single quotes, and concatenated with a space between each of
them.

Before wrapping in single quotes,
`citre-core--escape-single-quote' is applied to each of them to
prevent certain kinds of shell injection.  See its docstring for
details.

This function is not for building shell commands in general, but
only for Citre's own use, especially for building readtags
commands."
  (string-join
   (mapcar (lambda (elt)
             (format "'%s'"
                     (citre-core--escape-single-quote
                      (format (if (stringp elt) "%s" "%S") elt))))
           args)
   " "))

(defun citre-core--split-at-1st-colon (string)
  "Split STRING at the first colon in it.
A cons cell of the part before and after the colon is returned.
If STRING doesn't contain a colon, it will be (nil . STRING)."
  (if-let ((sep (string-match ":" string)))
      (cons (substring string 0 sep)
            (substring string (1+ sep)))
    (cons nil string)))

(defun citre-core--string-match-all-escaping-backslash
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
      ;; Jump over the char after the backslash to search for next escaping
      ;; sequence.  NOTE: This may cause an "args out of range" error, but only
      ;; on string containing invalid trailing backslashes.  We don't check it
      ;; for performance.
      (setq start (+ idx 2)))
    (nreverse result)))

;; TODO: Do we need to support Windows style path?
(defun citre-core--file-name-extension (file)
  "Return the extension of FILE.
If it doesn't have an extension, return the file name without
directory.

This is faster than `file-name-extension'."
  (or (string-match "\\.\\([^./]+\\)$" file)
      (string-match "/\\([^/]+\\)$" file))
  (match-string 1 file))

;;;; Internals: Additional information handling

(defvar citre-core--tags-file-info-alist nil
  "Alist for storing additional info about tags files.
Since tags files can offer ambiguous info, we use this variable
to store additional info to ascertain them.

This alist looks like:

  (alist of:
   tags file -> hash table of additional info:
                (info field -> value))

Info fields and their corresponding values are:

- `time': The last update time of the file info, which is, the
  hash table.  It's in the style of (current-time).
- `relative-path-p': Whether there's relative path used in the
  tags file.
- `dir': the current working directory when generating the tags
  file.  This can be nil when `use-relative-path' is t, since
  `path' would be useless in this situation.
- `one-letter-kind-p': Whether the tags file uses single-letter
  kind field.
- `kind-table': A hash table for getting full-length kinds from
  single-letter kinds, like `citre-core--kind-name-table', or nil
  if the TAG_KIND_DESCRIPTION pseudo tags are not presented.")

(defun citre--core-get-dir (tag ptag-cwd tagsfile relative-path-p)
  "Get the `dir' info of TAGSFILE.
TAG is a tag from the file, PTAG-CWD is the value of TAG_PROC_CWD
pseudo tag.  RELATIVE-PATH-P indicates whether the tags file uses
relative path."
  (or ptag-cwd
      ;; Further inspect it only when relative path is used.
      (let ((cwd-guess (file-name-directory tagsfile)))
        (when relative-path-p
          (or (when (file-exists-p
                     (expand-file-name (gethash 'input tag)
                                       cwd-guess))
                cwd-guess)
              (read-directory-name
               (format "Root dir of tags file %s: " tagsfile)))))))

(defun citre--core-get-kind-table (kind-descs)
  "Get the `kind-table' info.
KIND-DESCS is the values of TAG_KIND_DESCRIPTION pseudo tags."
  (when kind-descs
    (let ((prefix-len (length "!_TAG_KIND_DESCRIPTION!"))
          (table (make-hash-table :test #'equal)))
      (dolist (kind-desc kind-descs)
        (let* ((lang (substring (car kind-desc) prefix-len))
               (kind-pair (split-string (nth 1 kind-desc) ","))
               (kind (car kind-pair))
               (kind-full (nth 1 kind-pair)))
          (unless (gethash lang table)
            (puthash lang (make-hash-table :test #'equal) table))
          (puthash kind kind-full (gethash lang table))))
      table)))

;; NOTE: Since we call the `citre-core-get-records' API to get sample records
;; for analysis, and that in turn requires tags file info, we must prevent
;; infinite loop from happening.  We use the following rules:
;;
;; - We must not ask for extension fields (i.e., fields that don't exists in
;;   the tags file, but defined by Citre) here to get tags file info.
;; - `citre-core-get-records', when used to get only regular fields (i.e.,
;;   non-extension fields), must not ask for tags file info (which is what we
;;   are doing).

(defun citre-core--fetch-tags-file-info (tagsfile)
  "Write the additional info of TAGSFILE to `citre--tags-file-info-alist'.
TAGSFILE is the canonical path of the tags file.  The info is
returned."
  (let* ((recent-mod (file-attribute-modification-time
                      (file-attributes tagsfile)))
         (info (make-hash-table :test #'eq))
         ;; Get a tag with relative path.  TODO: Make it work on Windows.
         (tag (car (citre-core-get-records
                    tagsfile nil nil nil
                    :filter (not (citre-core-build-filter
                                  'input "/" 'prefix))
                    :require '(input pattern kind) :lines 1)))
         (relative-path-p (when tag t))
         (tag (or tag
                  ;; If tags file uses relative path, get its first tag.
                  (car (citre-core-get-records
                        tagsfile nil nil nil
                        :require '(input pattern kind) :lines 1))
                  (error "Invalid tags file")))
         (ptag-cwd (nth 1 (car (citre-core-get-pseudo-tags
                                "TAG_PROC_CWD" tagsfile))))
         (kind-descs (citre-core-get-pseudo-tags
                      "TAG_KIND_DESCRIPTION" tagsfile 'prefix)))
    (puthash 'time recent-mod info)
    ;; path
    (puthash 'relative-path-p relative-path-p info)
    (puthash 'dir (citre--core-get-dir tag ptag-cwd tagsfile relative-path-p)
             info)
    ;; kind
    (puthash 'one-letter-kind-p
             (eq 1 (length (citre-core-get-field 'kind tag)))
             info)
    (puthash 'kind-table
             (citre--core-get-kind-table kind-descs)
             info)
    info))

;; TODO: In many situations, we require the file path is not only absolute
;; (i.e., `file-name-absolute-p' returns t), but also "canonical" (i.e., AND it
;; doesn't start with "~"). We should make this definition clear in the
;; documentations for developers, and make the requirement clear in all the
;; docstrings.
(defun citre-core--tags-file-info (tagsfile)
  "Return the additional info FIELDS of tags file TAGSFILE.
TAGSFILE is the canonical path of the tags file.  The return
value is a valid value in `citre--tags-file-info-alist'.

This function caches the info, and uses the cache when possible."
  (unless (file-exists-p tagsfile)
    (error "%s doesn't exist" tagsfile))
  (let ((recent-mod (file-attribute-modification-time
                     (file-attributes tagsfile)))
        (info (alist-get tagsfile
                         citre-core--tags-file-info-alist
                         nil nil #'equal)))
    (if (and info (equal (gethash 'time info) recent-mod))
        info
      (setf (alist-get tagsfile
                       citre-core--tags-file-info-alist
                       nil nil #'equal)
            (citre-core--fetch-tags-file-info tagsfile)))))

;;;; Internals: Tags file filtering & parsing

;;;;; Get lines

;; The shell command in this function is a bit hard to understand.  I'll
;; explain it.  An example of `cmd' would be like:
;;
;; { readtags -l || printf "\n$?\n" 1>&2; } | head -n 1; echo "$?"
;;
;; It runs readtags, and if an error happens, write the exit code ($?) to
;; stderr.  We add a newline before $?, since the error message of readtags is
;; hard coded in its code, and I've seen missing trailing newlines in some of
;; them.  It's also redirected to stderr, so it won't be filtered by "head".
;;
;; The output of readtags is piped into "head" to get the first N lines.  Then
;; the exit code of "head" is echoed.  At the end we get something like:
;;
;; ...readtags output or error message (can be empty)...
;; when readtags fails, the exit code of it (can be empty)
;; the exit code of head
;;
;; If the exit code of readtags appears, we have to deal with it.  Here we
;; consider a value > 128 to be successful, since it indicates that readtags is
;; terminated by a signal, and that does happen: when readtags outputs a lot,
;; since "head" will close the pipe after it reads the first N lines, readtags
;; will receive a SIGPIPE signal, but we already get the line we want, and this
;; is preferred as we don't need to wait for readtags to finish all its output.
;;
;; There's no reliable way to get the exit code corresponding to SIGPIPE: its
;; signal number is 13, and POSIX standard says when a command is terminated by
;; a signal, the exit code should be > 128, but nothing more.  In practice,
;; most shell gives you 13 + 128, but at least ksh93 adds it by 256.
;;
;; If the inferior shell is bash, we have the PIPESTATUS variable to get the
;; exit code of the command before a pipe.  But we want the command to be
;; POSIX-compliant, so we have to take this uglier way.

(defun citre-core--get-lines
    (tagsfile &optional name match case-fold filter sorter lines)
  "Get lines in tags file TAGSFILE using readtags.
See `citre-core-get-records' to know about NAME, MATCH,
CASE-FOLD, FILTER, SORTER and LINES."
  ;; Prevents shell injection.
  (when lines (cl-assert (natnump lines)))
  (let* ((match (or match 'exact))
         (extras (concat
                  "-Ene"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-fold "i" "")))
         parts cmd)
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
    (setq
     cmd
     (format
      "{ %s || printf \"\\n$?\\n\" 1>&2; }%s"
      (apply #'citre-core--build-shell-command
             (nreverse parts))
      (if lines
          (format " | head -n %s; echo \"$?\"" lines)
        ;; The filtering succeeded because we did nothing (equivalent to
        ;; filtering by an "identity" function).
        "; echo \"0\"")))
    (let* ((result (split-string
                    (shell-command-to-string cmd)
                    "\n" t))
           (len (length result))
           (pipe-code (nth (- len 1) result))
           (readtags-code
            (when-let ((code (when (> len 1) (nth (- len 2) result))))
              (when (string-match-p "^\[0-9]+$" code) code)))
           (output (cl-subseq result 0 (if readtags-code -2 -1))))
      (if (and (string= pipe-code "0")
               (or (null readtags-code)
                   ;; TODO: test this on a large tags file?  It's not very
                   ;; realistic because a user doesn't want to clone a huge
                   ;; repo to use a Emacs package.
                   (> (string-to-number readtags-code) 128)))
          output
        (error "Readtags exits %s.  Head exits %s.\n%s"
               readtags-code pipe-code (string-join output "\n"))))))

;;;;; Parse lines

(defun citre-core--read-field-value (value)
  "Translate escaped sequences in VALUE.
See man tags(5) to know about the escaped sequences.  VALUE
should be a field value in a tags file."
  (if-let ((backslash-idx
            (citre-core--string-match-all-escaping-backslash value)))
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

(defun citre-core--forward-pattern (line pos)
  "Jump over the pattern field.
LINE is a tag line, POS is the start position of the pattern
field in it.  This returns its end position."
  ;; If pattern begins with a number, it will be like one of
  ;;
  ;; - 20;"
  ;; - 20;/pattern/;"
  ;; - 20;?pattern?;"
  (when (<= ?0 (aref line pos) ?9)
    ;; We jump after the first semicolon.
    (setq pos (1+ (string-match ";" line pos))))
  (pcase (aref line pos)
    (?\" (1+ pos))
    (c (let ((;; We want a regexp that takes us to near the end of the pattern,
              ;; which means the end of the match should be the closing
              ;; delimiter, so there's still a ;" between it and the end of the
              ;; pattern.
              pat-end-regexp
              (pcase c
                ;; Search for an unescaped slash.  It must be the ending
                ;; delimiter.
                (?/ "[^\\\\]\\(\\\\\\\\\\)*/")
                ;; The same.
                (?? "[^\\\\]\\(\\\\\\\\\\)*\\?")
                (_ (error "Invalid pattern field")))))
         (setq pos
               ;; Search from after the opening delimiter.
               (progn (string-match pat-end-regexp line (1+ pos))
                      (match-end 0)))
         (+ 2 pos)))))

(defun citre-core--lexer-forward-field-name (line length lexer)
  "Move the lexer forward the following field name.
LINE is a tag line.  LENGTH is its length.  LEXER is a vector
like [POS N], where POS is the beginning position of a field, and
it's the Nth field in the line (N counts from 0).

This sets POS to the beginning of the field value, and returns
the field name as a symbol.  When there's no more field to parse,
this returns nil, and the caller should stop parsing."
  (let ((pos (aref lexer 0))
        (n (aref lexer 1)))
    (pcase n
      (0 'name)
      (1 'input)
      (2 'pattern)
      (_ (when (< pos length)
           (let ((sep (string-match ":" line pos)))
             (cond
              ;; The kind field may not begin with "kind:".  It's always the
              ;; 4th field (N=3), but the 4th field is not always the kind
              ;; field.
              ((and (eq n 3)
                    (or (null sep)
                        (when-let ((tab (string-match "\t" line pos)))
                          (> sep tab))))
               'kind)
              (sep
               (let ((field-name (intern (substring line pos sep))))
                 (pcase field-name
                   ((or 'class 'struct)
                    'scope)
                   (_
                    (setf (aref lexer 0) (1+ sep))
                    field-name))))
              (t (error "Invalid LINE")))))))))

(defun citre-core--lexer-forward-field-value
    (line length lexer &optional parse-value)
  "Move the lexer forward the following field value.
LINE is a tag line.  LENGTH is its length.  LEXER is a vector
like [POS N], where POS is the beginning position of a field
value, and it's the Nth field in the line (N counts from 0).

This sets POS to the beginning of the next field, and add 1 to N.
If PARSE-VALUE is non-nil, returns the field value."
  (let ((pos (aref lexer 0))
        (nfield (aref lexer 1))
        tab value)
    (pcase nfield
      (2 (setq tab (citre-core--forward-pattern line pos))
         (when parse-value
           (setq value (substring line pos tab))))
      (_ (setq tab (or (string-match "\t" line pos)
                       length))
         (when parse-value
           (setq value
                 (citre-core--read-field-value (substring line pos tab))))))
    (setf (aref lexer 0) (1+ tab))
    (setf (aref lexer 1) (1+ nfield))
    value))

(defun citre-core--parse-line (line &optional tagsfile-info
                                    require optional exclude
                                    require-ext optional-ext ext-dep
                                    parse-all-fields)
  "Parse a LINE from readtags output.
This returns a hash table called \"record\" by Citre.  Its keys
are the fields, values are their values.  It can be utilized by
`citre-get-field'.

Optional arguments can be used to specify the fields wanted in
the returned record. REQUIRE, OPTIONAL, EXCLUDE and
PARSE-ALL-FIELDS are similar to `citre-core-get-records', but
extension fields can't appear in them.  Use these for extension
fields:

- REQUIRE-EXT: A list containing extension fields that must be
  presented.  If any of these fields can't be get, an error will
  occur.
- OPTIONAL-EXT: A list containing fields that's optional.  For
  any field in it, if it can be get, it will be recorded; if
  can't, it's ignored, and no error will occur.

The normal fields they depend on should appear in either REQUIRE,
OPTIONAL or EXT-DEP to make sure they are captured.

TAGSFILE-INFO is needed to offer additional information for these
extension fields.  It is the additional info of the tags file
containing LINE.  Such TAGSFILE-INFO should be get using
`citre-core--get-tags-file-info'.

The arguments must satisfy certain conditions, which the caller
should take care of.  `citre--core-parse-line' doesn't check them
for the sake of performance.  Other than those mentioned above,
we still have:

- All lists specifying needed fields should not contain
  duplicated elements.
- REQUIRE, OPTIONAL and EXCLUDE shouldn't intersect with each
  other.
- EXT-DEP shouldn't intersect with REQUIRE or OPTIONAL.
- OPTIONAL and EXCLUDE should not be used together."
  (let* ((record (make-hash-table :test #'eq :size 20))
         (parse-all-fields (or exclude parse-all-fields))
         (require-num (length require))
         (require-counter 0)
         (optional-num (length optional))
         (optional-counter 0)
         (ext-dep-num (length ext-dep))
         (ext-dep-counter 0)
         (lexer (vector 0 0))
         (len (length line))
         field
         (write (lambda (field)
                  (puthash field
                           (citre-core--lexer-forward-field-value
                            line len lexer t)
                           record))))
    (cl-block nil
      (while (setq field (citre-core--lexer-forward-field-name line len lexer))
        (cond
         ((memq field require)
          (funcall write field)
          (cl-incf require-counter))
         ((memq field optional)
          (funcall write field)
          (cl-incf optional-counter))
         (t (or
             (when (memq field ext-dep)
               (funcall write field)
               (cl-incf ext-dep-counter)
               t)
             (when (and parse-all-fields (null (memq field exclude)))
               (funcall write field)
               t)
             (citre-core--lexer-forward-field-value line len lexer nil))))
        (when (and (null parse-all-fields)
                   (eq require-counter require-num)
                   (eq optional-counter optional-num)
                   (eq ext-dep-counter ext-dep-num))
          (cl-return))))
    (when (< require-counter require-num)
      (error "Fields not found in tags file: %s"
             (string-join
              (mapcar #'symbol-name
                      (cl-set-difference require
                                         (hash-table-keys record)))
              ", ")))
    (dolist (field require-ext)
      (citre-core--write-ext-field record field tagsfile-info))
    (dolist (field optional-ext)
      (ignore-errors
        (citre-core--write-ext-field record field tagsfile-info)))
    (if parse-all-fields
        ;; Excluded field may be written because it's in `ext-dep'.
        (dolist (field exclude)
          (remhash field record))
      (dolist (field ext-dep)
        (remhash field record)))
    record))

;;;;; Extension fields

(defvar citre-core--ext-fields-dependency-alist
  '((ext-abspath   . (input))
    (ext-kind-full . (kind language input)))
  "Alist of extension fields and their dependencies.
Its keys are extension fields offered by Citre, values are lists
of (normal) fields the the extension field depends on.")

(defvar citre-core--ext-fields-method-table
  #s(hash-table
     test eq
     data
     (ext-abspath
      citre-core--get-ext-abspath
      ext-kind-full
      citre-core--get-ext-kind-full))
  "Hash table of extension fields and the methods to get them.
Its keys are extension fields offered by Citre, and values are
functions that return the value of the extension field.  The
arguments of the functions are:

- RECORD: A hash table containing the fields that the extension
  field depends on.
- TAGSFILE-INFO: The additional info of the tags file.  See
  `citre-core--tags-file-info' to know how to make use of it.

If the extension field can't be calculated, the functions should
signal an error, rather than return nil.

The needed RECORD and TAGSFILE-INFO are specified by
`citre-core--ext-fields-dependency-alist'.
`citre-core--write-ext-field' takes care to pass the needed
arguments to the functions.

If the function only needs RECORD, consider make it an extra
extension field (see `citre-core-extra-ext-fields-table').")

(defun citre-core--write-ext-field
    (record field tagsfile-info)
  "Write the value of extension field FIELD to RECORD.
RECORD should contain the fields that FIELD depends on.
TAGSFILE-INFO is the additional info that FIELD depends on."
  (if-let ((method (gethash field citre-core--ext-fields-method-table)))
      (puthash field (funcall method record tagsfile-info) record)
    (error "Invalid FIELD")))

;;;;;; ext-abspath

(defun citre-core--get-ext-abspath (record tagsfile-info)
  "Return the absolute path of the input file.
This needs the `input' field to be presented in RECORD, and if
it's value is a relative path, `path' info in TAGSFILE-INFO is
used.  If the `path' info doesn't contain the current working
directory when generating the tags file, an error will be
signaled."
  (let ((input (or (gethash 'input record)
                   (error "\"input\" field not found in RECORD"))))
    (if (file-name-absolute-p input)
        input
      (expand-file-name input (gethash 'dir tagsfile-info)))))

;;;;;; ext-kind-full

(defun citre-core--get-ext-kind-full (record tagsfile-info)
  "Return full-length kind name.
This needs the `kind' field to be presented in RECORD.  If the
tags file uses full-length kind name (told by TAGSFILE-INFO),
it's returned directly.  If not, then:

- The language is guessed first, see `citre-core--get-ext-lang'.
- The single-letter kind is converted to full-length, based on
  the TAG_KIND_DESCRIPTION pseudo tags, or
  `citre-core--kind-name-table' if it's not presented.

If this fails, the single-letter kind is returned directly."
  (if-let ((one-letter-kind-p (gethash 'one-letter-kind-p tagsfile-info)))
      (if-let* ((kind (gethash 'kind record))
                (lang (citre-core--get-lang-from-record record))
                (table (or (gethash 'kind-table tagsfile-info)
                           citre-core--kind-name-table))
                (table (gethash lang table))
                (kind-full (gethash kind table)))
          kind-full
        kind)
    (gethash 'kind record)))

;;;;; Get records from tags files

(cl-defun citre-core--get-records
    (tagsfile &optional name match case-fold
              &key filter sorter
              require optional exclude parse-all-fields lines)
  "Get records of tags in tags file TAGSFILE based on the arguments.

This is like `citre-core-get-records', which actually calls this
function internally.  The difference is this is a interface
that's closer to actual readtags command line calls.  The
differences are:

- NAME: If this is a non-empty string, use the NAME action.
  Otherwise use the -l action.
- MATCH: Can only be nil, `exact' or `prefix', which translates
  to arguments controlling the NAME action.
- CASE-FOLD: Only controls the NAME action.

Notice when calling `citre-core-get-records' with NAME being
`substr' or `regexp', it generates a filter expression to do
that, and is merged with FILTER by a logical `and'.

For SORTER, REQUIRE, OPTIONAL, EXCLUDE, PARSE-ALL-FIELDS and
LINES, see `citre-core-get-records'."
  (when (and optional exclude)
    (error "OPTIONAL and EXCLUDE can't be used together"))
  (when (cl-intersection require exclude)
    (error "REQUIRE and EXCLUDE can't intersect"))
  (when (cl-intersection optional exclude)
    (error "OPTIONAL and EXCLUDE can't intersect"))
  (let* ((optional (cl-set-difference optional require))
         (find-field-depends
          (lambda (field)
            (alist-get field citre-core--ext-fields-dependency-alist)))
         (ext-fields (mapcar #'car citre-core--ext-fields-dependency-alist))
         (require-ext (cl-intersection require ext-fields))
         (optional-ext (cl-intersection optional ext-fields))
         (ext-dep (cl-delete-duplicates
                   (apply #'append
                          (append
                           (mapcar find-field-depends require-ext)
                           (mapcar find-field-depends optional-ext)))))
         (ext-dep (cl-set-difference ext-dep require))
         (ext-dep (cl-set-difference ext-dep optional))
         (require (cl-delete-duplicates
                   (cl-set-difference require ext-fields)))
         (optional (cl-delete-duplicates
                    (cl-set-difference optional ext-fields)))
         (exclude (cl-delete-duplicates exclude))
         (info (when (or require-ext optional-ext)
                 (citre-core--tags-file-info tagsfile))))
    (when (cl-intersection exclude ext-fields)
      (error "EXCLUDE shouldn't contain extension fields"))
    (mapcar (lambda (line)
              (citre-core--parse-line
               line info
               require optional exclude
               require-ext optional-ext ext-dep
               parse-all-fields))
            (citre-core--get-lines
             tagsfile name match case-fold
             filter sorter lines))))

;;;; Internals: Extension fields from records

(defvar citre-core-extra-ext-fields-table
  #s(hash-table
     test eq
     data
     (extra-line
      citre-core--get-line-from-record
      extra-matched-str
      citre-core--get-matched-str-from-record
      extra-lang
      citre-core--get-lang-from-record))
  "Hash table for getting extra extension fields from records.
It's used by `citre-core-get-field'. Its keys will be valid FIELD
argument values for `citre-core-get-field', and values are
functions that return the value of the fields.  The arguments of
the functions are:

- RECORD: The record to get field from.

The needed fields in RECORD should appear in the docstrings of
the functions.  If the field can't be calculated, the functions
should return nil, rather than signal an error, so it feels more
like `gethash', which makes sense since the records are indeed
hash tables.

This table is intended to be extended by libraries that uses
citre-core.  They should not modify existing key-value pairs in
this table, and the added keys should be prefixed by the name of
the library to avoid naming conflict.")

(defun citre-core--get-line-from-record (record)
  "Get the line number from RECORD.
It tries these in turn:

- Use the line field directly.
- Use the pattern field if it contains the line number.
- Return nil."
  (or (citre-core-get-field 'line record)
      (car (citre-core--split-pattern
            (citre-core-get-field 'pattern record)))))

(defun citre-core--get-matched-str-from-record (record)
  "Get the string contained by the pattern field from RECORD.
Returns nil if the pattern field doesn't exist or contain a
search pattern."
  (when-let ((pat (nth 1 (citre-core--split-pattern
                          (citre-core-get-field 'pattern record)))))
    (car (citre-core--parse-search-pattern pat))))

(defun citre-core--get-lang-from-record (record)
  "Get language from RECORD.
It tries these in turn:

- Use the `language' field directly.
- Guess the language based on the `input' field.  See
  `citre-core--lang-extension-table'.
- Return the file extension, or the filename if it doesn't have
  an extension.
- Return nil."
  (or (gethash 'language record)
      (when-let ((input (gethash 'input record))
                 (extension (citre-core--file-name-extension input)))
        (or (gethash (downcase extension) citre-core--lang-extension-table)
            extension))))

;;;; APIs

;;;;; Build postprocessor expressions

(defun citre-core-build-filter (field string match
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
  (let ((field (intern (concat "$" (symbol-name field))))
        (filter
         (pcase match
           ('regexp
            `((string->regexp ,string :case-fold
                              ,(pcase case-fold
                                 ('nil 'false)
                                 (_ 'true)))
              ,field))
           (_ `(,(intern (concat (symbol-name match) "?"))
                ,(pcase case-fold
                   ('nil field)
                   (_ `(downcase ,field)))
                ,(pcase case-fold
                   ('nil string)
                   (_ (downcase string))))))))
    (when invert
      (setq filter `(not ,filter)))
    (when ignore-missing
      (setq filter `(or (not ,field) ,filter)))
    filter))

;; TODO: Should we convert between single-letter and full-length kinds here?
;; The implementation would be messy since it also involves the language field,
;; and we need to match the file extension if the language field is missing.
(defun citre-core-filter-match-kind (tagsfile kind)
  "Return a filter expression that matches the kind field by KIND.
If KIND is single-letter (or full-length), but the tags file
TAGSFILE uses full-length (or single-letter) kinds, then `true'
will be returned.  TAGSFILE is a canonical path."
  (let ((tags-file-one-letter-kind-p
         (gethash 'one-letter-kind-p
                  (citre-core--tags-file-info tagsfile)))
        (arg-one-letter-kind-p (eq (length kind) 1)))
    (if (eq tags-file-one-letter-kind-p
            arg-one-letter-kind-p)
        `(eq? $kind ,kind)
      'true)))

(defun citre-core-build-sorter (&rest fields)
  "Return a sorter expression based on FIELDS.
The return value can be used as the :sorter argument in
`citre-core-get-records'.

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

  (citre-core-build-sorter input \\='(line -))

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

(defun citre-core-get-pseudo-tags (name tagsfile &optional prefix)
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
                    (citre-core--build-shell-command
                     program "-t" tagsfile "-Q" `(,op $name ,name) "-D")
                    "; printf \"\n$?\n\""))
                  "\n" t))
         (status (car (last result)))
         (output (cl-subseq result 0 -1)))
    (if (string= status "0")
        (mapcar (lambda (line)
                  (split-string line "\t" t))
                output)
      (error "Readtags exits %s.\n%s" status output))))

(cl-defun citre-core-get-records
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
`citre-core-get-field'.  The fields to contain can be customized
by these keyword arguments:

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

- `ext-kind-full': The full name of `kind'. It uses the `kind'
  field if it's not single-letter, or it guesses the full name by
  `kind' and the language (which is also guessed by `input' if
  necessary).

  Needs `kind', `language' or `input' fields to be presented in
  the tags file.

  When this fails, the single-letter kind is used.

For more on extension fields, see
`citre-core--ext-fields-dependency-alist' and
`citre-core--ext-fields-method-table'.  To use an extension
field, it must appear in REQUIRE or OPTIONAL.

Other keyword arguments are:

- LINES: When non-nil, get the first LINES of records at most."
  (let* ((name- (when (memq match '(nil exact prefix)) name))
         (match- (when (memq match '(nil exact prefix)) match))
         (filter- (when (and name (memq match '(suffix substr regexp)))
                    (citre-core-build-filter 'name name match case-fold)))
         (filter- (if (and filter- filter)
                      `(and ,filter- ,filter)
                    (or filter- filter))))
    (citre-core--get-records tagsfile name- match- case-fold
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
(defun citre-core-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre--core-parse-line'.  FIELD can be
all valid normal and extension fields, see
`citre-core--parse-line'.

When FIELD is `line' or `end', an integer is returned, instead of
a string.

`citre-core-extra-ext-fields-table' defines some extra fields
that can be used as FIELD.  Their values are calculated in
real-time based on RECORD.  The built-in ones are:

- `extra-line': The line number of the tag.  This uses the `line'
  field directly, and if it's not presented, get the line number
  from the pattern field if it's a combined field.  If both can't
  be done, return nil.

- `extra-lang': The language.  It uses the `language' field if
  it's presented, or guesses the language by the file extension.
  When both fails, the file extension (or the file name, if it
  doesn't have an extension) is returned.  If this also
  fails (i.e. RECORD doesn't contain an `input' field), return
  nil.

- `extra-matched-str': The substring in the source file that's
  matched by ctags when generating the tag.  It's often the whole
  line containing the tag.  This depends on the `pattern' field,
  and returns nil if it doesn't record the matched
  string (e.g. in tags file generated using the -n option)."
  (if-let ((method (gethash field citre-core-extra-ext-fields-table)))
      (funcall method record)
    (pcase field
      ((or 'line 'end) (when-let ((val (gethash field record)))
                         (string-to-number val)))
      (_ (gethash field record)))))

;;;;; Helper for finding the location of a tag

;;;;;; Internals

(defvar citre-core--pattern-search-limit 50000
  "The limit of chars to go when searching for a pattern.")

(defun citre-core--split-pattern (pattern)
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

(defun citre-core--parse-search-pattern (pattern)
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
              (citre-core--string-match-all-escaping-backslash pattern)))
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

(defun citre-core--find-nearest-regexp
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

(defun citre-core-locate-tag (record &optional use-linum)
  "Find the tag RECORD in current buffer.
Returns the position to goto, or line number if USE-LINUM is
non-nil.  Current buffer should be the buffer visiting the file
containing the tag.

The search is helped by:

- The pattern field.
- The line field, if the pattern is not a combined
  pattern (i.e. not contatining the line number).
- The name of the tag.

The pattern and name field need to be presented, or an error will
be signaled.

This function does its best to find the tag if the file has been
changed, and even when the line including the tag itself has been
changed.  See the code for details.  If the search fails
completely, it will return the beginning position of the file.

This function has no side-effect on the buffer.  Upper components
could wrap this function to provide a desired UI for jumping to
the position of a tag."
  (pcase-let*
      ((name (or (citre-core-get-field 'name record)
                 (error "NAME field doesn't exist")))
       (pat (or (citre-core-get-field 'pattern record)
                (error "PATTERN field doesn't exist")))
       (`(,line ,pat) (citre-core--split-pattern pat))
       (line (or (citre-core-get-field 'line record) line))
       (`(,str ,from-beg ,to-end)
        (when pat (citre-core--parse-search-pattern pat)))
       (pat-beg (if from-beg "^" ""))
       (pat-end (if to-end "$" ""))
       (lim citre-core--pattern-search-limit))
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
           (citre-core--find-nearest-regexp
            (concat pat-beg (regexp-quote str) pat-end)
            lim)
           ;; Maybe the indentation or trailing whitespaces has changed, or
           ;; something is added after.
           (citre-core--find-nearest-regexp
            (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
            lim)
           ;; The content is changed.  Try cutting from the end of the tag
           ;; name and search.  From now on we also use case-fold search to
           ;; deal with projects that uses a case-insensitive language and
           ;; don't have a consistant style on it.
           (when-let ((bound (when (let ((case-fold-search nil))
                                     (string-match (regexp-quote name) str))
                               (match-end 0)))
                      (str (substring str 0 bound)))
             (citre-core--find-nearest-regexp
              (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
              lim 'case-fold))
           ;; Last try: search for the tag name.
           (citre-core--find-nearest-regexp (regexp-quote name)
                                            lim 'case-fold)))
        (if use-linum (line-number-at-pos) (point))))))

(provide 'citre-core)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-core.el ends here
