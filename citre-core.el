;;; citre-core.el --- A readtags abstraction layer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 04 May 2020
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

;; This is a readtags abstraction layer.  See
;; docs/developer-manual/project-structure.md to know the design.  See the APIs
;; section in this file to know the APIs it offers.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common)
(require 'citre-core-tables)
(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defgroup citre nil
  "Code editing & reading solution based on Universal Ctags."
  :group 'convenience
  :group 'tools
  :prefix "citre-"
  :link '(url-link "https://github.com/universal-ctags/citre"))

(defcustom citre-readtags-program nil
  "The name or path of the readtags program.
Set this if readtags is not in your PATH, or its name is not
\"readtags\".

Citre requires the readtags program provided by Universal Ctags."
  :type '(set string (const nil)))

;;;; Internals: Basic Helpers

(defun citre-core--string-non-empty-p (string)
  "Test if STRING is an non-empty string."
  (and (stringp string)
       (not (string-empty-p string))))

(defun citre-core--strip-text-property-in-list (object)
  "Recursively traverse OBJECT and strip properties in strings."
  (pcase object
    ((and (pred stringp) val) (substring-no-properties val))
    ((and (pred consp) val)
     (cons (citre-core--strip-text-property-in-list (car val))
           (citre-core--strip-text-property-in-list (cdr val))))
    (val val)))

(defmacro citre-core--error-on-arg (arg test)
  "Test ARG using TEST, and throw an error if it fails.
When calling the APIs, some arguments are likely to be calculated
based on information fetched from the environment, and they may
have problems like being empty, or not having the right type for
being nil.  This should be used to test them."
  `(unless (ignore-errors (funcall ,test ,arg))
     (error "%s fails on %s.  It is a %s: %S"
            (quote ,test) (upcase (symbol-name (quote ,arg)))
            (type-of ,arg) ,arg)))

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
- `remotep': Whether the tags file is a remote file.
- `dir': The full current working directory when generating the
  tags file.  It's a remote dir when tags file is a remote file
  name.
- `os': When the local part of `dir' is unix-style path (begins
  with a slash), this is `unix', or it's `nt'.  We have such a
  field since `system-type' can't tell us about the remote
  machine, and the behavior of many file functions depends on the
  *local* system type.
- `kind-table': A hash table for getting full-length kinds from
  single-letter kinds, like
  `citre-core--kind-name-single-to-full-table', or nil if the
  TAG_KIND_DESCRIPTION pseudo tags are not presented.")

(defvar citre-core--tags-file-cwd-guess-table (make-hash-table :test #'equal)
  "A hash table for guessed cwd for tags files.
This is used in `citre-core--get-dir'.  See its docstring for
details.  This is intended for upper components to set since it's
easier for them to infer the cwd.

Its keys are canonical paths of tags files, values are their
cwds (absolute path, can be remote or local when the tags file is
a remote file).")

(defvar citre-core--dont-prompt-for-cwd nil
  "In `citre-core--get-dir', don't ask the user for the cwd.
When non-nil, it uses the directory of the tags file as the cwd
instead.  This is for running unit tests only, as tags files in
them don't have TAG_PROC_CWD ptag.")

(defun citre-core--get-dir-os (ptag-cwd tagsfile)
  "Get the `dir' and `os' info of TAGSFILE.
PTAG-CWD is the value of TAG_PROC_CWD pseudo tag, and is used
when non-nil.  If it's nil, we have fallbacks:

- Get the guessed cwd from
  `citre-core--tags-file-cwd-guess-table'.  This table is
  intended fo upper components to set because they better
  understanding of the project structure.
- Prompt the user to choose a dir.

This returns a cons pair like (dir . os)."
  (let* ((dir (or ptag-cwd
                  (gethash tagsfile citre-core--tags-file-cwd-guess-table)
                  (if citre-core--dont-prompt-for-cwd
                      (file-name-directory tagsfile)
                    (read-directory-name
                     (format "Root dir of tags file %s: " tagsfile)))))
         (dir (expand-file-name dir))
         (dir-local (file-local-name dir)))
    ;; Ctags on windows generates disk symbol in capital letter, while if DIR
    ;; is given by Emacs, it may be a small letter.  We don't use `system-type'
    ;; to detect since we may work on a remote Unix machine on Windows.
    (unless (eq (aref dir-local 0) ?/)
      (setq dir-local (citre-upcase-first-letter dir-local)))
    (cons
     ;; If tagsfile is a remote file, we may have to prefix dir by the remote
     ;; identifier (e.g., if dir comes from the TAG_PROC_CWD ptag).
     (if-let ((remote-id (file-remote-p tagsfile)))
         (concat remote-id dir-local)
       dir-local)
     (pcase (aref dir-local 0)
       (?/ 'unix)
       (_ 'nt)))))

(defun citre-core--get-kind-table (kind-descs)
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

(defun citre-core--fetch-tags-file-info (tagsfile)
  "Write the additional info of TAGSFILE to `citre--tags-file-info-alist'.
TAGSFILE is the canonical path of the tags file.  The info is
returned."
  (let* ((recent-mod (file-attribute-modification-time
                      (file-attributes tagsfile)))
         (info (make-hash-table :test #'eq))
         (ptag-cwd (nth 1 (car (citre-core-get-pseudo-tags
                                "TAG_PROC_CWD" tagsfile))))
         (kind-descs (citre-core-get-pseudo-tags
                      "TAG_KIND_DESCRIPTION" tagsfile 'prefix)))
    ;; time
    (puthash 'time recent-mod info)
    ;; remotep
    (puthash 'remotep (file-remote-p tagsfile) info)
    ;; dir
    (pcase-let ((`(,dir . ,os) (citre-core--get-dir-os ptag-cwd tagsfile)))
      (puthash 'dir dir info)
      (puthash 'os os info))
    ;; kind-table
    (puthash 'kind-table
             (citre-core--get-kind-table kind-descs)
             info)
    info))

;;;; Internals: Tags file filtering & parsing

;;;;; Get lines

(defvar citre-core-stop-process-on-input nil
  "Non-nil allows user input to stop readtags process.
Let-bind this to non-nil for situations like popup completions to
make a responsive UI.

When the process is stopped on input, `citre-core-get-tags' and
similar APIs return nil.")

(defun citre-core--get-lines
    (tagsfile &optional name match case-fold filter sorter action)
  "Get lines in TAGSFILE using readtags.
See `citre-core-get-tags' to know about NAME, MATCH, CASE-FOLD,
FILTER, and SORTER.  ACTION can be nil, to get regular tags, or
any valid actions in readtags, e.g., \"-D\", to get pseudo tags."
  (let* ((match (or match 'exact))
         (extras (concat
                  "-Ene"
                  (pcase match
                    ('exact "")
                    ('prefix "p")
                    (_ (error "Unexpected value of MATCH")))
                  (if case-fold "i" "")))
         (output-buf (get-buffer-create " *citre-readtags*"))
         (tagsfile (substring-no-properties tagsfile))
         (name (when name (substring-no-properties name)))
         (filter (citre-core--strip-text-property-in-list filter))
         (sorter (citre-core--strip-text-property-in-list sorter))
         inhibit-message
         cmd proc exit-msg)
    (with-current-buffer output-buf
      (erase-buffer))
    ;; Program name
    (push (or citre-readtags-program "readtags") cmd)
    ;; Read from this tags file
    (push "-t" cmd)
    (let ((local-name (file-local-name tagsfile)))
      (if (equal local-name tagsfile)
          (push tagsfile cmd)
        ;; Suppress TRAMP message when it starts a remote process.
        (setq inhibit-message t)
        (push local-name cmd)))
    ;; Filter expression
    (when filter (push "-Q" cmd) (push (format "%S" filter) cmd))
    (when sorter (push "-S" cmd) (push (format "%S" sorter) cmd))
    ;; Extra arguments
    (push extras cmd)
    ;; Action
    (if action (push action cmd)
      (if (or (null name) (string-empty-p name))
          (push "-l" cmd)
        (push "-" cmd)
        (push name cmd)))
    ;; We allow keyboard quit when waiting for the process to finish, by
    ;; `with-local-quit'.  This line is to make sure the following cleanup
    ;; can't be breaked by user input, especially quickly swapping the sentinel
    ;; function, see comments later.
    (let ((inhibit-quit t))
      ;; Credit: This technique is developed from
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32986.
      (pcase
          ;; Allow keyboard quit when waiting the process.
          (with-local-quit
            (catch 'citre-done
              (setq proc
                    (make-process
                     :name "readtags"
                     :buffer output-buf
                     :command (nreverse cmd)
                     :connection-type 'pipe
                     ;; NOTE: Using a buffer or pipe for :stderr has caused a
                     ;; lot of troubles on Windows.
                     :stderr nil
                     :sentinel (lambda (_proc _msg)
                                 ;; While we use `sleep-for' for pending,
                                 ;; throw/catch can stop the pending.
                                 (throw 'citre-done t))
                     :file-handler t))
              ;; Poll for the process to finish.  Once it's finished, the
              ;; sentinel function throws a tag which breaks the sleeping.
              (if citre-core-stop-process-on-input
                  ;; `sit-for' wakes up when input is avaliable.
                  (while (sit-for 30))
                ;; `sleep-for' doesn't bother with input (except `C-g').
                (while (sleep-for 30)))))
        ;; Since we throw t, this can only receive nil when user input arrives.
        ;; When this happens, we immediately replace the sentinel function, or
        ;; it will throw to the wild and cause a "No catch for tag: citre-done"
        ;; error.
        ('nil (set-process-sentinel proc #'ignore)
              (if (eq system-type 'windows-nt)
                  ;; Based on my experiment on a large tags file,
                  ;; `interrupt-process' doesn't work reliably on Windows,
                  ;; while sighup seems does.  When using a remote Unix machine
                  ;; on Windows, this may send a SIGHUP to the remote process,
                  ;; but shouldn't be a problem since SIGHUP is not a harsh
                  ;; signal.
                  (signal-process proc 'sighup)
                (interrupt-process proc))
              nil)
        (_ (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 nil)
                (s (setq exit-msg (format "readtags exits %s\n" s)))))
             (s (setq exit-msg
                      (format "abnormal status of readtags: %s\n" s))))
           (cl-symbol-macrolet
               ((output (with-current-buffer output-buf (buffer-string)))
                (output-list (split-string output "\n" t))
                (output-list-while-no-input
                 (pcase (while-no-input output-list) ('t nil) (val val))))
             (if exit-msg
                 (error (concat exit-msg output))
               ;; Allow user input to stop the post-processing part as it can
               ;; also take some time.
               (if citre-core-stop-process-on-input
                   output-list-while-no-input
                 output-list))))))))

;;;;; Parse tagline

(defun citre-core--read-field-value (value)
  "Translate escaped sequences in VALUE.
See tags(5) manpage to know about the escaped sequences.  VALUE
should be a field value in a tags file."
  (if-let ((backslash-idx
            (citre-string-match-all-escaping-backslash value)))
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
LINE is a tagline, POS is the start position of the pattern field
in it.  This returns its end position."
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
LINE is a tagline.  LENGTH is its length.  LEXER is a vector like
[POS N], where POS is the beginning position of a field, and it's
the Nth field in the line (N counts from 0).

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
LINE is a tagline.  LENGTH is its length.  LEXER is a vector like
[POS N], where POS is the beginning position of a field value,
and it's the Nth field in the line (N counts from 0).

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
  "Parse a tagline LINE.
This returns a hash table called \"tag\" by Citre.  Its keys are
the fields, values are their values.  It can be utilized by
`citre-get-field'.

Optional arguments can be used to specify the fields wanted in
the returned tag. REQUIRE, OPTIONAL, EXCLUDE and PARSE-ALL-FIELDS
are similar to `citre-core-get-tags', but extension fields can't
appear in them.  Use these for extension fields:

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
`citre-core-tags-file-info'.

The arguments must satisfy certain conditions, which the caller
should take care of.  `citre-core--parse-line' doesn't check them
for the sake of performance.  Other than those mentioned above,
we still have:

- All lists specifying needed fields should not contain
  duplicated elements.
- REQUIRE, OPTIONAL and EXCLUDE shouldn't intersect with each
  other.
- EXT-DEP shouldn't intersect with REQUIRE or OPTIONAL.
- OPTIONAL and EXCLUDE should not be used together."
  (let* ((tag (make-hash-table :test #'eq :size 20))
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
                           tag))))
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
                                         (hash-table-keys tag)))
              ", ")))
    (dolist (field require-ext)
      (citre-core--write-ext-field tag field tagsfile-info))
    (dolist (field optional-ext)
      (ignore-errors
        (citre-core--write-ext-field tag field tagsfile-info)))
    (if parse-all-fields
        ;; Excluded field may be written because it's in `ext-dep'.
        (dolist (field exclude)
          (remhash field tag))
      (dolist (field ext-dep)
        (remhash field tag)))
    tag))

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

- TAG: A hash table containing the fields that the extension
  field depends on.
- TAGSFILE-INFO: The additional info of the tags file.  See
  `citre-core--tags-file-info' to know how to make use of it.

If the extension field can't be calculated, the functions should
signal an error, rather than return nil.

The needed TAG and TAGSFILE-INFO are specified by
`citre-core--ext-fields-dependency-alist'.
`citre-core--write-ext-field' takes care to pass the needed
arguments to the functions.

If the function only needs TAG, consider make it an extra
extension field (see `citre-core-extra-ext-fields-table').")

(defun citre-core--write-ext-field
    (tag field tagsfile-info)
  "Write the value of extension field FIELD to TAG.
TAG should contain the fields that FIELD depends on.
TAGSFILE-INFO is the additional info that FIELD depends on."
  (if-let ((method (gethash field citre-core--ext-fields-method-table)))
      (puthash field (funcall method tag tagsfile-info) tag)
    (error "Invalid FIELD")))

;;;;;; ext-abspath

(defun citre-core--get-ext-abspath (tag tagsfile-info)
  "Return the full path of the input file.
This needs the `input' field to be presented in TAG, and if its
value is a relative path, `dir' info in TAGSFILE-INFO is used.

This returns a remote path when the tagsfile is remote."
  (let* ((input (or (gethash 'input tag)
                    (error "\"input\" field not found in TAG")))
         (remotep (gethash 'remotep tagsfile-info))
         (os (gethash 'os tagsfile-info))
         (input-absolute-p (pcase os
                             ('unix (eq (aref input 0) ?/))
                             ;; tags file uses capital letter on Windows.
                             ('nt (and (<= ?A (aref input 0) ?Z)
                                       (eq (aref input 1) ?:))))))
    (if input-absolute-p
        (if remotep
            (concat (file-remote-p (gethash 'dir tagsfile-info)) input)
          input)
      (expand-file-name input (gethash 'dir tagsfile-info)))))

;;;;;; ext-kind-full

(defun citre-core--get-ext-kind-full (tag tagsfile-info)
  "Return full-length kind name.
This needs the `kind' field to be presented in TAG.  If the tags
file uses full-length kind name (told by TAGSFILE-INFO), it's
returned directly.  If not, then:

- The language is guessed first, see `citre-core--get-ext-lang'.
- The single-letter kind is converted to full-length, based on
  the TAG_KIND_DESCRIPTION pseudo tags, or
  `citre-core--kind-name-single-to-full-table' if it's not
  presented.

If this fails, the single-letter kind is returned directly."
  (if-let* ((kind (gethash 'kind tag))
            ;; Check if the kind is single letter.
            (single-letter-p (eq (length kind) 1))
            (lang (citre-core--get-lang-from-tag tag))
            (table (or (gethash 'kind-table tagsfile-info)
                       citre-core--kind-name-single-to-full-table))
            (table (gethash lang table))
            (kind-full (gethash kind table)))
      kind-full
    kind))

;;;;; Get tags from tags files

(cl-defun citre-core--get-tags
    (tagsfile &optional name match case-fold
              &key filter sorter
              require optional exclude parse-all-fields)
  "Get tags in TAGSFILE.
This is like `citre-core-get-tags', which actually calls this
function internally.  The difference is this is a interface
that's closer to actual readtags command line calls.  The
differences are:

- NAME: If this is a non-empty string, use the NAME action.
  Otherwise use the -l action.
- MATCH: Can only be nil, `exact' or `prefix', which translates
  to arguments controlling the NAME action.
- CASE-FOLD: Only controls the NAME action.

Notice when calling `citre-core-get-tags' with NAME being
`substr' or `regexp', it generates a filter expression to do
that, and is merged with FILTER by a logical `and'.

For SORTER, REQUIRE, OPTIONAL, EXCLUDE, and PARSE-ALL-FIELDS, see
`citre-core-get-tags'."
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
                 (citre-core-tags-file-info tagsfile))))
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
             filter sorter nil))))

;;;; Internals: Extra extension fields

(defvar citre-core-extra-ext-fields-table
  #s(hash-table
     test eq
     data
     (extra-line
      citre-core--get-line-from-tag
      extra-matched-str
      citre-core--get-matched-str-from-tag
      extra-lang
      citre-core--get-lang-from-tag))
  "Hash table for getting extra extension fields from tags.
It's used by `citre-core-get-field'. Its keys will be valid FIELD
argument values for `citre-core-get-field', and values are
functions that return the value of the fields.  The arguments of
the functions are:

- TAG: The tag to get field from.

The needed fields in TAG should appear in the docstrings of the
functions.  If the field can't be calculated, the functions
should return nil, rather than signal an error, so it feels more
like `gethash', which makes sense since the tags are indeed hash
tables.

Packages that uses citre-core could extend this table.  They
should not modify existing key-value pairs in this table, and the
added keys should be prefixed by the name of the library to avoid
naming conflict.")

(defun citre-core--get-line-from-tag (tag)
  "Get the line number from TAG.
It tries these in turn:

- Use the line field directly.
- Use the pattern field if it contains the line number.
- Return nil."
  (or (citre-core-get-field 'line tag)
      (car (citre-core--split-pattern
            (citre-core-get-field 'pattern tag)))))

(defun citre-core--get-matched-str-from-tag (tag)
  "Get the string contained by the pattern field from TAG.
Returns nil if the pattern field doesn't exist or contain a
search pattern."
  (when-let* ((pat (citre-core-get-field 'pattern tag))
              (search-pat (nth 1 (citre-core--split-pattern pat))))
    (car (citre-core--parse-search-pattern search-pat))))

(defun citre-core--get-lang-from-tag (tag)
  "Get language from TAG.
It tries these in turn:

- Use the `language' field directly.
- Guess the language based on the `input' field.  See
  `citre-core--extension-lang-table'.
- Return the file extension, or the filename if it doesn't have
  an extension.
- Return nil."
  (or (gethash 'language tag)
      (when-let ((input (gethash 'input tag))
                 (extension (citre-file-name-extension input)))
        (or (gethash (downcase extension) citre-core--extension-lang-table)
            extension))))

;;;; APIs

;;;;; Tags file info

(defun citre-core-tags-file-info (tagsfile)
  "Return the additional info of tags file TAGSFILE.
TAGSFILE is the absolute path of the tags file.  The return
value is a valid value in `citre-core--tags-file-info-alist'.

This function caches the info, and uses the cache when possible."
  (citre-core--error-on-arg tagsfile #'stringp)
  (unless (and (file-exists-p tagsfile)
               (not (file-directory-p tagsfile)))
    (error "%s doesn't exist" tagsfile))
  (let ((tagsfile (file-truename tagsfile))
        (recent-mod (file-attribute-modification-time
                     (file-attributes tagsfile)))
        (info (alist-get tagsfile
                         citre-core--tags-file-info-alist
                         nil nil #'equal)))
    (if (and info (equal (gethash 'time info) recent-mod))
        info
      (let ((info (citre-core--fetch-tags-file-info tagsfile)))
        ;; Seems `setf' in Emacs 26 doesn't return the last VAL.
        (setf (alist-get tagsfile
                         citre-core--tags-file-info-alist
                         nil nil #'equal)
              info)
        info))))

;;;;; Build filter expressions

;;;;;; Internals

(defun citre-core--filter-regexp-builder (str1 str2 case-fold)
  "Build filter expression that matches STR1 by STR2.
STR1 can be a string or a symbol representing a field.  STR2 must
be a string.  When CASE-FOLD is non-nil, do case-insensitive
matching."
  (unless (stringp str2)
    (error "STR2 must be a string"))
  `((string->regexp ,str2
                    :case-fold
                    ,(pcase case-fold
                       ('nil 'false)
                       (_ 'true)))
    ,str1))

(defun citre-core--csv-contain-regexp-builder (str)
  "Build a regexp that matches a CSV string that contains STR.
STR can also be a list of strings, then the regexp matches a CSV
string that contains any element in STR.

This is for use in readtags filter."
  (let ((string-or-list-of-string-p
         (lambda (str)
           (or (stringp str)
               (and str (null (cl-position nil (mapcar #'stringp str))))))))
    (citre-core--error-on-arg str string-or-list-of-string-p))
  (when (stringp str)
    (setq str (list str)))
  (concat "(^|,) ?("
          (mapconcat #'citre-core-regexp-quote str "|")
          ")(,|$)"))

(defun citre-core--filter-case-fold-string-builder (str case-fold)
  "Convert STR by CASE-FOLD.
STR can be a string or a symbol representing a field.  When
case-fold is non-nil, its downcased version is returned.
Otherwise it's directly returned."
  (if case-fold
      (if (symbolp str)
          `(downcase ,str)
        (downcase str))
    str))

;;;;;; APIs

(defun citre-core-regexp-quote (str)
  "Return a regexp that matches STR in readtags filter expressions.
Readtags uses POSIX extended regular expressions (ERE), which is
different from regexp in Emacs."
  ;; (rx (or "(" ")" "[" "]" "{" "}" "." "*" "+" "^" "$" "|" "?" "\\"))
  (replace-regexp-in-string "[]$(-+.?[\\{|}^]" "\\\\\\&" str))

(defun citre-core-filter (str1 str2 match
                               &optional case-fold invert keep-missing)
  "Return a filter expression that matches STR1 and STR2.
Both STRs can be a string, or a symbol of the field name.  STR2
can also be a list of strings if MATCH is `csv-contain', see
below.  MATCH could be:

- `eq': See if STR1 equals STR2.
- `prefix': See if STR1 is prefixed by STR2.
- `suffix': See if STR1 is suffixed by STR2.
- `substr': See if STR1 contains STR2.
- `regexp': See if STR1 can be matched by STR2, which is a
  regexp.  \"/\" in strings doesn't need to be escaped.  STR2
  must be a string.
- `csv-contain': See if STR1 contains STR2 as a member, where
  STR1 is a comma-separated list.  STR2 can be a string, or a
  list of strings, then it sees if STR1 contains any element in
  STR2.

The order of STR1 and STR2 may feel a bit weird for Elisp users.
That's because the convention of readtags is use STR1 as the
\"target string\", and use STR2 as the prefix/suffix/regexp...

If CASE-FOLD is non-nil, do case-insensitive matching.  If INVERT
is non-nil, flip the filter so it only keep lines that doesn't
match.  If KEEP-MISSING is non-nil, also keep lines where the
fields pointed by STR1 or STR2 (if one/both of them are symbols)
are missing, otherwise only keep lines that have those fields."
  (let* (syms
         (str-process
          (lambda (str)
            (if (symbolp str)
                (let ((sym (intern (concat "$" (symbol-name str)))))
                  (push sym syms)
                  sym)
              str)))
         (str1 (funcall str-process str1))
         (str2 (funcall str-process str2))
         filter final-filter)
    (setq filter
          (if (memq match '(regexp csv-contain))
              (let ((str2 (if (eq match 'regexp)
                              str2
                            (citre-core--csv-contain-regexp-builder str2))))
                (citre-core--filter-regexp-builder str1 str2 case-fold))
            (let ((symbol-or-string-p (lambda (str)
                                        (or (symbolp str) (stringp str)))))
              (citre-core--error-on-arg str1 symbol-or-string-p)
              (citre-core--error-on-arg str2 symbol-or-string-p))
            (let* ((str1 (citre-core--filter-case-fold-string-builder
                          str1 case-fold))
                   (str2 (citre-core--filter-case-fold-string-builder
                          str2 case-fold)))
              `(,(intern (concat (symbol-name match) "?")) ,str1 ,str2))))
    (when invert
      (setq filter `(not ,filter)))
    ;; The value of a missing field is #f, and applying string operators on it
    ;; produces an error.  So we have to make sure it's not #f beforehand.
    (push (if keep-missing 'or 'and) final-filter)
    (dolist (sym syms)
      (push (if keep-missing `(not ,sym) sym) final-filter))
    (push filter final-filter)
    (nreverse final-filter)))

(defun citre-core-filter-field-exist (field &optional invert)
  "Return a filter expression that requires FIELD to exist.
FIELD is a symbol of the field name.  When INVERT is non-nil,
require FIELDS to be missing."
  (let ((field (intern (concat "$" (symbol-name field)))))
    (if invert
        `(not ,field)
      field)))

(defun citre-core-filter-lang (lang)
  "Return a filter expression that requires the language to be LANG.
If the `language' field exists, this uses that field, otherwise
filter based on the extension of the `input' field, which may not
be accurate.

Run \"ctags --list-languages\" to see valid values of LANG.  Be
careful about the capitalization!"
  (let* ((ext (gethash lang citre-core--lang-extension-table)))
    `(or ,(citre-core-filter 'language lang 'eq)
         ,(if ext
              (citre-core-filter
               'input
               (concat "\\.("
                       (string-join (mapcar #'citre-core-regexp-quote ext) "|")
                       ")$")
               'regexp)
            'true))))

(defun citre-core-filter-kind (kind &optional ignore-missing)
  "Return a filter expression that matches the kind field by KIND.
KIND should be a full-length kind.  The generated filter works on
tags file using single-letter `kind' field, but it will match
more tags than it should, because a single-letter kind can
corresponds to multiple full-length kinds.

When TAGSFILE is non-nil, it detects if the tags file uses
single-letter kind, and generate simpler (and presumably faster)
filter based on that.  When IGNORE-MISSING is non-nil, also keep
tags that don't have `kind' field."
  (let ((kinds (gethash kind citre-core--kind-name-full-to-single-table)))
    (push kind kinds)
    (citre-core-filter
     'kind
     (concat "^("
             (string-join (mapcar #'citre-core-regexp-quote kinds) "|")
             ")$")
     'regexp nil nil ignore-missing)))

(defun citre-core-filter-input (filename tagsfile)
  "Return a filter expression that matches the input field by FILENAME.
TAGSFILE is the absolute path of the tags file.  FILENAME should
be absolute.  The generated filter can work no matter the tag
uses relative or absolute path."
  (let* ((tagsfile (expand-file-name tagsfile))
         ;; We use this to match the input field in the tags file, so we need
         ;; the local path.
         (local-name (file-local-name (expand-file-name filename)))
         (local-name-nondir (file-name-nondirectory local-name))
         ;; local-name and the path in a tag may look different, but actually
         ;; point to the same location, through symlink.  If the tag records
         ;; the symlink path, and FILENAME is the truepath, we can't solve it.
         ;; But if the opposite is true, we can convert FILENAME to its
         ;; truepath and match by it.

         ;; TODO: Test this.  For now I don't know if git + symlink has any
         ;; problem on Windows.
         (truename (file-local-name (file-truename filename)))
         ;; Don't bother with truename if it's the same as local-name.
         (truename (unless (equal local-name truename) truename))
         (truename-nondir (when truename (file-name-nondirectory truename)))
         (truename-nondir (unless (equal truename-nondir local-name-nondir)
                            truename))
         ;; When there are "./" or "../" in the input field, given the
         ;; restrictions of readtags filter expressions, it's hard to match it
         ;; with the "true" filename.  When this happens, we match against the
         ;; non directory part.
         (implied-relative-path-regex
          (lambda (file-non-dir-part)
            (concat "(^|/)..?/"
                    (citre-core-regexp-quote file-non-dir-part)
                    "$")))
         (filter (list 'or))
         (info (citre-core-tags-file-info tagsfile))
         (cwd (file-local-name (gethash 'dir info)))
         (os (gethash 'os info)))
    ;; Ctags on windows generates directory symbol in capital letter, while
    ;; `buffer-file-name' returns it in small letter.  We don't use
    ;; `system-type' to detect since we may work on a remote Unix machine on
    ;; Windows.  We don't need the same treatment for cwd as it uses capital
    ;; disk symbols on Windows, see `citre-core--get-dir'.
    (when (eq os 'nt)
      (setq local-name (citre-upcase-first-letter local-name))
      (when truename
        (setq truename (citre-upcase-first-letter truename))))
    (dolist (f (list local-name truename))
      (when f
        (push (citre-core-filter 'input f 'eq) filter)
        (when (and (string-prefix-p cwd f))
          ;; We don't use `file-relative-name' due to the same reason.  Its
          ;; behavior depends on the platform.
          (push (citre-core-filter 'input (substring f (length cwd)) 'eq)
                filter))))
    (dolist (f (list local-name-nondir truename-nondir))
      (when f
        (push (citre-core-filter 'input (funcall implied-relative-path-regex f)
                                 'regexp)
              filter)))
    (nreverse filter)))

;;;;; Build sorter expressions

;;;;;; Internals

(defun citre-core--readtags-expr-replace-$-by-& (expr)
  "Replace $-entry by &-entry in EXPR.
EXPR is a filter expression."
  (if (consp expr)
      (cons (citre-core--readtags-expr-replace-$-by-& (car expr))
            (citre-core--readtags-expr-replace-$-by-& (cdr expr)))
    (if (and (symbolp expr) (eq (aref (symbol-name expr) 0) ?$))
        (intern (concat "&" (substring (symbol-name expr) 1)))
      expr)))

(defun citre-core--simple-sorter (elt)
  "Build a sorter based on ELT.
ELT is an element of the FIELDS arg in `citre-core-sorter', and
is one of the \"OPERATOR\" or \"field\" variant."
  (let* ((variant (car elt))
         (field (nth 1 elt))
         (entry (lambda (prefix)
                  (intern (concat prefix (symbol-name field)))))
         ($-entry (funcall entry "$"))
         (&-entry (funcall entry "&"))
         (opd (lambda (entry)
                (pcase variant
                  ('field entry)
                  (_ `(,(car elt) ,entry)))))
         ($-opd (funcall opd $-entry))
         (&-opd (funcall opd &-entry)))
    `(if (and ,$-entry ,&-entry)
         ,(pcase (nth 2 elt)
            ('+ `(<> ,$-opd ,&-opd))
            ('- `(<> ,&-opd ,$-opd))
            (_ (error "Invalid element: %s" elt)))
       ;; For tags without the specified field, the order is uncertain.
       0)))

(defun citre-core--filter-sorter (elt)
  "Build a sorter based on ELT.
ELT is an element of the FIELDS arg in `citre-core-sorter', and
is the \"filter\" variant."
  (let* (($-filter (nth 1 elt))
         (&-filter (citre-core--readtags-expr-replace-$-by-& $-filter))
         (vals (pcase (nth 2 elt)
                 ('+ '(-1 1))
                 ('- '(1 -1))
                 (_ (error "Invalid element: %s" elt)))))
    `(<> (if ,$-filter ,@vals)
         (if ,&-filter ,@vals))))

;;;;;; The API

(defun citre-core-sorter (&rest args)
  "Return a sorter expression based on ARGS.
The return value can be used as the :sorter argument in
`citre-core-get-tags'.

Each element of FIELDS can be:

- A symbol.  For example, `input' means sort based on the input
  field, in ascending order.

- A list

      (field SYMBOL +/-)

  For example, `(field line +)' means sorting based on the line
  field, in ascending order, and `(field line -)' means in
  descending order.

- A list

      (OPERATOR SYMBOL +/-)

  For example, `(length name +)' means sorting based on the
  lengths of the tag names, in ascending order, and `(length name
  -)' means in the descending order.

- A list

      (filter FILTER-EXPR +/-)

  For example, `(filter (eq? $kind \"file\") +)' means puting
  tags with \"file\" kind above others, and `(filter (eq? $kind
  \"file\") -)' means putting them below others.

In readtags, if you sort directly based on a field that's missing
in some lines, it will throw an error.  Here, all above variants
except the \"filter\" one are processed so that this won't
happen, and the order of tags involving missing fields is
uncertain.  For \"filter\" variant, it's recommended to build the
filter expression using Citre APIs to make sure it can be evaled
on each tag.  You can think that tags that are keeped by
`citre-core-filter' are put above/below others.

When multiple elements are presented in FIELDS, they are tried in
order, until the order is decided.  For example,

  (citre-core-build-sorter input \\='(line -))

means sort by the file name, then the line number (in descending
order) if they are in the same file."
  (let* ((sorter (list '<or>)))
    (dolist (arg args)
      (when (symbolp arg)
        (setq arg `(field ,arg +)))
      (push
       (pcase (car arg)
         ('field (citre-core--simple-sorter arg))
         ('filter (citre-core--filter-sorter arg))
         (_ (citre-core--simple-sorter arg)))
       sorter))
    (nreverse sorter)))

;;;;; Get tags from tags files

(defun citre-core-get-pseudo-tags (name tagsfile &optional prefix)
  "Read pseudo tags matching NAME in tags file TAGSFILE.
TAGSFILE is the absolute path of the tags file.  When PREFIX is
non-nil, match NAME by prefix.

NAME should not start with \"!_\".  Run

  $ ctags --list-pseudo-tags

to know the valid NAMEs.  The return value is a list, and each
element of it is another list consists of the fields separated by
tabs in a pseudo tagline."
  (citre-core--error-on-arg name #'citre-core--string-non-empty-p)
  (citre-core--error-on-arg tagsfile #'citre-core--string-non-empty-p)
  (let* ((tagsfile (expand-file-name tagsfile))
         (name (concat "!_" name))
         (result (citre-core--get-lines
                  tagsfile nil nil nil
                  (citre-core-filter 'name name (if prefix 'prefix 'eq))
                  nil "-D")))
    (mapcar (lambda (line)
              (split-string line "\t" t))
            result)))

(cl-defun citre-core-get-tags
    (tagsfile &optional name match case-fold
              &key filter sorter
              require optional exclude parse-all-fields)
  "Get tags in TAGSFILE.
TAGSFILE is the absolute path of tags file.  The meaning of the
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
  field in it, if it's presented in a tagline, it will be
  recorded; if not presented, it's ignored, and no error will
  occur.
- EXCLUDE: A list containing fields that should be excluded.  All
  other fields will be recorded.

OPTIONAL and EXCLUDE should not be used together.  When both
OPTIONAL and EXCLUDE are not presented, then only the fields in
REQUIRE are parsed, unless PARSE-ALL-FIELDS is non-nil.

Fields should be symbols.  Please notice these field names:

- `name': The name of the tag itself.
- `input': The file containing the tag.
- `pattern': EX command used to search the tag in the file.

If the tags file is not generated with --fields=+Z, then a
`scope' field like \"scope:class:A\" becomes \"class:A\".  If
this happens, there's no reliable way to know such a field is
actually a `scope' field.  Currently, when the scope is class or
struct, Citre parses them correctly.  Otherwise the scope name is
used as the field name.

Certain fields may offer ambiguous information.  To ascertain
them, Citre offers its own extension fields:

- `ext-abspath': The full path of `input'.

  Needs `input' field to be presented in the tags file.  When the
  tags file is a remote file, this is also a remote file name.

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
field, it must appear in REQUIRE or OPTIONAL."
  (let ((nil-or-string-p (lambda (x) (or (null x) (stringp x)))))
    (citre-core--error-on-arg tagsfile #'citre-core--string-non-empty-p)
    (citre-core--error-on-arg name nil-or-string-p))
  (let* ((tagsfile (expand-file-name tagsfile))
         (name- (when (memq match '(nil exact prefix)) name))
         (match- (when (memq match '(nil exact prefix)) match))
         (filter- (when (and name (memq match '(suffix substr regexp)))
                    (citre-core-filter 'name name match case-fold)))
         (filter- (if (and filter- filter)
                      `(and ,filter- ,filter)
                    (or filter- filter))))
    (citre-core--get-tags tagsfile name- match- case-fold
                          :filter filter- :sorter sorter
                          :require require :optional optional
                          :exclude exclude
                          :parse-all-fields parse-all-fields)))

;;;;; Get fields from tags

(defun citre-core-get-field (field tag &optional after-colon)
  "Get FIELD from TAG.
TAG is an element in the return value of `citre-core-get-tags'.
FIELD can be all normal and extension fields.

When FIELD is `line' or `end', an integer is returned, instead of
a string.

When AFTER-COLON is non-nil, the field is splitted at the first
colon, and the part after it is returned.  This is for fields
like `scope' or `typeref'.  In a tagline they may look like this:

    typeref:struct:structName

when getting the `typeref' field from this tag, a non-nil
AFTER-COLON gives \"structName\", while nil gives
\"struct:structName\".

If you use this option on a field that doesn't contain a colon,
the whole field is returned.

`citre-core-extra-ext-fields-table' defines some extra fields
that can be used as FIELD.  Their values are calculated in
real-time based on TAG.  The built-in ones are:

- `extra-line': The line number of the tag.  This uses the `line'
  field directly, and if it's not presented, get the line number
  from the pattern field if it's recorded there.  If both can't
  be done, return nil.

- `extra-lang': The language.  It uses the `language' field if
  it's presented, or guesses the language by the file extension.
  When both fails, the file extension (or the file name, if it
  doesn't have an extension) is returned.

- `extra-matched-str': The substring in the source file that's
  matched by ctags when generating the tag.  It's often the whole
  line containing the tag.  This depends on the `pattern' field,
  and returns nil if it doesn't record the matched string (e.g.,
  in tags file generated using the -n option)."
  (let ((maybe-split (if after-colon
                         #'citre-string-after-1st-colon
                       #'identity))
        value)
    (if-let ((method (gethash field citre-core-extra-ext-fields-table)))
        (setq value (funcall method tag))
      (setq value (gethash field tag))
      (when value
        (pcase field
          ((or 'line 'end) (when-let ((val (gethash field tag)))
                             (string-to-number val)))
          (_ (funcall maybe-split (gethash field tag))))))))

;;;;; Helper for finding the location of a tag

;;;;;; Internals

(defvar citre-core--pattern-search-limit 50000
  "The limit of chars to go when searching for a pattern.")

(defun citre-core--split-pattern (pattern)
  "Split the pattern PATTERN.
PATTERN should be the pattern field in a tag.  It returns (LINUM
PAT) where:

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
pattern field of a tagline.

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
              (citre-string-match-all-escaping-backslash pattern)))
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

(defun citre-core-locate-tag (tag &optional use-linum)
  "Find TAG in current buffer.
Returns the position to goto, or line number if USE-LINUM is
non-nil.  Current buffer should be the buffer visiting the file
containing the tag.

The search is helped by:

- The pattern field.
- The line field, if the pattern is not a combined
  pattern (i.e., not contatining the line number).
- The name of the tag.

This function does its best to find the tag if the file has been
changed, and even when the line including the tag itself has been
changed.  See the code for details.  If the search fails
completely, it will return the beginning position of the file.

This function has no side-effect on the buffer.  Upper components
could wrap this function to provide a desired UI for jumping to
the position of a tag."
  (pcase-let*
      ((name (citre-core-get-field 'name tag))
       (pat (citre-core-get-field 'pattern tag))
       (`(,line ,pat) (when pat (citre-core--split-pattern pat)))
       (line (or (citre-core-get-field 'line tag) line))
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
        (or
         (when pat
           (or
            ;; Search for the whole line.
            (citre-core--find-nearest-regexp
             (concat pat-beg (regexp-quote str) pat-end)
             lim)
            ;; Maybe the indentation or trailing whitespaces has changed, or
            ;; something is added after.  From now on we also use case-fold
            ;; search to deal with projects that uses a case-insensitive
            ;; language and don't have a consistant style on it.
            (citre-core--find-nearest-regexp
             (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
             lim 'case-fold)
            ;; The content is changed.  Try cutting from the end of the tag
            ;; name and search.
            (when-let ((name name)
                       (bound (when (let ((case-fold-search nil))
                                      (string-match (regexp-quote name) str))
                                (match-end 0)))
                       (str (substring str 0 bound)))
              (citre-core--find-nearest-regexp
               (concat pat-beg "[ \t]*" (regexp-quote (string-trim str)))
               lim 'case-fold))))
         ;; Last try: search for the tag name.
         (when name
           (or
            (citre-core--find-nearest-regexp (concat "\\_<"
                                                     (regexp-quote name)
                                                     "\\_>")
                                             lim 'case-fold)
            (citre-core--find-nearest-regexp (regexp-quote name)
                                             lim 'case-fold))))
        (if use-linum (line-number-at-pos) (point))))))

;;;;; Edit pseudo tags

;; NOTE: Editing tags files may be implemented by an "edittags" program in
;; Universal Ctags in the future.  After that, we should reimplement this as an
;; edittags abstraction layer.

;; NOTE: I suspect `write-region' will return befure it finishes the writing.
;; If you pack `citre-core-write-pseudo-tag' and any operation that calls
;; readtags (like `citre-core-get-tags') into a `progn' form, there are chances
;; that readtags process freezes.  For now upper components uses `sit-for'
;; between them to workaround this bug.  I think it can be solved after we
;; switch to edittags.

(defun citre-core-write-pseudo-tag (tagsfile name value comment)
  "Write a pseudo tag to TAGSFILE.
TAGSFILE is the absolute path of the tags file.  NAME is the name
of the pseudo tags, without the beginning \"!_\".  VALUE is its
value, and COMMENT is its comment info.

When there's already a pseudo tag with the same name, COMMENT
will not overwrite the original comment."
  (setq comment (concat "/" comment "/;\""))
  (with-temp-file tagsfile
    (insert-file-contents tagsfile)
    ;; Jump over all pseudo tags.
    (while (eq (char-after) ?!)
      (forward-line))
    ;; Record the point position, and don't search beyond it later.
    (let ((end (point)))
      (goto-char 0)
      (if (search-forward (concat "!_" name) end 'noerror)
          (progn
            (dotimes (_ 2)
              (search-forward "\t"))
            (setq comment (buffer-substring (point) (line-end-position)))
            (delete-region (line-beginning-position) (line-end-position))
            (unless (eobp)
              ;; Delete the newline character.
              (delete-char 1)))
        (goto-char 0))
      (insert "!_" name "\t" value "\t" comment "\n"))))

(provide 'citre-core)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-core.el ends here
