;;; citre-util.el --- Utilities for tools in Citre -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
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

(require 'citre-core)
(require 'cl-lib)
(require 'subr-x)

;;;; User options

;;;;; Options: Project related

(defcustom citre-project-denoter-files
  '(".citre" ".projectile" ".dumbjump")
  "List of project denoter files.
If the project root detection fails, put a file with one of these
names in your project root.  This list is in descending order of
priority (i.e., if we find one, then the rest will be ignored).

See `citre-project-root' to know how Citre detects the project
root."
  :type '(repeat string)
  :group 'citre)

(defcustom citre-project-fallback-denoter-files
  '("Makefile" "makefile" "tags" ".tags")
  "List of project denoter files used as fallbacks.
These are files that may appear in some parent directory of the
project, thus they are used only when normal detection methods
fail.  This list is in descending order of priority (i.e., if we
find one, then the rest will be ignored).

See `citre-project-root' to know how Citre detects the project
root."
  :type '(repeat string)
  :group 'citre)

(defcustom citre-project-root nil
  "Absolute path of project root directory.
Set this in your .dir-locals.el if the project root detection
fails, and for some reason you can't put a denoter file in the
project root (see `citre-project-denoter-files').

If you don't set this manually, Citre will detect the project
root and set it automatically.  See `citre-project-root' to know
how this is done."
  :type '(choice (const nil) string)
  :group 'citre)

(make-variable-buffer-local 'citre-project-root)

(defcustom citre-tags-files '(".tags" "tags")
  "List of tags file paths.
Relative paths to the project root or absolute paths can both be
used as elements.  This list is in descending order of
priority (i.e., if we find one, then the rest will be ignored)."
  :type '(repeat string)
  :group 'citre)

(make-variable-buffer-local 'citre-tags-files)

;;;;; Options: Behavior of Citre

(defcustom citre-case-sensitivity 'smart
  "Case sensitivity of auto-completion.  Can be:

- `sensitive': Always do case sensitive completion.
- `insensitive': Always do case insensitive completion.
- `smart': Be sensive when completing a symbol with uppercase
  letters or underscores, otherwise be insensitive.

Note for developers: Actually this doesn't affect auto-completion
directly.  This option controls the behavior of
`citre-get-records' when its argument MATCH is not nil or
`exact', and when this is the case, it's likely that the user is
getting records for auto-completion."
  :type '(choice (const :tag "Sensitive" sensitive)
                 (const :tag "Insensitive" insensitive)
                 (const :tag "Smart" smart))
  :group 'citre)

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after jumping to a location."
  :type 'hook
  :group 'citre)

;;;; Core API wrapper

(cl-defun citre-get-records
    (&optional tagsfile name match
               &key filter sorter
               require optional exclude parse-all-fields lines)
  "Get records of tags in tags file TAGSFILE that match NAME.
This is like `citre-core-get-records', except that:

- TAGSFILE could be nil, and it will be find automatically under
  current project root.
- When MATCH is nil or `exact', CASE-FOLD is always nil,
  otherwise it's decided by `citre-case-sensitivity' and NAME.

TAGSFILE is the canonical path of the tags file.  For FILTER,
SORTER, REQUIRE, OPTIONAL, EXCLUDE, PARSE-ALL-FIELDS and LINES,
see `citre-core-get-records'.

Each element in the returned value is a list containing the tag
and some of its fields, which can be utilized by
`citre-core-get-field'."
  (let* ((tagsfile- (or tagsfile (citre-tags-file-path)))
         (case-fold- (pcase citre-case-sensitivity
                       ('sensitive nil)
                       ('insensitive t)
                       ('smart (if (memq match '(nil exact))
                                   nil
                                 (if (and name
                                          (or (string= (downcase name) name)
                                              (string-match "_" name)))
                                     t nil))))))
    (citre-core-get-records tagsfile- name match case-fold-
                            :filter filter :sorter sorter
                            :require require :optional optional
                            :exclude exclude
                            :parse-all-fields parse-all-fields
                            :lines lines)))

;;;; Helpers

(defun citre--find-dir-with-denoters (file denoters)
  "Search up directory hierarchy from FILE for a denoter file.
DENOTERS is a list of denoter files, in the order of
precedence (i.e., if we find one, then the rest will be ignored).
The directory containing the denoter file will be returned.  If
such directory doesn't exist, nil will be returned."
  (cl-dolist (denoter denoters)
    (let ((dir (locate-dominating-file file denoter)))
      (when dir
        (cl-return (file-name-directory (expand-file-name dir)))))))

;;;; APIs

;;;;; APIs: Project related

(defun citre-project-root (&optional buffer)
  "Find the project root of current file.
The following methods are tried in turn, and the first succeeded
one determines the project root:

- Return `citre-project-root' directly if it's set.
- Search up directory hierarchy for a file in
  `citre-project-denoter-files'.
- Use `project-current'.  Currently it only deals with projects
  under version control.
- Search up directory hierarchy for a file in
  `citre-project-fallback-denoter-files'.
- Use the directory of current file.

After the project root is found, `citre-project-root' in current
buffer is set.  When BUFFER is non-nil, find project root for the
file in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (or citre-project-root
        (when-let* ((file (or (buffer-file-name)
                              ;; Support non-file buffers, e.g., Dired.
                              (expand-file-name default-directory)))
                    (dir (file-name-directory file)))
          (setq citre-project-root
                (or (citre--find-dir-with-denoters
                     file citre-project-denoter-files)
                    (when-let ((project (project-current nil dir)))
                      (expand-file-name (cdr project)))
                    (citre--find-dir-with-denoters
                     file citre-project-fallback-denoter-files)
                    dir))))))

(defun citre-relative-path (path &optional project)
  "Return PATH but relative to current project root.
If PATH is not under the project, it's directly returned.  If
project root PROJECT is specified, use that project instead."
  (let* ((project (when project (expand-file-name project)))
         (project (or project (citre-project-root)))
         (path (expand-file-name path)))
    (if (and project (string-prefix-p project path))
        (file-relative-name path project)
      path)))

(defun citre-tags-file-path (&optional project)
  "Find tags file in PROJECT and return its path.
If PROJECT is not specified, use current project in buffer.  This
looks up `citre-tags-files' to find the tags file needed, and
throws an user error if no tags file was found."
  (or (cl-some
       (lambda (file)
         (let ((tags-file (expand-file-name
                           file
                           (or project (citre-project-root)))))
           (when (file-exists-p tags-file) tags-file)))
       citre-tags-files)
      (user-error "Tags file not found")))

;;;;; APIs: Language support framework

(defvar-local citre--buffer-file-name nil
  "File name in non-file buffers.
If a tool needs to open a file in a non-file buffer (like
`citre-peek'), set this variable in that buffer, then the
`buffer-file-name' function inside `citre-get-symbol' call could
return the file name.  Simply puts it, the `:get-symbol'
function (see `citre-language-support-alist') can use
`buffer-file-name' function normally and it works for peek
sessions.")

;;;;;; The lookup table

(defvar citre-language-support-alist nil
  "The lookup table for language-specific support.

A key of it is the language's major mode (a symbol).

A value of it is a plist.  Its props and values are:

- `:get-symbol': The function to get the symbol at point.

  It's a function with no arguments.  The returned value is a
  string of the symbol name.  To support auto-completion, Citre
  requires a `citre-bounds' property, which is a cons pair of the
  beginning/end positions of the symbol.

  You can use other properties to record the information you need
  for filtering/sorting the tags, see the props below.
  `citre-file-path' for the canonical path of current file, and
  `citre-tags-file' for the canonical path of tags file are
  automatically attached to the returned value, so
  filters/sorters can make use of them.

  If you don't specify this prop, `citre-get-symbol-default' is
  used as fallback.  You can also use it internally, and add more
  properties you need.

  When there's an inactive region, it's recommended to get the
  text in it as a symbol, so when your function doesn't work well
  for the user, they can manually specify which part to get.
  `citre-get-marked-symbol' implements this, and is also used by
  `citre-get-symbol'.

- `:completion-filter': The filter for auto-completion.

  It can be a filter expression, a symbol whose value is a filter
  expression, or a function that takes the string returned by the
  `:get-symbol' function, and returns the filter expression.  The
  fallback is `citre-completion-default-filter'.

- `:completion-sorter': The sorter for auto-completion.

  It can be a sorter expression, a symbol whose value is a sorter
  expression, or a function that takes the string returned by the
  `:get-symbol' function, and returns the sorter expression.The
  fallback is `citre-completion-default-sorter'.

- `:definition-filter' and `:definition-sorter': The same as
  `:completion-filter' and `:completion-sorter', but used for
  finding definitions.  Their fallback values are
  `citre-definition-default-filter' and
  `citre-definition-default-sorter'.

The filter/sorter functions should be pure, i.e., should only use
information provided by the symbol, and not fetch information
from the environment.")

(defun citre--get-value-in-language-alist (prop &optional symbol)
  "A helper for lookup PROP in `citre-language-support-alist'.
Returns the value in it for the language in current buffer, and
PROP.

If SYMBOL is non-nil, and the value we get is a function, call
the function on SYMBOL and return its value."
  (when-let ((value (plist-get (alist-get major-mode
                                          citre-language-support-alist)
                               prop)))
    (cond
     ((and (symbolp value) (boundp value))
      (symbol-value value))
     ((and symbol (functionp value))
      (funcall value symbol))
     (t value))))

;;;;;; Get symbol at point

(defun citre-get-marked-symbol ()
  "Get the text in activate region as a symbol."
  (when (use-region-p)
    (let ((bounds (cons (region-beginning) (region-end))))
      (citre-put-property
       (buffer-substring-no-properties (car bounds) (cdr bounds))
       'bounds bounds))))

(defun citre-get-symbol-at-point ()
  "Get the symbol at point."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (citre-put-property
     (buffer-substring-no-properties (car bounds) (cdr bounds))
     'bounds bounds)))

(defun citre-get-symbol-default ()
  "Get the symbol at point.
If there's an active region, the text in it is returned as a
symbol.  Otherwise, the symbol at point is returned.  If both
fails, nil is returned.

The returned symbol is a string with a `citre-bounds' property,
recording the beginning/end positions of the symbol."
  (or (citre-get-marked-symbol)
      (citre-get-symbol-at-point)))

(defun citre-get-symbol ()
  "Get the symbol at point.
Set `citre-get-symbol-function-alist' to control the behavior of
this function for different languages.  `citre-file-path' and
`citre-tags-file' properties are attached to the symbol string so
filters/sorters can make use of them."
  ;; HACK: make `buffer-file-name' work in temp buffers opened by a peek
  ;; session.  This is unavoidable, since even we could use something like:
  ;;
  ;;   (or citre--buffer-file-name (buffer-file-name))
  ;;
  ;; when calculating the `citre-file-path' property, language-specific
  ;; `:get-symbol' function may still call `buffer-file-name' for its own use.
  (cl-letf* ((buffer-file-name-orig (symbol-function 'buffer-file-name))
             ((symbol-function 'buffer-file-name)
              (lambda (&optional buffer)
                (or (with-current-buffer (or buffer (current-buffer))
                      citre--buffer-file-name)
                    (funcall buffer-file-name-orig buffer)))))
    (let ((sym (funcall (or (citre--get-value-in-language-alist :get-symbol)
                            #'citre-get-symbol-default))))
      (citre-put-property sym 'file-path (buffer-file-name))
      (citre-put-property sym 'tags-file (citre-tags-file-path))
      sym)))

;;;;; APIs: Auto-completion related

;; TODO: A better filter
(defun citre-completion-default-filter (symbol)
  "Default completion filter for SYMBOL."
  (let ((tags-file (citre-get-property symbol 'tags-file))
        (file-path (citre-get-property symbol 'file-path)))
    `(not
      (or
       ,(citre-core-build-filter 'extras "anonymous" 'member)
       ,(citre-core-build-filter 'extras "reference" 'member)
       ;; For tags file using single-letter kind, apply `not' to
       ;; `citre-core-filter-kind' may exclude more tags than it should.
       ;; But we know the "F" (file) kind is preserved by ctags, and "F" is
       ;; not used anywhere else, so we could do this.
       ,(citre-core-filter-kind "file" tags-file)
       ;; Exclude tags that have "file" scope, and is not in this file.
       (and (not ,(citre-core-filter-input file-path tags-file))
            (or ,(citre-core-filter-field-exist 'file)
                ,(citre-core-build-filter 'extras "fileScope" 'member)))))))

(defvar citre-completion-default-sorter
  (citre-core-build-sorter
   '(length name +) 'name)
  "The default sorter expression for auto-completion.
This sorts the candidates by their length, then the alphabetical
order of their name.")

(defun citre-get-completions (&optional symbol tagsfile substr-completion)
  "Get completions from TAGSFILE of symbol at point.
If SYMBOL is non-nil, use that symbol instead.  If TAGSFILE is
not specified, fint it automatically under current project root.
If SUBSTR-COMPLETION is non-nil, get tags that contains SYMBOL,
or get tags that starts with SYMBOL.  The case sensitivity is
controlled by `citre-case-sensitivity'.

The result is a list of strings, each string is a tag name, with
its text property `citre-kind' and `citre-signature' being the
kind and signature of the tag.

It returns nil when the completion can't be done."
  (let ((symbol (or symbol (citre-get-symbol)))
        (tagsfile (or tagsfile (citre-tags-file-path)))
        (match (if substr-completion 'substr 'prefix)))
    (citre-get-records tagsfile symbol match
                       :filter (or (citre--get-value-in-language-alist
                                    :completion-filter symbol)
                                   (citre-completion-default-filter symbol))
                       :sorter (or (citre--get-value-in-language-alist
                                    :completion-sorter symbol)
                                   citre-completion-default-sorter)
                       :require '(name)
                       :optional '(ext-kind-full signature typeref))))

;; TODO: Is it better to define these in the util layer or in specific tools?
(defun citre-make-completion-str (record)
  "Generate a string for RECORD for displaying.
RECORD should be an element in the returned value of
`citre-get-completions'.  The string returned is the tag name of
it, with RECORD stored in its property `citre-record'.

This is for showing the results for auto-completion tools."
  (citre-propertize
   (citre-core-get-field 'name record)
   record))

;;;;; APIs: Finding definitions

(defun citre-definition-default-filter (symbol)
  "Default definition filter for SYMBOL."
  (let ((tags-file (citre-get-property symbol 'tags-file))
        (file-path (citre-get-property symbol 'file-path)))
    `(not
      (or
       ,(citre-core-build-filter 'extras "anonymous" 'member)
       ,(citre-core-filter-kind "file" tags-file)
       ;; Exclude tags that have "file" scope, and is not in this file.
       (and (not ,(citre-core-filter-input file-path tags-file))
            (or ,(citre-core-filter-field-exist 'file)
                ,(citre-core-build-filter 'extras "fileScope" 'member)))))))

(defvar citre-definition-default-sorter
  (citre-core-build-sorter
   `(filter ,(citre-core-build-filter 'extras "reference" 'member) -)
   'input '(length name +) 'name)
  "The default sorter expression for finding definitions.
This sorts the file name by their alphabetical order, then the
length and alphabetical order of the tag names.")

(defun citre-get-definitions (&optional tagsfile symbol)
  "Get definitions from tags file TAGSFILE of symbol at point.
If SYMBOL is non-nil, use that symbol instead.  If TAGSFILE is
not specified, find it automatically under current project root.

The result is a list of records, with the fields `ext-abspath',
`line' and `kind'."
  (let ((symbol (or symbol (citre-get-symbol)))
        (tagsfile (or tagsfile (citre-tags-file-path))))
    (unless symbol
      (user-error "No symbol at point"))
    (citre-get-records tagsfile symbol 'exact
                       :filter (or (citre--get-value-in-language-alist
                                    :definition-filter symbol)
                                   (citre-definition-default-filter symbol))
                       :sorter (or (citre--get-value-in-language-alist
                                    :definition-sorter symbol)
                                   citre-definition-default-sorter)
                       :require '(name ext-abspath pattern)
                       :optional '(ext-kind-full line typeref))))

;; TODO: annotate reference tags
(defun citre-make-location-str (record)
  "Generate a string for RECORD for displaying.
RECORD should be an element in the returned value of
`citre-get-definitions'.  The string returned looks like
\"file(line-number): content\", with RECORD stored in its
property `citre-record'.

This is for showing the results for \"finding definition\"
tools."
  (let* ((line (citre-core-get-field 'extra-line record))
         (line (if line
                   (concat "("
                           (propertize (number-to-string line) 'face 'warning)
                           ")")
                 ""))
         (str (citre-core-get-field 'extra-matched-str record))
         (str (if str
                  (concat ": " (string-trim str))
                ""))
         (kind (citre-core-get-field 'ext-kind-full record))
         (type (citre-core-get-field 'typeref record 'after-colon))
         (annotation (propertize (concat
                                  (or kind "")
                                  (if (and kind type) "/" "")
                                  (or type "")
                                  (if (or kind type) " " ""))
                                 'face 'citre-definition-annotation-face))
         (abspath (citre-core-get-field 'ext-abspath record))
         (path (propertize (citre-relative-path abspath)
                           'face 'font-lock-function-name-face))
         (file-missing-p (if (file-exists-p abspath) "" "*")))
    (citre-propertize (concat annotation file-missing-p path line str)
                      record)))

(defun citre-goto-tag (record &optional window)
  "Jump to the location of tag RECORD.
WINDOW can be:

- nil: Use current window.
- `other-window': Use other window.
- `other-window-noselect': Use other window but don't select it."
  ;; TODO: I actually don't know well about this whole display-buffer,
  ;; pop-to-buffer and switch-to-buffer thing.  Will come back and see if this
  ;; docstring describes the behavior well.
  (let ((path (citre-core-get-field 'ext-abspath record)))
    (unless path
      (error "RECORD doesn't have the ext-abspath field"))
    (unless (file-exists-p path)
      (user-error "File %s doesn't exist" path))
    (let* ((buf (find-file-noselect path))
           (current-buf (current-buffer)))
      (if window
          (pop-to-buffer buf)
        (switch-to-buffer buf))
      (goto-char (citre-core-locate-tag record))
      (run-hooks 'citre-after-jump-hook)
      (when (eq window 'other-window-noselect)
        (pop-to-buffer current-buf)))))

(defun citre-recenter-and-blink ()
  "Recenter point and blink after point.
This is suitable to run after jumping to a location."
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

;;;;; APIs: Text property related

;; TODO: I don't know if these (destructive) functions are gonna cause troubles
;; if they are used to propertize strings in a record, and we use the record
;; later to do something, but Citre doesn't do that.  If we can confirm this,
;; we'll add (or not add) a warning for developers.
(defun citre-propertize (str record &rest fields)
  "Propertize STR by FIELDS in RECORD.
Added text properties are prefixed by \"citre-\".  For example,
the `kind' field will be stored in the `citre-kind' property.

When FIELDS are nil, the whole record is stored in the
`citre-record' property.

Notice that this is destructive, which is different from
`propertize'.  The propertized STR is returned."
  (let ((len (length str)))
    (if fields
        (dolist (field fields)
          (put-text-property 0 len
                             (intern (concat "citre-" (symbol-name field)))
                             (citre-core-get-field field record)
                             str))
      (put-text-property 0 len 'citre-record record str))
    str))

(defun citre-get-property (str &optional field from-record)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `citre-propertize' or
`citre-put-property'.

When FIELD is non-nil and FROM-RECORD is nil, what it actually
does is prefix FIELD by `citre-', and get that text property.

When FIELD and FROM-RECORD are both non-nil, it gets the record
first, then get FIELD from it using `citre-core-get-field'.

When FIELD is nil and FROM-RECORD is non-nil, it gets the record
from STR, stored in the `citre-record' text property."
  (cond
   ((and field (null from-record))
    (get-text-property 0 (intern (concat "citre-" (symbol-name field))) str))
   ((and (null field) from-record)
    (get-text-property 0 'citre-record str))
   ((and field from-record)
    (citre-core-get-field field (get-text-property 0 'citre-record str)))
   (t
    (error "Invalid combination of FIELD and FROM-RECORD"))))

(defun citre-put-property (str prop val)
  "Set the text property corresponding to PROP in STR.
The value is specified by VAL.  The text property added is
prefixed by \"citre-\".  Propertized STR is returned."
  (put-text-property 0 (length str)
                     (intern (concat "citre-" (symbol-name prop)))
                     val str)
  str)

(provide 'citre-util)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-util.el ends here
