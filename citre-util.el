;;; citre-util.el --- Utilities for tools in Citre -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0
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

;; The basis of user tools offered by Citre.  See
;; docs/developer-manual/project-structure.md.

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

;;;;; Options: Finding tags file

(defcustom citre-tags-files '(".tags" "tags")
  "List of tags files.
These are searched up directory hierarchy from the file of
current buffer, or from `default-directory' in current buffer, to
decide which tags file to use.

This list is in descending order of priority (i.e., if we find
one, then the rest will be ignored)."
  :type '(repeat string)
  :group 'citre)

(defcustom citre-tags-file-alist nil
  "Alist of directory -> tags file.
If current file in buffer is in one of the directories, the
corresponding tags file will be used.

This is a buffer-local variable so you can customize it on a
per-project basis.  Relative paths in it will be expanded against
the project root, which is detected by
`citre-project-root-function'."
  :type '(alist :key-type string :value-type string)
  :group 'citre)

;;;###autoload
(put 'citre-tags-file-alist 'safe-local-variable #'listp)
(make-variable-buffer-local 'citre-tags-file-alist)

(defcustom citre-project-root-function #'citre-project-root
  "A function that returns project root in current buffer.
It takes no arguments.  It's used for:

- Displaying the path of a tag relatively.
- Expanding relative paths in `citre-tags-file-alist'."
  :type 'function
  :group 'citre)

;;;;; Options: Behavior of Citre

(defcustom citre-completion-case-sensitive t
  "Case sensitivity of auto-completion.

Note for developers: Actually this doesn't affect auto-completion
directly.  This option controls the behavior of `citre-get-tags'
when its argument MATCH is not nil or `exact', and when this is
the case, it's likely that the user is getting tags for
auto-completion."
  :type 'boolean
  :group 'citre)

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after jumping to a location."
  :type 'hook
  :group 'citre)

;;;;; Options: Appearance

(defface citre-definition-annotation-face
  '((((background light))
     :foreground "#666666" :slant italic)
    (t
     :foreground "#c0c0c0" :slant italic))
  "Face used for annotations when presenting a definition.
Annotations include kind, type, etc."
  :group 'citre)

(defcustom citre-definition-annotation-separator
  (propertize "/" 'face 'citre-definition-annotation-face)
  "The separator between kind and type in annotation."
  :type 'string
  :group 'citre)

(defcustom citre-definition-reference-mark
  (propertize "<R>" 'face 'citre-definition-annotation-face)
  "Mark added for references in definitions."
  :type 'string
  :group 'citre)

(defface citre-definition-path-face
  '((t :inherit font-lock-function-name-face))
  "Face used for the path in a definition."
  :group 'citre)

(defcustom citre-definition-missing-file-mark
  (propertize "!" 'face 'warning)
  "Mark added before missing files in definitions."
  :type 'string
  :group 'citre)

;;;; Core API wrapper

(cl-defun citre-get-tags
    (&optional tagsfile name match
               &key filter sorter
               require optional exclude parse-all-fields)
  "Get tags in tags file TAGSFILE that match NAME.
This is like `citre-core-get-tags', except that:

- TAGSFILE could be nil, and it will be find automatically.
- When MATCH is nil or `exact', CASE-FOLD is always nil,
  otherwise it's decided by `citre-completion-case-sensitive' and
  NAME.

TAGSFILE is the canonical path of the tags file.  For FILTER,
SORTER, REQUIRE, OPTIONAL, EXCLUDE, and PARSE-ALL-FIELDS, see
`citre-core-get-tags'.

Each element in the returned value is a list containing the tag
and some of its fields, which can be utilized by
`citre-core-get-field'."
  (citre-core-get-tags (or tagsfile (citre-tags-file-path)) name match
                       (not citre-completion-case-sensitive)
                       :filter filter :sorter sorter
                       :require require :optional optional
                       :exclude exclude
                       :parse-all-fields parse-all-fields))

;;;; Helpers

(defun citre-project-root ()
  "Caninical path of project root of current buffer.
This uses `project-current' internally."
  (when-let ((project (project-current nil)))
    (expand-file-name (cdr project))))

;;;; APIs

;;;;; APIs: Find tags file

(defvar-local citre--tags-file nil
  "Buffer-local cache for tags file path.")

(defun citre-tags-file-path ()
  "Return the canonical path of tags file for current buffer.
This looks up `citre-tags-files' to find the tags file needed,
and throws an user error if no tags file was found.

It also sets `citre-core--tags-file-cwd-guess-table', so for tags
file without the TAG_PROC_CWD pseudo tag, we can better guess its
root dir."
  (if (and citre--tags-file (file-exists-p citre--tags-file))
      citre--tags-file
    (let ((current-file (or (buffer-file-name) default-directory))
          (project (funcall citre-project-root-function)))
      (or
       (cl-dolist (pair citre-tags-file-alist)
         (when (and (or (not (file-name-absolute-p (car pair)))
                        (not (file-name-absolute-p (cdr pair))))
                    (null project))
           (user-error "Relative path used in `citre-tags-file-alist', \
but project root can't be decided by `citre-project-root-function'"))
         (let ((cwd (expand-file-name (car pair) project))
               (tagsfile (expand-file-name (cdr pair) project)))
           (when (file-in-directory-p current-file cwd)
             (puthash tagsfile cwd citre-core--tags-file-cwd-guess-table)
             (cl-return (setq citre--tags-file tagsfile)))))
       (cl-dolist (tagsfile citre-tags-files)
         (let ((dir (locate-dominating-file current-file tagsfile)))
           (when dir
             (let ((tagsfile (expand-file-name tagsfile dir)))
               (puthash tagsfile (expand-file-name dir)
                        citre-core--tags-file-cwd-guess-table)
               (cl-return (setq citre--tags-file tagsfile))))))))))

;;;;; APIs: Language support framework

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
  for filtering/sorting the tags, see the props below.  Citre
  automatically attach 2 more props to the returned value:
  `citre-file-path' for the canonical path of current file (when
  in a file buffer), and `citre-tags-file' for the canonical path
  of tags file, so filters/sorters can make use of them.

  If you don't specify this prop, `citre-get-symbol-default' is
  used as fallback.  You can also use it internally, and add more
  properties you need.

  When there's an active region, it's recommended to get the text
  in it as a symbol, so when your function doesn't work well for
  the user, they can manually specify which part to get.
  `citre-get-marked-symbol' implements this, and is also used by
  `citre-get-symbol-default'.

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
  (let ((sym (funcall (or (citre--get-value-in-language-alist :get-symbol)
                          #'citre-get-symbol-default))))
    (citre-put-property sym 'file-path (buffer-file-name))
    (citre-put-property sym 'tags-file (citre-tags-file-path))
    sym))

;;;;; APIs: Auto-completion related

(defun citre-completion-default-filter (symbol)
  "Default completion filter for SYMBOL."
  (let ((tags-file (citre-get-property 'tags-file symbol))
        (file-path (citre-get-property 'file-path symbol)))
    `(not
      (or
       ,(citre-core-filter 'extras '("anonymous" "reference" "inputFile")
                           'csv-contain)
       ;; For tags file using single-letter kind, apply `not' to
       ;; `citre-core-filter-kind' may exclude more tags than it should.
       ;; But we know the "F" (file) kind is preserved by ctags, and "F" is
       ;; not used anywhere else, so we could do this.
       ,(citre-core-filter-kind "file")
       ;; Exclude tags that have "file" scope, and is not in this file.
       ,(if file-path
            `(and (not ,(citre-core-filter-input file-path tags-file))
                  (or ,(citre-core-filter-field-exist 'file)
                      ,(citre-core-filter 'extras "fileScope"
                                          'csv-contain)))
          'false)))))

(defvar citre-completion-default-sorter
  (citre-core-sorter
   '(length name +) 'name)
  "The default sorter expression for auto-completion.
This sorts the candidates by their length, then the alphabetical
order of their name.")

(defun citre-get-completions (&optional symbol tagsfile substr-completion)
  "Get completions from TAGSFILE of symbol at point.
TAGSFILE is the canonical path of the tags file.  If SYMBOL is
non-nil, use that symbol instead.  If TAGSFILE is not specified,
fint it automatically.  If SUBSTR-COMPLETION is non-nil, get tags
that contains SYMBOL, or get tags that starts with SYMBOL.  The
case sensitivity is controlled by
`citre-completion-case-sensitive'.

The returned value is a list of tags.  Nil is returned when the
completion can't be done."
  (when-let* ((symbol (or symbol (citre-get-symbol)))
              (tagsfile (or tagsfile (citre-tags-file-path)))
              (match (if substr-completion 'substr 'prefix)))
    (citre-get-tags tagsfile symbol match
                    :filter (or (citre--get-value-in-language-alist
                                 :completion-filter symbol)
                                (citre-completion-default-filter symbol))
                    :sorter (or (citre--get-value-in-language-alist
                                 :completion-sorter symbol)
                                citre-completion-default-sorter)
                    :require '(name)
                    :optional '(ext-kind-full signature typeref))))

;;;;; APIs: Display tags

;;;;;; Internals

(defun citre--relative-path (path &optional root)
  "Return PATH but relative to ROOT.
If PATH is not under ROOT, it's directly returned.  If ROOT is
nil, use project in current buffer (by
`citre-project-root-function') instead."
  (let ((root (or root (funcall citre-project-root-function))))
    (if (and root (file-in-directory-p path root))
        (file-relative-name path root)
      path)))

(defun citre--make-tag-name-str (tag prop)
  "Generate a string to display the name of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let ((name (citre-core-get-field 'name tag))
        (face
         (pcase (citre-core-get-field 'ext-kind-full tag)
           ("class" 'font-lock-type-face)
           ((or "const" "constant") 'font-lock-constant-face)
           ("macro" 'font-lock-keyword-face)
           ((or "function" "f") 'font-lock-function-name-face)
           ("method" 'font-lock-function-name-face)
           ("struct" 'font-lock-type-face)
           ((or "typedef" "type") 'font-lock-type-face)
           ((or "variable" "var" "v") 'font-lock-variable-name-face))))
    (when name
      (concat (or (plist-get prop :prefix) "")
              (if face (propertize name 'face face) name)
              (or (plist-get prop :suffix) "")))))

(defun citre--make-tag-annotation-str (tag prop)
  "Generate a string to display the annotation of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let* ((kind (unless (plist-get prop :no-kind)
                 (citre-core-get-field 'ext-kind-full tag)))
         (type (unless (plist-get prop :no-type)
                 (citre-core-get-field 'typeref tag 'after-colon)))
         (extras (citre-core-get-field 'extras tag))
         (reference
          (unless (plist-get prop :no-reference)
            (and extras
                 (string-match "\\(^\\|,\\) ?reference\\(,\\|$\\)" extras))))
         (reference (when reference citre-definition-reference-mark))
         (ref-first (plist-get prop :reference-first))
         (face 'citre-definition-annotation-face))
    (when (or kind type reference)
      (concat
       (propertize (or (plist-get prop :prefix) "") 'face face)
       (if ref-first (or reference ""))
       (concat (propertize (or kind "") 'face face)
               (if (and kind type) citre-definition-annotation-separator "")
               (propertize (or type "") 'face face))
       (if (not ref-first) (or reference ""))
       (propertize (or (plist-get prop :suffix) "") 'face face)))))

(defun citre--make-tag-location-str (tag prop)
  "Generate a string to display the location of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let* ((abspath (unless (plist-get prop :no-path)
                    (citre-core-get-field 'ext-abspath tag)))
         (line (unless (plist-get prop :no-line)
                 (citre-core-get-field 'extra-line tag))))
    (when (or abspath line)
      (concat
       (or (plist-get prop :prefix) "")
       ;; path
       (if abspath
           (concat
            (if (file-exists-p abspath) ""
              citre-definition-missing-file-mark)
            (propertize (citre--relative-path abspath (plist-get prop :root))
                        'face 'citre-definition-path-face))
         "")
       (if line
           (concat (if abspath "(" "")
                   (propertize (number-to-string line)
                               'face 'warning)
                   (if abspath ")" ""))
         "")
       (or (plist-get prop :suffix) "")))))

(defun citre--make-tag-content-str (tag prop)
  "Return the string recorded in the pattern field of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (when-let ((str (citre-core-get-field 'extra-matched-str tag)))
    (concat (or (plist-get prop :prefix) "")
            (string-trim str)
            (or (plist-get prop :suffix) ""))))

;;;;;; The API

(cl-defun citre-make-tag-str (tag separator &rest args)
  "Generate a string for TAG for displaying.
TAG should be an element in the returned value of
`citre-get-definitions'.  ARGS is the components the string
should contain, in the order of presence.  Each element of ARGS
is a list of:

  (component :prop val :prop val ...)

Avaliable ones are:

- name: Name of the tag.  It's propertized by font-lock faces
  according to the kind of the tag.

  relevant fields: `name', `ext-kind-full'.

- annotation: Looks like \"kind/type<R>\".  \"<R>\" is a mark for
  reference tags, customizable by
  `citre-definition-reference-mark'.  `:no-kind', `:no-type',
  `:no-reference' controls the presence of each part,
  `:reference-first' puts the reference mark before other parts.

  relevant fields: `ext-kind-full', `typeref', `extras'.

- location: Looks like \"path(line)\". `:no-path', `:no-line'
  controls the presence of each part.  When there's only the line
  number, the parentheses around it are omitted.  When `:root' is
  specified, files under it will be displayed relative to it.
  When the path doesn't exist,
  `citre-definition-missing-file-mark' is prefixed to the path.

  relevant fields: `ext-abspath', `extra-line'.

- content: The string recorded in the pattern field of TAG.

  relevant fields: `pattern'.

All components have `:prefix' and `:suffix' properties to attach
extra prefix and suffix strings to them.  When a component or
some parts of it can't be generated, they are omitted.

SEPARATOR specifies the separator between components.  A space is
used when it's nil."
  (let (parts)
    (dolist (arg args)
      (let ((prop (cdr arg)))
        (push
         (pcase (car arg)
           ('name (citre--make-tag-name-str tag prop))
           ('annotation (citre--make-tag-annotation-str tag prop))
           ('location (citre--make-tag-location-str tag prop))
           ('content (citre--make-tag-content-str tag prop)))
         parts)))
    (string-join (cl-remove nil (nreverse parts) :test #'eq)
                 (or separator " "))))

;;;;; APIs: Finding definitions

(defun citre-definition-default-filter (symbol)
  "Default definition filter for SYMBOL."
  (let ((tags-file (citre-get-property 'tags-file symbol))
        (file-path (citre-get-property 'file-path symbol)))
    `(not
      (or
       ,(citre-core-filter 'extras '("anonymous" "inputFile") 'csv-contain)
       ,(citre-core-filter-kind "file")
       ;; Exclude tags that have "file" scope, and is not in this file.
       ,(if file-path
            `(and (not ,(citre-core-filter-input file-path tags-file))
                  (or ,(citre-core-filter-field-exist 'file)
                      ,(citre-core-filter 'extras "fileScope"
                                          'csv-contain)))
          'false)))))

(defvar citre-definition-default-sorter
  (citre-core-sorter
   `(filter ,(citre-core-filter 'extras "reference" 'csv-contain) -)
   'input '(length name +) 'name)
  "The default sorter expression for finding definitions.
This sorts the file name by their alphabetical order, then the
length and alphabetical order of the tag names.")

(defun citre-get-definitions (&optional symbol tagsfile)
  "Get definitions from tags file TAGSFILE of symbol at point.
TAGSFILE is the canonical path of the tags file.  If SYMBOL is
non-nil, use that symbol instead.  If TAGSFILE is not specified,
find it automatically.

The result is a list of tags.  Nil is returned when no definition
is found."
  (let ((symbol (or symbol (citre-get-symbol)))
        (tagsfile (or tagsfile
                      (citre-tags-file-path)
                      (user-error "Can't find tagsfile"))))
    (unless symbol
      (user-error "No symbol at point"))
    (citre-get-tags tagsfile symbol 'exact
                    :filter (or (citre--get-value-in-language-alist
                                 :definition-filter symbol)
                                (citre-definition-default-filter symbol))
                    :sorter (or (citre--get-value-in-language-alist
                                 :definition-sorter symbol)
                                citre-definition-default-sorter)
                    :require '(name ext-abspath pattern)
                    :optional '(ext-kind-full line typeref extras))))

(defun citre-goto-tag (tag &optional window)
  "Jump to the location of TAG.
WINDOW can be:

- nil: Use current window.
- `other-window': Use other window.
- `other-window-noselect': Use other window but don't select it."
  (let ((path (citre-core-get-field 'ext-abspath tag)))
    (unless path
      (error "TAG doesn't have the ext-abspath field"))
    (unless (file-exists-p path)
      (user-error "File %s doesn't exist" path))
    (let* ((buf (find-file-noselect path))
           (current-buf (current-buffer))
           (current-window (selected-window)))
      (if window
          (pop-to-buffer
           buf
           '(display-buffer-use-some-window . ((inhibit-same-window . t)
                                               (inhibit-switch-frame . t)))
           (when (eq window 'other-window-noselect) 'norecord))
        (pop-to-buffer buf '(display-buffer-same-window)))
      (goto-char (citre-core-locate-tag tag))
      (run-hooks 'citre-after-jump-hook)
      (when (eq window 'other-window-noselect)
        (select-window current-window)
        (pop-to-buffer current-buf '(display-buffer-same-window) 'norecord)))))

(defun citre-recenter-and-blink ()
  "Recenter point and blink after point.
This is suitable to run after jumping to a location."
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

;;;;; APIs: Text property related

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

(provide 'citre-util)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-util.el ends here
