;;; citre-tags.el --- Tags file backend -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 25 May 2022
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.3.1
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

;;; Code:

;; Tags file backend.  For now it only contains code of `citre-peek'.

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-backend-interface)
(require 'citre-ctags)
(require 'citre-readtags)
(require 'cl-lib)
(require 'rx)
(require 'subr-x)

;;;; User Options

;;;;; Auto-completion

(defcustom citre-tags-substr-completion nil
  "Whether do substring completion for the tags backend.
Non-nil means to match tags *containing* the symbol to be
completed, Otherwise match tags *start with* the symbol to be
completed.

Notice that when listing the candidates, Emacs itself will
further filter the completions we supply, and its behavior is
controlled by `completion-styles'.  If you want substring
completion, you need to set `citre-tags-substr-completion' to
non-nil, *and* add `substring' to `completion-styles' (for Emacs
27, there is also a `flex' style that will work)."
  :type 'boolean
  :group 'citre)

(make-obsolete 'citre-capf-substr-completion
               'citre-tags-substr-completion
               "0.3")

(defcustom citre-tags-completion-case-sensitive t
  "Case sensitivity of auto-completion using tags backend.

Note for developers: Actually this doesn't affect auto-completion
directly.  This option controls the behavior of
`citre-tags-get-tags' when its argument MATCH is not nil or
`exact', and when this is the case, it's likely that the user is
getting tags for auto-completion."
  :type 'boolean
  :group 'citre)

(make-obsolete 'citre-capf-completion-case-sensitive
               'citre-tags-completion-case-sensitive
               "0.3")

;;;;; Imenu

(defcustom citre-tags-imenu-create-tags-file-threshold (* 50 1024 1024)
  "The threshold (in bytes) to create a tags file for imenu.
When the tags file used is bigger than this threshold, searching
for tags in current file in it could be slow.  So, Citre let
Ctags scan the current file, and create a temporary tags file (in
variable `temporary-file-directory'), which is faster.

When the tags file in use contains a recipe, the command line in
it is used, just the dir/files to scan are substituted by the
current file.  If not, a command for Universal Ctags is used.

When this is nil, always use the existing tags file and never
create one for imenu.  When this is 0, always create a new tags
file for imenu."
  :type '(set integer (const nil))
  :group 'citre)

(make-obsolete 'citre-imenu-create-tags-file-threshold
               'citre-tags-imenu-create-tags-file-threshold
               "0.3")

;;;; APIs

;;;;; Readtags API wrapper

(cl-defun citre-tags-get-tags
    (&optional tagsfile name match
               &key filter sorter
               require optional exclude parse-all-fields)
  "Get tags in tags file TAGSFILE that match NAME.
This is like `citre-readtags-get-tags', except that:

- TAGSFILE could be nil, and it will be find automatically.
- When MATCH is nil or `exact', CASE-FOLD is always nil,
  otherwise it's decided by `citre-tags-completion-case-sensitive'.

TAGSFILE is the absolute path of the tags file.  For FILTER,
SORTER, REQUIRE, OPTIONAL, EXCLUDE, and PARSE-ALL-FIELDS, see
`citre-readtags-get-tags'.

Each element in the returned value is a list containing the tag
and some of its fields, which can be utilized by
`citre-get-tag-field'."
  (when (citre-executable-find (or citre-readtags-program "readtags") t)
    (when-let ((tagsfile (or tagsfile (citre-tags-file-path))))
      (citre-readtags-get-tags tagsfile name match
                               (unless (or (null match) (eq match 'exact))
                                 (not citre-tags-completion-case-sensitive))
                               :filter filter :sorter sorter
                               :require require :optional optional
                               :exclude exclude
                               :parse-all-fields parse-all-fields))))

;;;;; Common filter/sorter snippets

(defun citre-tags-filter-extra-tags (extras)
  "Filter that matches extra tags in list EXTRAS."
  (citre-readtags-filter 'extras extras 'csv-contain))

(defvar citre-tags-filter-file-tags
  `(or
    ,(citre-readtags-filter 'extras '("inputFile") 'csv-contain)
    ,(citre-readtags-filter-kind "file"))
  "Filter that matches file tags.")

(defun citre-tags-filter-local-symbol-in-other-file (file tagsfile)
  "Filter that matches tags with \"file\" scope, but not in FILE.
TAGSFILE is the absolute path of the tags file to use this filter
on."
  `(and (not ,(citre-readtags-filter-input file tagsfile))
        (or ,(citre-readtags-filter-field-exist 'file)
            ,(citre-readtags-filter 'extras "fileScope" 'csv-contain))))

(defvar citre-tags-sorter-arg-size-order
  '(expr (if (and $line $end &line &end)
             (<> (- &end &line) (- $end $line))
           0))
  "For tags with `line' and `end' field, sort them by size.
The \"size\" is the difference between its `end' and `line'
field.  A \"smaller\" definition may be a prototype or forward
declaration, while the \"bigger\" one is the actual definition.

This can be used as an arg for `citre-readtags-sorter'.")

(defvar citre-tags-sorter-arg-put-references-below
  `(filter ,(citre-readtags-filter 'extras "reference" 'csv-contain) -)
  "Put reference tags below others.
This can be used as an arg for `citre-readtags-sorter'.")

(defun citre-tags-sorter-arg-put-kinds-above (kinds)
  "Put tags with kind field in list KINDS above others.
This can be used as an arg for `citre-readtags-sorter'."
  (let ((filters (mapcar (lambda (k) (citre-readtags-filter-kind k))
                         kinds)))
    (if (eq (length filters) 1)
        (setq filters `(filter ,(car filters) +))
      (setq filters `(filter (or ,@filters) +)))))

;;;;; Language support framework

;;;;;; The lookup table

(defvar citre-tags-language-support-alist nil
  "The lookup table for language-specific support of tags backend.

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
  `citre-file-path' for the full path of current file (when in a
  file buffer), and `citre-tags-file' for the canonical path of
  tags file, so filters/sorters can make use of them.

  If you don't specify this prop, `citre-tags-get-symbol-default' is
  used as fallback.  You can also use it internally, and add more
  properties you need.

  When there's an active region, it's recommended to get the text
  in it as a symbol, so when your function doesn't work well for
  the user, they can manually specify which part to get.
  `citre-get-marked-symbol' implements this, and is also used by
  `citre-tags-get-symbol-default'.

- `:completion-filter': The filter for auto-completion.

  It can be a filter expression, a symbol whose value is a filter
  expression, or a function that takes the string returned by the
  `:get-symbol' function, and returns the filter expression.  The
  fallback is `citre-tags-completion-default-filter'.

- `:completion-sorter': The sorter for auto-completion.

  It can be a sorter expression, a symbol whose value is a sorter
  expression, or a function that takes the string returned by the
  `:get-symbol' function, and returns the sorter expression.The
  fallback is `citre-tags-completion-default-sorter'.

- `:definition-filter' and `:definition-sorter': The same as
  `:completion-filter' and `:completion-sorter', but used for
  finding definitions.  Their fallback values are
  `citre-tags-definition-default-filter' and
  `citre-tags-definition-default-sorter'.

The filter/sorter functions should be pure, i.e., should only use
information provided by the symbol, and not fetch information
from the environment.")

(defun citre-tags--get-value-in-language-alist (prop &optional symbol)
  "A helper for lookup PROP in `citre-tags-language-support-alist'.
Returns the value in it for the language in current buffer, and
PROP.

If SYMBOL is non-nil, and the value we get is a function, call
the function on SYMBOL and return its value."
  (when-let ((value (plist-get (alist-get major-mode
                                          citre-tags-language-support-alist)
                               prop)))
    (cond
     ((and (symbolp value) (boundp value))
      (symbol-value value))
     ((and symbol (functionp value))
      (funcall value symbol))
     (t value))))

;;;;;; APIs

(defun citre-tags-get-marked-symbol ()
  "Get the text in activate region as a symbol."
  (when (use-region-p)
    (let ((bounds (cons (region-beginning) (region-end))))
      (citre-put-property
       (buffer-substring-no-properties (car bounds) (cdr bounds))
       'bounds bounds))))

(defun citre-tags-get-symbol-at-point ()
  "Get the symbol at point."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (citre-put-property
     (buffer-substring-no-properties (car bounds) (cdr bounds))
     'bounds bounds)))

(defun citre-tags-get-symbol-default ()
  "Get the symbol at point.
If there's an active region, the text in it is returned as a
symbol.  Otherwise, the symbol at point is returned.  If both
fails, nil is returned.

The returned symbol is a string with a `citre-bounds' property,
recording the beginning/end positions of the symbol."
  (or (citre-tags-get-marked-symbol)
      (citre-tags-get-symbol-at-point)))

(defun citre-tags-get-symbol (&optional tagsfile)
  "Get the symbol at point.
Set `citre-tags-language-support-alist' to control the behavior
of this function for different languages.  `citre-file-path' and
`citre-tags-file' properties are attached to the symbol string so
filters/sorters can make use of them.

When TAGSFILE is non-nil, write it (rather than the tags file
associated with current buffer) to the `citre-tags-file' property
in the returned string.  This is needed when getting
definitions/completions of the returned symbol from a specified
tags file."
  (let ((sym (funcall (or (citre-tags--get-value-in-language-alist :get-symbol)
                          #'citre-tags-get-symbol-default))))
    (citre-put-property sym 'file-path (buffer-file-name))
    (citre-put-property sym 'tags-file (or tagsfile (citre-tags-file-path)))
    sym))

(defun citre-tags-register-language-support (mode plist)
  "Register language support for the tags backend.
MODE is a symbol of the major mode, PLIST is a plist described in
`citre-tags-language-support-alist'."
  (setf (alist-get mode citre-tags-language-support-alist)
        plist))

(defun citre-tags--symbol-at-point ()
  "Get the symbol at point using tags backend.
This is for display purpose only and the returned string doesn't
contain some properties as returned by `citre-tags-get-symbol'."
  (funcall (or (citre-tags--get-value-in-language-alist :get-symbol)
               #'citre-tags-get-symbol-default)))

(citre-register-symbol-at-point-backend 'tags #'citre-tags--symbol-at-point)

;;;;; Auto-completion related

(defun citre-tags-completion-default-filter (symbol)
  "Default completion filter for SYMBOL."
  (let ((tags-file (citre-get-property 'tags-file symbol))
        (file-path (citre-get-property 'file-path symbol)))
    `(not
      (or
       ,(citre-tags-filter-extra-tags '("anonymous" "reference"))
       ,citre-tags-filter-file-tags
       ,(if file-path
            (citre-tags-filter-local-symbol-in-other-file file-path tags-file)
          'false)))))

(defvar citre-tags-completion-default-sorter
  (citre-readtags-sorter
   '(length name +) 'name)
  "The default sorter expression for auto-completion.
This sorts the candidates by their length, then the alphabetical
order of their name.")

(defun citre-tags-get-completions (&optional symbol tagsfile substr-completion)
  "Get completions from TAGSFILE of symbol at point.
TAGSFILE is the absolute path of the tags file.  If SYMBOL is
non-nil, use that symbol instead.  If TAGSFILE is not specified,
fint it automatically.  If SUBSTR-COMPLETION is non-nil, get tags
that contains SYMBOL, or get tags that starts with SYMBOL.  The
case sensitivity is controlled by
`citre-tags-completion-case-sensitive'.

The returned value is a list of tags.  Nil is returned when the
completion can't be done."
  (when-let* ((symbol (or symbol (citre-tags-get-symbol tagsfile)))
              (tagsfile (or tagsfile (citre-tags-file-path)))
              (match (if substr-completion 'substr 'prefix)))
    (citre-tags-get-tags
     tagsfile symbol match
     :filter (or (citre-tags--get-value-in-language-alist
                  :completion-filter symbol)
                 (citre-tags-completion-default-filter symbol))
     :sorter (or (citre-tags--get-value-in-language-alist
                  :completion-sorter symbol)
                 citre-tags-completion-default-sorter)
     :require '(name)
     :optional '(ext-kind-full signature pattern scope typeref))))

;;;;; Finding definitions

(defun citre-tags-definition-default-filter (symbol)
  "Default definition filter for SYMBOL."
  (let ((tags-file (citre-get-property 'tags-file symbol))
        (file-path (citre-get-property 'file-path symbol)))
    `(not
      (or
       ;; Don't excluded "anonymous" here as such symbols can appear in typeref
       ;; or scope fields of other tags, which may be shown in an xref buffer,
       ;; so we should be able to find their definitions.
       ,(if file-path
            (citre-tags-filter-local-symbol-in-other-file file-path tags-file)
          'false)))))

(defvar citre-tags-definition-default-sorter
  (citre-readtags-sorter
   citre-tags-sorter-arg-put-references-below
   'input '(length name +) 'name
   citre-tags-sorter-arg-size-order)
  "The default sorter expression for finding definitions.
This sorts the file name by their alphabetical order, then the
length and alphabetical order of the tag names.")

(defun citre-tags-get-definitions (&optional symbol tagsfile)
  "Get definitions of symbol at point.
If SYMBOL is non-nil, use that symbol instead.  Notice it should
be returned by `citre-tags-get-symbol'.  If TAGSFILE is non-nil,
find definitions from that tags file, otherwise get the tagsfile
from SYMBOL.

The result is a list of tags.  Nil is returned when no definition
is found."
  (when-let* ((symbol (or symbol (citre-tags-get-symbol tagsfile)))
              (tagsfile (or tagsfile (citre-get-property 'tags-file symbol))))
    (citre-tags-get-tags
     tagsfile symbol 'exact
     :filter (or (citre-tags--get-value-in-language-alist
                  :definition-filter symbol)
                 (citre-tags-definition-default-filter symbol))
     :sorter (or (citre-tags--get-value-in-language-alist
                  :definition-sorter symbol)
                 citre-tags-definition-default-sorter)
     :require '(name ext-abspath pattern)
     :optional '(ext-kind-full line typeref scope extras))))

;;;; Completion backend

(defvar citre-tags--completion-cache
  '(:file nil :symbol nil :bounds nil :substr nil :cands nil)
  "A plist for completion cache of the tags backend.
Its props are:

- `:file': The file where the completion happens.
- `:symbol': The symbol that's been completed.
- `:bounds': The bound positions of `:symbol'.
- `:substr': Whether substring completion is used.  This is
  needed since in the same position, user may use popup
  completion that does prefix completion, and use their own
  command that let binds `citre-tags-substr-completion' to t and
  call `completion-at-point'.
- `:cands': The tags of completions.")

(defun citre-tags-get-completions-at-point ()
  "Get completions of symbol at point.
The result is a list (BEG END TAGS), see
`citre-register-completion-backend'."
  ;; Just to make sure the tags file exists.
  (when-let ((tagsfile (citre-tags-file-path))
             (symbol (citre-tags-get-symbol)))
    (if citre-capf-optimize-for-popup
        (let* ((cache citre-tags--completion-cache)
               (file (buffer-file-name))
               (bounds (citre-get-property 'bounds symbol)))
          (if (and (equal (plist-get cache :file) file)
                   (string-prefix-p (plist-get cache :symbol) symbol)
                   ;; We also need to make sure we are in the process of
                   ;; completing the same whole symbol, since same symbol in
                   ;; different positions can produce different results,
                   ;; depending on the language support implementation.
                   (eq (car (plist-get cache :bounds)) (car bounds))
                   ;; Just in case the user set `citre-tags-substr-completion'
                   ;; to something can't compare by `eq', we use `null' to make
                   ;; sure we are comparing t or nil.
                   (eq (null (plist-get cache :substr))
                       (null citre-tags-substr-completion)))
              (list (car bounds) (cdr bounds) (plist-get cache :cands))
            ;; Make sure we get a non-nil collection first, then setup the
            ;; cache, since the calculation can be interrupted by user input,
            ;; and we get nil, which aren't the actual completions.
            (when-let ((cands (citre-tags-get-completions
                               symbol nil citre-tags-substr-completion)))
              ;; Prevent keyboard quit when building cache.
              (let ((inhibit-quit t))
                (plist-put cache :file file)
                (plist-put cache :symbol (substring-no-properties symbol))
                (plist-put cache :bounds bounds)
                (plist-put cache :substr citre-tags-substr-completion)
                (plist-put cache :cands cands))
              (list (car bounds) (cdr bounds) cands))))
      (let ((bounds (citre-get-property 'bounds symbol)))
        (list (car bounds) (cdr bounds)
              (citre-tags-get-completions
               symbol nil citre-tags-substr-completion))))))

(citre-register-completion-backend 'tags #'citre-tags-get-completions-at-point)

;;;; Find definition backend

(defun citre-tags-get-definitions-at-point ()
  "Get definitions of symbol at point."
  (when-let ((tagsfile (citre-tags-file-path))
             (symbol (citre-tags-get-symbol)))
    (citre-tags-get-definitions symbol tagsfile)))

(defvar citre-tags--find-definition-for-id-filter
  `(not ,(citre-readtags-filter 'extras "anonymous" 'csv-contain))
  "Filter for finding definitions when the symbol is inputted by user.")

(defvar citre-tags--id-list-cache
  '(:tags-file nil :time nil :tags nil)
  "Plist for caching identifier list for tags backend.
Its props and vals are:

- `:tags-file': Canonical path of tags file.
- `:time': Last modified time of tags file.
- `:tags': The tags.")

(defun citre-tags--get-definition-for-id (symbol)
  "Get definition for SYMBOL without text property.
When xref prompts for user input for the symbol, we can't get
information from the environment of the symbol at point, so we
have to bypass the whole filter/sort mechanism of Citre and use
simple tag name matching.  This function is for it."
  (citre-tags-get-tags
   nil symbol 'exact
   :filter citre-tags--find-definition-for-id-filter
   :sorter citre-tags-definition-default-sorter
   :require '(name ext-abspath pattern)
   :optional '(ext-kind-full line typeref scope extras)))

(defun citre-tags-get-identifiers ()
  "Get a list of identifiers in current project."
  (when-let* ((tagsfile (citre-tags-file-path))
              (update-time (gethash 'time (citre-readtags-tags-file-info
                                           tagsfile))))
    (if (and (equal tagsfile
                    (plist-get citre-tags--id-list-cache
                               :tags-file))
             (equal update-time
                    (plist-get citre-tags--id-list-cache
                               :time)))
        (plist-get citre-tags--id-list-cache :tags)
      (let ((tags
             (cl-remove-duplicates
              (mapcar
               (lambda (tag) (citre-get-tag-field 'name tag))
               (citre-tags-get-tags
                ;; We don't use STR here, but return all tag names,
                ;; since we need to work with completion styles that
                ;; may not do a prefix completion.
                tagsfile nil nil
                :filter citre-tags--find-definition-for-id-filter
                :sorter (citre-readtags-sorter '(length name +) 'name)
                :require '(name)))
              :test #'equal)))
        (plist-put citre-tags--id-list-cache
                   :tags-file tagsfile)
        (plist-put citre-tags--id-list-cache
                   :time update-time)
        (plist-put citre-tags--id-list-cache
                   :tags tags)
        tags))))

(citre-register-find-definition-backend
 'tags #'citre-tags-get-definitions-at-point
 :identifier-list-func #'citre-tags-get-identifiers
 :get-definitions-for-id-func #'citre-tags--get-definition-for-id)

;;;; Tags in buffer backend

(declare-function tramp-get-remote-tmpdir "tramp" (vec))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))

(defun citre-tags--imenu-temp-tags-file-path ()
  "Return a temporary tags file path for imenu.
This also works on a remote machine."
  ;; Generate a name unique to this user & Emacs instance so we don't prevent
  ;; other users on the same machine to generate their own temporary tags file.
  (let* ((file (format "citre-%s-%s.tags" (user-login-name) (emacs-pid)))
         (dir (if (file-remote-p default-directory)
                  (tramp-get-remote-tmpdir
                   (tramp-dissect-file-name default-directory))
                temporary-file-directory)))
    (expand-file-name file dir)))

(defun citre-tags--imenu-ctags-command-cwd ()
  "Return ctags command and its cwd for tags file for imenu."
  (if-let* ((tagsfile (citre-tags-file-path))
            (scan-files (list (file-local-name (buffer-file-name))))
            (target (citre-tags--imenu-temp-tags-file-path))
            (cmd-and-cwd (citre-get-ctags-recipe-and-replace-parts
                          tagsfile scan-files target))
            (cmd (car cmd-and-cwd))
            (cwd (cdr cmd-and-cwd)))
      (cons cmd cwd)
    (cons `(,(or citre-ctags-program "ctags") "-o"
            ,(citre-tags--imenu-temp-tags-file-path)
            "--kinds-all=*" "--fields=*" "--extras=*"
            ,(file-local-name (buffer-file-name)))
          default-directory)))

(defun citre-tags--imenu-tags-from-tags-file ()
  "Get tags for imenu from the tags file being used."
  (when-let ((tagsfile (citre-tags-file-path)))
    (citre-tags-get-tags
     tagsfile nil nil
     :filter
     `(and ,(citre-readtags-filter-input (buffer-file-name)
                                         tagsfile)
           (not (or ,(citre-readtags-filter
                      'extras
                      '("anonymous" "inputFile")
                      'csv-contain)
                    ,(citre-readtags-filter-kind "file"))))
     :sorter (citre-readtags-sorter 'line)
     :require '(name pattern)
     :optional '(ext-kind-full line typeref scope extras))))

(defun citre-tags--imenu-tags-from-temp-tags-file ()
  "Get tags for imenu from a new temporary tags file.
If the ctags program is not found, this returns nil."
  (when (citre-executable-find (or citre-ctags-program "ctags"))
    (pcase-let* ((`(,cmd . ,cwd) (citre-tags--imenu-ctags-command-cwd))
                 (tags-file (citre-tags--imenu-temp-tags-file-path)))
      (make-directory (file-name-directory tags-file) 'parents)
      (let ((default-directory cwd))
        (apply #'process-file (car cmd)
               nil (get-buffer-create "*ctags*") nil
               (cdr cmd)))
      ;; WORKAROUND: If we don't sit for a while, the readtags process will
      ;; freeze.  TOOD: Fix this when uctags offers "edittags" command.
      (sit-for 0.001)
      (citre-tags-get-tags
       tags-file nil nil
       :filter
       `(not (or ,(citre-readtags-filter
                   'extras
                   '("anonymous" "inputFile")
                   'csv-contain)
                 ,(citre-readtags-filter-kind "file")))
       :sorter (citre-readtags-sorter 'line)
       :require '(name pattern)
       :optional '(ext-kind-full line typeref scope extras))
      (delete-file tags-file))))

(defun citre-tags-get-tags-in-buffer ()
  "Get tags in buffer."
  (let* ((tagsfile (citre-tags-file-path)))
    (if (or (null citre-tags-imenu-create-tags-file-threshold)
            (and tagsfile
                 (< (file-attribute-size (file-attributes tagsfile))
                    citre-tags-imenu-create-tags-file-threshold)))
        (citre-tags--imenu-tags-from-tags-file)
      (citre-tags--imenu-tags-from-temp-tags-file))))

(citre-register-tags-in-buffer-backend 'tags #'citre-tags-get-tags-in-buffer)

;;;; Auto enable citre-mode

(citre-register-backend-usable-probe 'tags #'citre-tags-file-path)

(provide 'citre-tags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-tags.el ends here
