;;; citre-util.el --- Utilities for tools in Citre -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.2
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
(require 'rx)
(require 'subr-x)

;;;; User options

;;;;; Options: Finding tags file

;; NOTE: See docs/user-manual/about-tags-file.md to know about these options.

(defcustom citre-tags-files '(".tags" "tags")
  "List of tags files.
These are searched up directory hierarchy from the file of
current buffer, or from `default-directory' in current buffer, to
decide which tags file to use.

This list is in descending order of priority (i.e., if we find
one, then the rest will be ignored)."
  :type '(repeat string)
  :group 'citre)

(defcustom citre-tags-file-global-cache-dir "~/.cache/tags/"
  "An absolute directory where you can save all your tags files.
Tags files in it are named using the path to the directory in
which you want to use the tags file.

If you work on a remote machine, this points to directory on the
remote machine."
  :type 'string
  :group 'citre)

(defcustom citre-tags-file-per-project-cache-dir "./.tags/"
  "A relative directory where you can save all your tags files in the projct.
This directory is expanded to the project root detected by
`citre-project-root-function', and when you are visiting files in
the project, this directory is searched for a tags file.

Tags files in it are named using the relative path to the
directory in which you want to use the tags file."
  :type 'string
  :group 'citre)

(defcustom citre-tags-file-alist nil
  "Alist of directory -> tags file.
If current file in buffer is in one of the directories, the
corresponding tags file will be used.

This is a buffer-local variable so you can customize it on a
per-project basis.  Relative paths in it will be expanded against
the project root, which is detected by
`citre-project-root-function'.

The global (default) value of this still works as a fallback for
its buffer-local value.  So you can use `setq-default' to
customize this for directories where it's inconvenient to have
dir-local variables."
  :type '(alist :key-type string :value-type string)
  :group 'citre)

;;;###autoload
(put 'citre-tags-file-alist 'safe-local-variable #'listp)
(make-variable-buffer-local 'citre-tags-file-alist)

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

;;;; Core API wrapper

(cl-defun citre-get-tags
    (&optional tagsfile name match
               &key filter sorter
               require optional exclude parse-all-fields)
  "Get tags in tags file TAGSFILE that match NAME.
This is like `citre-core-get-tags', except that:

- TAGSFILE could be nil, and it will be find automatically.
- When MATCH is nil or `exact', CASE-FOLD is always nil,
  otherwise it's decided by `citre-completion-case-sensitive'.

TAGSFILE is the absolute path of the tags file.  For FILTER,
SORTER, REQUIRE, OPTIONAL, EXCLUDE, and PARSE-ALL-FIELDS, see
`citre-core-get-tags'.

Each element in the returned value is a list containing the tag
and some of its fields, which can be utilized by
`citre-get-tag-field'."
  (citre-core-get-tags (or tagsfile (citre-tags-file-path)
                           (user-error "Can't find a tags file"))
                       name match
                       (unless (or (null match) (eq match 'exact))
                         (not citre-completion-case-sensitive))
                       :filter filter :sorter sorter
                       :require require :optional optional
                       :exclude exclude
                       :parse-all-fields parse-all-fields))

(defun citre-get-pseudo-tag-value (name &optional tagsfile)
  "Get the value field of pseudo tag NAME in TAGSFILE.
NAME should not start with \"!_\".

When TAGSFILE is nil, find it automatically."
  (when-let ((tagsfile (or tagsfile (citre-tags-file-path)))
             (ptag (citre-core-get-pseudo-tags name tagsfile)))
    (nth 1 (car ptag))))

;;;; APIs

;;;;; APIs: Find tags file

;;;;;; Internals

(defvar-local citre--tags-file nil
  "Buffer-local cache for tags file path.")

(defun citre--up-directory (file)
  "Return the directory up from FILE.
No matter if FILE is a file or dir, return the dir contains
it.

If there's no directory up, return nil.  Also return nil if FILE
is a local/remote user home dir."
  (unless (string-match
           (rx (or (seq bol (opt (seq alpha ":")) "/" eol)
                   (seq bol
                        ;; remote identifier
                        (opt "/"
                             ;; method
                             (+ (or alnum "-")) ":"
                             ;; user.  NOTE: (not (any ...)) seems to be the
                             ;; only accepted form for Emacs 26.
                             (+ (not (any "/" "|" ":" "\t")))
                             ;; host
                             (opt "@" (+ (or alnum "_" "." "%" "-"))) ":")
                        ;; local filename
                        (or (seq "/home/" (+ (not (any "/"))) "/" eol)
                            (seq bol "~/" eol)))))
           file)
    (let* ((dirname (directory-file-name file))
           (dir (file-name-directory dirname)))
      dir)))

;;;;;; By `citre-tags-file-alist'

(defun citre--find-tags-by-tags-file-alist (dir project alist)
  "Find the tags file of DIR by ALIST.
ALIST meets the requirements of `citre-tags-file-alist'.  DIR is
an absolute path.  Relative paths in the alist are expanded
against PROJECT, an absolute path."
  (let* ((dir (file-truename dir))
         (expand-file-name-against-project
          (lambda (file)
            (if (file-name-absolute-p file)
                ;; Convert ~/foo to /home/user/foo
                (expand-file-name file)
              (when project
                (expand-file-name file project))))))
    (cl-dolist (pair alist)
      (when-let ((target-dir (funcall expand-file-name-against-project
                                      (car pair)))
                 (target-tags (funcall expand-file-name-against-project
                                       (cdr pair))))
        (when (and (file-equal-p dir target-dir)
                   (citre-non-dir-file-exists-p target-tags))
          (cl-return target-tags))))))

;;;;;; By `citre-tags-file-global/per-project-cache-dir'

(defun citre--path-to-cache-tags-file-name (path)
  "Convert PATH to the non-directory part a tagsfile name.
PATH is canonical or relative to the project root.  It's where
you want to use the tags file.  The returned name can be used in
`citre-tags-file-global-cache-dir' or
`citre-tags-file-per-project-cache-dir' as tags file names."
  (when (file-name-absolute-p path)
    (setq path (expand-file-name path))
    ;; Check if it's a Windows path.  We don't use `system-type' as the user
    ;; may work on a remote Windows machine (people really do this?)
    (when (string-match "^[[:alpha:]]:" (file-local-name path))
      ;; We remote the colon after the disk symbol, or Emacs will think
      ;; "d:!project!path" is absolute and refuse to expand it against the
      ;; cache dir.
      (setq path (concat (or (file-remote-p path) "")
                         (char-to-string (aref path 0))
                         (substring path 2)))))
  ;; Escape backslashes
  (setq path (replace-regexp-in-string "\\\\" "\\\\\\&" path))
  ;; Escape exclamation marks
  (setq path (replace-regexp-in-string "!" "\\\\\\&" path))
  (concat (replace-regexp-in-string "/" "!" path) ".tags"))

(defun citre-tags-file-in-global-cache (dir)
  "Return the tags file name of DIR in global cache dir.
DIR is absolute.  The full path of the tags file is returned."
  (concat
   (or (file-remote-p default-directory) "")
   (expand-file-name
    (citre--path-to-cache-tags-file-name (file-local-name (file-truename dir)))
    citre-tags-file-global-cache-dir)))

(defun citre-tags-file-in-per-project-cache (dir &optional project)
  "Return the tags file name of DIR in per-project cache dir.
DIR is absolute.  PROJECT is the absolute project root.  If it's
nil, it's detected by `citre-project-root-function'.  The full
path of the tags file is returned."
  (let ((project (or project (funcall citre-project-root-function)))
        (dir (file-truename dir)))
    (if project
        (progn
          (setq project (file-truename project))
          (expand-file-name
           (citre--path-to-cache-tags-file-name
            (file-relative-name dir project))
           (expand-file-name citre-tags-file-per-project-cache-dir project)))
      (error "Can't detect project root"))))

(defun citre--find-tags-in-cache-dirs (dir &optional project)
  "Find the tags file of DIR in cache dirs.
DIR is absolute.  PROJECT is the project root.  If it's nil, it's
detected by `citre-project-root-function'.

The full path of the tags file is returned."
  (let ((project (or project (funcall citre-project-root-function))))
    (cl-block nil
      ;; First search in per project cache dir.
      (when (and project citre-tags-file-per-project-cache-dir)
        (let ((tagsfile (citre-tags-file-in-per-project-cache dir project)))
          (when (citre-non-dir-file-exists-p tagsfile)
            (cl-return tagsfile))))
      ;; Then search in global cache dir.
      (when citre-tags-file-global-cache-dir
        (let ((tagsfile (citre-tags-file-in-global-cache dir)))
          (when (citre-non-dir-file-exists-p tagsfile)
            (cl-return tagsfile)))))))

;;;;;; By `citre-tags-files'

(defun citre--find-tags-in-dir (dir)
  "Find the tags file of DIR by `citre-tags-files' in DIR.
DIR is an absolute path."
  (cl-dolist (file citre-tags-files)
    (let ((tags (expand-file-name file dir)))
      (when (and (citre-non-dir-file-exists-p tags)
                 (not (file-directory-p tags)))
        (cl-return tags)))))

;;;;;; APIs

(defun citre-tags-file-path ()
  "Return the canonical path of tags file for current buffer.
This finds the tags file up directory hierarchy, and for each
directory, it tries the following methods in turn:

- Use `citre-tags-file-alist'.
- Find in `citre-tags-file-cache-dirs'.
- See if one name in `citre-tags-files' exists in this dir.

It also sets `citre-core--tags-file-cwd-guess-table', so for tags
file without the TAG_PROC_CWD pseudo tag, we can better guess its
root dir."
  (if (and citre--tags-file (citre-non-dir-file-exists-p citre--tags-file))
      citre--tags-file
    (let* ((current-dir (file-truename (citre-current-dir)))
           (project (funcall citre-project-root-function))
           (tagsfile nil))
      (while (and current-dir (null tagsfile))
        (setq tagsfile
              (or (and (local-variable-p 'citre-tags-file-alist)
                       (citre--find-tags-by-tags-file-alist
                        current-dir project citre-tags-file-alist))
                  (and (default-value 'citre-tags-file-alist)
                       (citre--find-tags-by-tags-file-alist
                        current-dir nil (default-value
                                          'citre-tags-file-alist)))
                  (and (or citre-tags-file-global-cache-dir
                           citre-tags-file-per-project-cache-dir)
                       (citre--find-tags-in-cache-dirs current-dir project))
                  (and citre-tags-files
                       (citre--find-tags-in-dir current-dir))))
        (unless tagsfile
          (setq current-dir (citre--up-directory current-dir))))
      (when tagsfile
        (setq tagsfile (file-truename tagsfile))
        (puthash tagsfile current-dir
                 citre-core--tags-file-cwd-guess-table)
        ;; Only cache the result for file buffers, since non-file buffers may
        ;; change their own default directories, e.g., when cd to another
        ;; project.
        (when buffer-file-name
          (setq citre--tags-file tagsfile))
        tagsfile))))

(defun citre-clear-tags-file-cache ()
  "Clear the cache of buffer -> tagsfile.
Use this when a new tags file is created."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (kill-local-variable 'citre--tags-file))))

;;;;; APIs: Common filter/sorter snippets

(defun citre-filter-extra-tags (extras)
  "Filter that matches extra tags in list EXTRAS."
  (citre-core-filter 'extras extras 'csv-contain))

(defvar citre-filter-file-tags
  `(or
    ,(citre-core-filter 'extras '("inputFile") 'csv-contain)
    ,(citre-core-filter-kind "file"))
  "Filter that matches file tags.")

(defun citre-filter-local-symbol-in-other-file (file tagsfile)
  "Filter that matches tags with \"file\" scope, but not in FILE.
TAGSFILE is the absolute path of the tags file to use this filter
on."
  `(and (not ,(citre-core-filter-input file tagsfile))
        (or ,(citre-core-filter-field-exist 'file)
            ,(citre-core-filter 'extras "fileScope" 'csv-contain))))

(defvar citre-sorter-arg-size-order
  '(expr (if (and $line $end &line &end)
             (<> (- &end &line) (- $end $line))
           0))
  "For tags with `line' and `end' field, sort them by size.
The \"size\" is the difference between its `end' and `line'
field.  A \"smaller\" definition may be a prototype or forward
declaration, while the \"bigger\" one is the actual definition.

This can be used as an arg for `citre-core-sorter'.")

(defvar citre-sorter-arg-put-references-below
  `(filter ,(citre-core-filter 'extras "reference" 'csv-contain) -)
  "Put reference tags below others.
This can be used as an arg for `citre-core-sorter'.")

(defun citre-sorter-arg-put-kinds-above (kinds)
  "Put tags with kind field in list KINDS above others.
This can be used as an arg for `citre-core-sorter'."
  (let ((filters (mapcar (lambda (k) (citre-core-filter-kind k))
                         kinds)))
    (if (eq (length filters) 1)
        (setq filters `(filter ,(car filters) +))
      (setq filters `(filter (or ,@filters) +)))))

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
  `citre-file-path' for the full path of current file (when in a
  file buffer), and `citre-tags-file' for the canonical path of
  tags file, so filters/sorters can make use of them.

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
       ,(citre-filter-extra-tags '("anonymous" "reference"))
       ,citre-filter-file-tags
       ,(if file-path
            (citre-filter-local-symbol-in-other-file file-path tags-file)
          'false)))))

(defvar citre-completion-default-sorter
  (citre-core-sorter
   '(length name +) 'name)
  "The default sorter expression for auto-completion.
This sorts the candidates by their length, then the alphabetical
order of their name.")

(defun citre-get-completions (&optional symbol tagsfile substr-completion)
  "Get completions from TAGSFILE of symbol at point.
TAGSFILE is the absolute path of the tags file.  If SYMBOL is
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
                    :optional '(ext-kind-full signature scope typeref))))

;;;;; APIs: Finding definitions

(defun citre-definition-default-filter (symbol)
  "Default definition filter for SYMBOL."
  (let ((tags-file (citre-get-property 'tags-file symbol))
        (file-path (citre-get-property 'file-path symbol)))
    `(not
      (or
       ;; Don't excluded "anonymous" here as such symbols can appear in typeref
       ;; or scope fields of other tags, which may be shown in an xref buffer,
       ;; so we should be able to find their definitions.
       ,citre-filter-file-tags
       ,(if file-path
            (citre-filter-local-symbol-in-other-file file-path tags-file)
          'false)))))

(defvar citre-definition-default-sorter
  (citre-core-sorter
   citre-sorter-arg-put-references-below
   'input '(length name +) 'name
   citre-sorter-arg-size-order)
  "The default sorter expression for finding definitions.
This sorts the file name by their alphabetical order, then the
length and alphabetical order of the tag names.")

(defun citre-get-definitions (&optional symbol tagsfile)
  "Get definitions from tags file TAGSFILE of symbol at point.
TAGSFILE is the absolute path of the tags file.  If SYMBOL is
non-nil, use that symbol instead.  If TAGSFILE is not specified,
find it automatically.

The result is a list of tags.  Nil is returned when no definition
is found."
  (let ((symbol (or symbol (citre-get-symbol)))
        (tagsfile (or tagsfile (citre-tags-file-path)
                      (user-error "Can't find tags file for current buffer"))))
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
                    :optional '(ext-kind-full line typeref scope extras))))
(provide 'citre-util)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-util.el ends here
