;;; citre-backend-interface.el --- Interface of Citre backends -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
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

;;

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common-tag)
(require 'citre-ui-jump)
(require 'citre-ui-peek)

;;;; User Options

(defcustom citre-completion-backends '(tags global)
  "List of completion backends.
The backends are tried in turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-find-definition-backends '(tags global)
  "List of backends for finding definitions.
The backends are tried in turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-find-reference-backends '(global)
  "List of backends for finding references.
The backends are tried in turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-tags-in-buffer-backends '(tags global)
  "List of backends for finding tags in buffer.
This is used for imenu integration.  The backends are tried in
turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

;; Not used in this file, but defined here so backends could check it, e.g.,
;; cache the results only when this is non-nil.
(defcustom citre-capf-optimize-for-popup t
  "Non-nil means optimize for popup completion.
This caches latest completion result, and allows typing while
calculating completions, making it slicker to use.

`company' and `auto-complete' users should leave this as t.  For
other users, set this to nil may be slightly better, since a
completion session can be interrupted when you call
`completion-at-point', and while it's calculating, you press some
key by mistake, but that doesn't happen very often."
  :type 'boolean
  :group 'citre)

;;;; Backend interface

;;;;; Internals

(defvar citre--completion-backends-table (make-hash-table :test #'eq :size 5)
  "Lookup table for Citre backends.
Use `citre-register-completion-backend' to modify this table.")

(defvar citre--find-definition-backends-table
  (make-hash-table :test #'eq :size 5)
  "Lookup table for find definition backends.
Use `citre-register-find-definition-backend' to modify this
table.")

(defvar citre--find-reference-backends-table
  (make-hash-table :test #'eq :size 5)
  "Lookup table for find reference backends.
Use `citre-register-find-reference-backend' to modify this
table.")

(defvar citre--tags-in-buffer-backends-table
  (make-hash-table :test #'eq :size 5)
  "Lookup table for find tags in buffer.
Use `citre-register-tags-in-buffer-backend' to modify this
table.")

(defvar citre--symbol-at-point-backends-table
  (make-hash-table :test #'eq :size 5)
  "Lookup table for symbol at point functions.
Use `citre-register-symbol-at-point-backend' to modify this
table.")

(defvar citre--backend-usable-probe-table
  (make-hash-table :test #'eq :size 5)
  "Lookup table of probe functions to detect if a backend is usable.
Use `citre-register-backend-usable-probe' to modify this table.")

(defvar citre--after-jump-functions nil
  "List of after jump functions.
Use `citre-register-after-jump-function' to modify this list.")

(defun citre--get-prop-of-backend (backend prop table)
  "Get property PROP of BACKEND from TABLE.
Return nil if the property is undefined."
  (if-let ((backend-table (gethash backend table)))
      (gethash prop backend-table)
    (error "Backend %s not exist in TABLE" backend)))

(defun citre--try-func-in-backend-list (func table list &rest arguments)
  "Try call FUNC in TABLE with backend iterate over LIST.
FUNC is a symbol, see `citre-register-*-backend' functions for
its valid values.  FUNC is called with ARGUMENTS.

The backend and the result of the first successful try is returnd
in a cons pair.  If all backend fails, nil is returned."
  (cl-dolist (backend list)
    (when-let* ((f (citre--get-prop-of-backend backend func table))
                (result (apply f arguments)))
      (cl-return (cons backend result)))))

;;;;; APIs for backends

(defun citre-register-completion-backend (name get-completions-func)
  "Register a new completion backend.
NAME is the name of the backend and should be a symbol.

GET-COMPLETIONS-FUNC is called with no arguments, and should
return a list like (BEG END TAGS).  BEG and END is the region
that the completion should replace, and TAGS is a list of tags,
which are completions of symbol at point.  The tags should at
least contain `name' field, and optionally `ext-kind-full',
`signature', `pattern', `scope' and `typeref' fields, which
allows the UI to display more information.  If no completion is
available, it should return nil."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'get-completions-func get-completions-func backend)
    (puthash name backend citre--completion-backends-table)))

(cl-defun citre-register-find-definition-backend
    (name get-definitions-func &key identifier-list-func
          get-definitions-for-id-func)
  "Register a new backend for finding definitions.
NAME is the name of the backend and should be a symbol.

GET-DEFINITIONS-FUNC is called with no arguments, and should
return a list of tags, which are the possible definitions of the
symbol at point.  The tags should at least contain `name' and
`ext-abspath' fields.  `line' and `pattern' field are optional but
could help locating the definition more precisely.
`ext-kind-full', `typeref', `scope' and `extras' fields allows
the UI to display more information.  If no definition is
available, it should return nil.

If it's possible, GET-DEFINITIONS-FUNC should use the marked text
as the symbol at point when there's an active region, so the user
could specify the symbol in case it's not grabbed correctly.

When finding definitions using xref, and there's no symbol at
point, xref prompts the user for an identifier.  If the backend
wants to support this, IDENTIFIER-LIST-FUNC and
GET-DEFINITIONS-FOR-ID-FUNC are needed.

IDENTIFIER-LIST-FUNC is called with no arguments and inside the
code buffer.  It should return a list of strings, which is all
identifiers in the current project.  If no identifier is
available, it should return nil.

GET-DEFINITIONS-FOR-ID-FUNC are called with an identifier name (a
string) as the argument and inside the code buffer, and should
return the definition tags of the identifier.

IDENTIFIER-LIST-FUNC and GET-DEFINITIONS-FOR-ID-FUNC are used
together by xref to find definitions of any symbol in a project.
To make them work, ensure that:

- GET-DEFINITIONS-FOR-ID-FUNC doesn't rely on the text properties
  of the returned value by IDENTIFIER-LIST-FUNC.  This is because
  xref uses `completing-read' to filter the identifier list which
  strips the text properties.
- When GET-DEFINITIONS-FOR-ID-FUNC returns nil, i.e., no
  identifiers could be find for the current project,
  GET-DEFINITIONS-FOR-ID-FUNC should return nil for any argument
  value.  This is to make sure that, for an id given by a certain
  backend, when we try backends in
  `citre-find-definition-backends' to find definitions for it,
  backends comes before that backend don't intercept it."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'get-definitions-func get-definitions-func backend)
    (puthash 'identifier-list-func identifier-list-func backend)
    (puthash 'get-definitions-for-id-func get-definitions-for-id-func backend)
    (puthash name backend citre--find-definition-backends-table)))

;; NOTE: We don't implement identifier table for finding references using xref,
;; since an `xref-backend-identifier-completion-table' method can't know if
;; it's called for finding definitions or references.  We don't want to call
;; `get-definitions-for-id-func' in a find-definition backend to get an ID
;; (maybe from a tags file) but feed it to a find-reference backend (which may
;; use global), so we only implement it for finding definitions.
(cl-defun citre-register-find-reference-backend
    (name get-references-func)
  "Register a new backend for finding references.
NAME is the name of the backend and should be a symbol.

The arguments are similar to
`citre-register-find-definition-backend', but GET-REFERENCES-FUNC
should return tags of references, not definitions."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'get-references-func get-references-func backend)
    (puthash name backend citre--find-reference-backends-table)))

(defun citre-register-tags-in-buffer-backend (name get-tags-in-buffer-func)
  "Register a new backend for finding tags in buffer.
This is used for, e.g., imenu integration.

NAME is the name of the backend and should be a symbol.
GET-TAGS-IN-BUFFER-FUNC is called with no arguments, and should
return a list of tags in current buffer.  `name' field is
required in the tags, `pattern' and/or `line' fields should also
appear.  `kind' and `extras' fields helps imenu to classify the
tags.  When no tags is available, it should return nil."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'get-tags-in-buffer-func get-tags-in-buffer-func backend)
    (puthash name backend citre--tags-in-buffer-backends-table)))

(defun citre-register-symbol-at-point-backend (name symbol-at-point-func)
  "Register a new backend for getting symbol at point.
This is used as hints in the UI, e.g., in the error message when
no definition is found for symbol at point.

NAME is the name of the backend and should be a symbol.
SYMBOL-AT-POINT-FUNC is called with no arguments, and should
return a string of the symbol at point.  When there's no symbol
at point, it should return nil."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'symbol-at-point-func symbol-at-point-func backend)
    (puthash name backend citre--symbol-at-point-backends-table)))

(defun citre-register-backend-usable-probe (name backend-usable-func)
  "Register a probe for detecting if backend NAME is usable.
BACKEND-USABLE-FUNC is called with no arguments, and should
return nil when the backend is not usable in current buffer.

This is used for `citre-auto-enable-citre-mode'."
  (let ((backend (make-hash-table :test #'eq :size 5)))
    (puthash 'backend-usable-func backend-usable-func backend)
    (puthash name backend citre--backend-usable-probe-table)))

(defun citre-register-after-jump-function (func)
  "Register FUNC as an after-jump function.
FUNC is called after jumping to a definition or reference, with
the buffer before jumping as the argument.  If the backends uses
some database (e.g., tags file), and no suitable database can be
found for the new buffer, this function could set it to be the
database used for the previous buffer, which could be beneficial
when jumping outside of current project"
  (unless (member func citre--after-jump-functions)
    (push func citre--after-jump-functions)))

;;;; APIs for upper components

(defun citre-get-backend-and-completions ()
  "Try getting completions of symbol at point.
Backends in `citre-completion-backends' are tried in turn.  The
first succeeded backend and the results are returned in a cons
pair."
  (citre--try-func-in-backend-list
   'get-completions-func
   citre--completion-backends-table citre-completion-backends))

(defun citre-get-completions ()
  "Get completions of symbol at point.
The result is a list (BEG END TAGS), see
`citre-register-completion-backends'."
  (when-let ((result (citre-get-backend-and-completions)))
    (cdr result)))

(defun citre-get-backend-and-definitions ()
  "Try getting definitions of symbol at point.
Backends in `citre-find-definition-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-func-in-backend-list
   'get-definitions-func
   citre--find-definition-backends-table citre-find-definition-backends))

(defun citre-get-definitions ()
  "Get definitions of symbol at point."
  (when-let ((result (citre-get-backend-and-definitions)))
    (cdr result)))

(defun citre-get-backend-and-references ()
  "Try getting references of symbol at point.
Backends in `citre-find-reference-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-func-in-backend-list
   'get-references-func
   citre--find-reference-backends-table citre-find-reference-backends))

(defun citre-get-references ()
  "Get references of symbol at point."
  (when-let ((result (citre-get-backend-and-references)))
    (cdr result)))

(defun citre-get-backend-and-tags-in-buffer ()
  "Try getting tags in buffer using `citre-tags-in-buffer-backends'.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-func-in-backend-list
   'get-tags-in-buffer-func
   citre--tags-in-buffer-backends-table citre-tags-in-buffer-backends))

(defun citre-get-tags-in-buffer ()
  "Get tags in buffer."
  (when-let ((result (citre-get-backend-and-tags-in-buffer)))
    (cdr result)))

(defun citre-get-backend-and-id-list ()
  "Try getting a list of identifiers using `citre-find-definition-backends'.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-func-in-backend-list
   'identifier-list-func
   citre--find-definition-backends-table citre-find-definition-backends))

(defun citre-get-definitions-of-id (id &optional backend)
  "Get definitions of identifier ID using BACKEND.
Returns a list of tags.  If BACKEND is nil, try backends in
`citre-find-definition-backends'."
  (if backend
      (funcall (citre--get-prop-of-backend
                backend 'get-definitions-for-id-func
                citre--find-definition-backends-table)
               id)
    (cdr (citre--try-func-in-backend-list
          'get-definitions-for-id-func citre--find-definition-backends-table
          citre-find-definition-backends id))))

(defun citre-get-symbol-at-point-for-backend (backend)
  "Get symbol at point using BACKEND.
It uses `citre--symbol-at-point-backends-table' internally and
returns a string or nil."
  (funcall (citre--get-prop-of-backend backend 'symbol-at-point-func
                                       citre--symbol-at-point-backends-table)))

(defun citre-backend-usable-p (backend)
  "Check if BACKEND is usable for current buffer."
  (funcall (citre--get-prop-of-backend backend 'backend-usable-func
                                       citre--backend-usable-probe-table)))

(defun citre-after-jump-action (buffer)
  "Run the actions registered by `citre-register-after-jump-function'.
BUFFER should be the buffer before jumping."
  (dolist (f citre--after-jump-functions)
    (funcall f buffer)))

(provide 'citre-backend-interface)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-backend-interface.el ends here
