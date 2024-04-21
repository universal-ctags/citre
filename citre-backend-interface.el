;;; citre-backend-interface.el --- Interface of Citre backends -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.4
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

;; This file defines the Citre backend and offers APIs to use them.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common-tag)

;;;; User Options

(defcustom citre-completion-backends '(tags global)
  "List of completion backends.
When finding completions, these backends are tried in turn until
one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-find-definition-backends '(eglot tags global)
  "List of backends for finding definitions.
The backends are tried in turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-find-reference-backends '(eglot global)
  "List of backends for finding references.
The backends are tried in turn until one succeeded."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-identifier-list-backends '(global tags)
  "List of backends for getting all identifiers in current project.
The backends are tried in turn until one succeeded.  This is not
useful on its own, but provides completions for finding
definitions or references of a user inputted symbol.  It's best
to put backends that gets identifiers faster before the slower
ones."
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

(defvar citre-debug nil
  "Debug mode switch.
If this is nil, errors produced by backends are demoted.")

(defmacro citre--with-demoted-errors (format &rest body)
  "Demote errors produced by BODY if `citre-debug' is nil.
See `with-demoted-errors' for FORMAT."
  `(if citre-debug
       (with-demoted-errors ,format ,@body)
     ,@body))

(defvar citre--backends-table (make-hash-table :test #'eq :size 5)
  "Lookup table for Citre backends.
It's keys are symbols of backend names, values are of
`citre--backend' type.")

(cl-defstruct (citre--backend
               (:constructor nil)
               (:constructor
                citre-backend-create
                (&key usable-probe symbol-at-point-fn
                      completions-fn id-list-fn
                      defs-fn defs-of-id-fn
                      refs-fn refs-of-id-fn
                      tags-in-buffer-fn after-jump-fn))
               (:copier nil))
  "Citre backend.
Use `citre-backend-create' to create a backend, then use
`citre-register-backend' to register one.

`citre-backend-create' accpets key arguments. For detailed
explanation, see docstrings of data members of `citre--backend'.

If a backend could check whether itself is usable for a newly
opened file, provide `:usable-probe'.  This is used to
automatically enable `citre-mode'.

For auto-completion capability, provide `:completions-fn'.  This
function may want to utilize `citre-capf-optimize-for-popup', see
its docstring.

For finding definition capability, provide `:symbol-at-point-fn'
and `:defs-fn'.  If the backend is capable of finding definitions
of user inputted symbol, provide `:defs-of-id-fn'.  If the
backend can also find all symbols in current project, provide
`:id-list-fn', and xref could uses this as identifier completion
table. Based on how the backend works, you may also want to
provide `:after-jump-fn'.

Finding references are similar to finding definitions, only the
keyword names are changed to `:refs-fn' and `refs-of-id-fn'.

For the capability of finding tags in current buffer (which is
used by imenu), provide `:tags-in-buffer-fn'."
  (usable-probe
   nil
   :documentation
   "Detect if the backend is usable in current buffer.
It is called with no arguments, and should return nil when the
backend is not usable.")
  (symbol-at-point-fn
   nil
   :documentation
   "A function that returns symbol at point.
The symbol name is shown in various Citre UIs.  The function is
called with no arguments, and should return a string of the
symbol at point.  When there's no symbol at point, it should
return nil.

Depending on the behavior of `defs-fn' and `refs-fn' (see their
docstrings), this may return the marked text if there's an active
region."
   :type "function")
  (completions-fn
   nil
   :documentation
   "A function that returns completions of symbol at point.
It is called with no arguments, and should return a list
like (BEG END TAGS).  BEG and END is the region that the
completion should replace, and TAGS is a list of tags, which are
completions of symbol at point.  The tags should at least contain
`name' field, and optionally `ext-kind-full', `signature',
`pattern', `scope' and `typeref' fields, which allows the UI to
display more information.  If no completion is available, it
should return nil.")
  (id-list-fn
   nil
   :documentation
   "A function that returns all symbols in the current project.
It is called in the code buffer, with no arguments.  It should
return a list of strings, or nil if it can't figure out the
symbols.")
  (defs-fn
   nil
   :documentation
   "A function that returns definitions of symbol at point.
It is called with no arguments, and should return a list of tags,
which are definitions of the symbol at point.  The tags should at
least contain `name' and `ext-abspath' fields.  `line' and
`pattern' field are optional but could help locating the
definition more precisely.  `ext-kind-full', `typeref', `scope'
and `extras' fields allows the UI to display more information.
If no definition is available, it should return nil.

If it's possible, This should use the marked text as the symbol
at point when there's an active region, so the user could specify
the symbol in case it's not grabbed correctly.")
  ;; TODO: For now this is only used in xref. Could Citre use this to show
  ;; definitions of a user inputted symbol?
  (defs-of-id-fn
   nil
   :documentation
   "A function that returns definitions of a symbol name.
It is called in the code buffer, with an symbol name (a string)
as the argument , and should return the definition tags of the
symbol.")
  (refs-fn
   nil
   :documentation
   "Like `defs-fn', but returns references.
The returned tags should contain \"reference\" in their `extras'
field so Citre knows they are references.")
  (refs-of-id-fn
   nil
   :documentation
   "Like `defs-of-id-fn', but returns references.")
  (tags-in-buffer-fn
   nil
   :documentation
   "A function that returns a list of tags in current buffer.
It is called with no arguments.  `name' field is required in the
returned tags, `pattern' and/or `line' fields should also appear.
`kind' and `extras' fields helps imenu to classify the tags.
When no tags is available, it should return nil.")
  (after-jump-fn
   nil
   :documentation
   "A function that's called after jumping to a definition or reference.
Some backends requires certain initialization procedures, e.g.,
finding a database or connect to a server.  If the user jumps to
a location found by some backend, it's likely that the user will
continue to find definitions or references from that location, so
the backend should be initialized.

This is called with no arguments."))

(defun citre--get-backend (name)
  "Get a `citre--backend' by symbol NAME.
Returns nil if there's no such backend."
  (gethash name citre--backends-table))

(defun citre--call-backend-fn (name member &rest args)
  "Call function MEMBER from backend symbol NAME.
When the backend is not registered or the member is not defined,
return nil.  The errors produced by the function may be demoted
based on `citre-debug'."
  (when-let* ((backend (citre--get-backend name))
              (fn (funcall (intern (format "citre--backend-%s" member))
                           backend)))
    (citre--with-demoted-errors "Citre backend error: %S"
                                (apply fn args))))

(defun citre--try-fn-on-list (fn lst &rest args)
  "Call fn on LST until one returns non-nil.
A cons cell of that element in LST and the returned value of FN
is returned.  Elements in LST is used as the first argument of
FN, ARGS are the rest."
  (cl-dolist (e lst)
    (when-let ((v (apply fn e args)))
      (cl-return (cons e v)))))

;;;;; APIs for backends

(defun citre-register-backend (name backend)
  "Register a Citre backend.
NAME is a symbol, BACKEND is a created by `citre-backend-create'.
This ties the symbol with the backend, so the symbol could be
used in `citre-completion-backends',
`citre-find-definition-backends', `citre-find-reference-backends'
and `citre-tags-in-buffer-backends', and other Citre APIs."
  (puthash name backend citre--backends-table))

;;;; APIs for upper components

;;;;; APIs for tinkering with a single backend

(defun citre-backend-usable-p (backend)
  "Check if BACKEND is usable."
  (citre--call-backend-fn backend 'usable-probe))

(defun citre-backend-completions (backend)
  "Get completions of symbol at point by BACKEND."
  (citre--call-backend-fn backend 'completions-fn))

(defun citre-backend-symbol-at-point (backend)
  "Get symbol at point by BACKEND."
  (citre--call-backend-fn backend 'symbol-at-point-fn))

(defun citre-backend-id-list (backend)
  "Get identifiers in project by BACKEND."
  (citre--call-backend-fn backend 'id-list-fn))

(defun citre-backend-find-definition (backend)
  "Return definitions of symbol at point by BACKEND."
  (citre--call-backend-fn backend 'defs-fn))

(defun citre-backend-find-definition-of-id (backend id)
  "Return definitions of ID by BACKEND."
  (citre--call-backend-fn backend 'defs-of-id-fn id))

(defun citre-backend-find-reference (backend)
  "Return references of symbol at point by BACKEND."
  (citre--call-backend-fn backend 'refs-fn))

(defun citre-backend-find-reference-of-id (backend id)
  "Return references of ID by BACKEND."
  (citre--call-backend-fn backend 'refs-of-id-fn id))

(defun citre-backend-tag-in-buffer (backend)
  "Return tags in buffer by BACKEND."
  (citre--call-backend-fn backend 'tags-in-buffer-fn))

(defun citre-backend-after-jump (backend)
  "Call after-jump action of BACKEND."
  (citre--call-backend-fn backend 'after-jump-fn))

;;;;; APIs for getting completions, definitions, references and tags in buffer

(defun citre-get-backend-and-completions ()
  "Get completions of symbol at point.
Backends in `citre-completion-backends' are tried in turn.  The
first succeeded backend and the results are returned in a cons
pair."
  (citre--try-fn-on-list #'citre-backend-completions
                         citre-completion-backends))

(defun citre-get-completions ()
  "Get completions of symbol at point.
The result is a list (BEG END TAGS)."
  (cdr (citre-get-backend-and-completions)))

(defun citre-get-backend-and-id-list ()
  "Get a list of identifiers of current project.
`citre-find-definition-backends' and
`citre-find-reference-backends' are both tried.  The first
succeeded backend and the results are returned in a cons pair."
  (citre--try-fn-on-list #'citre-backend-id-list
                         citre-identifier-list-backends))

(defun citre-get-id-list ()
  "Get a list of identifiers of current project."
  (cdr (citre-get-backend-and-id-list)))

(defun citre-get-backend-and-definitions ()
  "Get definitions of symbol at point.
Backends in `citre-find-definition-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-fn-on-list #'citre-backend-find-definition
                         citre-find-definition-backends))

(defun citre-get-definitions ()
  "Get definitions of symbol at point.
The result is a list of tags."
  (cdr (citre-get-backend-and-definitions)))

(defun citre-get-backend-and-definitions-of-id (id)
  "Get definitions of identifier ID.
Backends in `citre-find-definition-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-fn-on-list #'citre-backend-find-definition-of-id
                         citre-find-definition-backends
                         id))

(defun citre-get-definitions-of-id (id)
  "Get definitions of identifier ID.
The result is a list of tags."
  (car (citre-get-backend-and-definitions-of-id id)))

(defun citre-get-backend-and-references ()
  "Get references of symbol at point.
Backends in `citre-find-reference-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-fn-on-list #'citre-backend-find-reference
                         citre-find-reference-backends))

(defun citre-get-references ()
  "Get references of symbol at point.
The result is a list of tags."
  (cdr (citre-get-backend-and-references)))

(defun citre-get-backend-and-references-of-id (id)
  "Get references of identifier ID.
Backends in `citre-find-reference-backends' are tried in turn.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-fn-on-list #'citre-backend-find-reference-of-id
                         citre-find-reference-backends
                         id))

(defun citre-get-references-of-id (id)
  "Get references of identifier ID.
The result is a list of tags."
  (car (citre-get-backend-and-references-of-id id)))

(defun citre-get-backend-and-tags-in-buffer ()
  "Try getting tags in buffer using `citre-tags-in-buffer-backends'.
The first succeeded backend and the results are returned in a
cons pair."
  (citre--try-fn-on-list #'citre-backend-tag-in-buffer
                         citre-tags-in-buffer-backends))

(defun citre-get-tags-in-buffer ()
  "Get tags in buffer."
  (cdr (citre-get-backend-and-tags-in-buffer)))

(provide 'citre-backend-interface)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-backend-interface.el ends here
