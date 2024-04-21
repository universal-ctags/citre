;;; citre-tags.el --- Tags file backend -*- lexical-binding: t -*-

;; Copyright (C) 2024 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 14 April 2024
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

;; Xref backend adapter to convert an xref backend to a Citre backend.  It also
;; bundles an eglot backend as an example.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-backend-interface)
;; We only use `citre-tags--symbol-at-point' in this module.  TODO: We really
;; shouldn't need this dependency.  We should extract a module for getting
;; symbol at point.
(require 'citre-tags)
(require 'cl-lib)
(require 'xref)

;;;; User Options

;;;; Internals

(defun citre--get-tags-by-xref-method
    (method backend &optional symbol-atpt-fn)
  "Find tags for identifier at point by xref backend BACKEND using METHOD.
METHOD is a `xref-backend-*' function.  The result is a list of
tags, or nil if tags couldn't be found or created.

When SYMBOL-ATPT-FN is non-nil, the symbol name captured by this
function is used as names of the tags."
  (let* ((xref-id (xref-backend-identifier-at-point backend))
         (id (if symbol-atpt-fn (funcall symbol-atpt-fn) xref-id))
         (id-no-props (substring-no-properties id))
         (objs (funcall method backend xref-id)))
    (cl-loop for obj in objs
             for tag = (citre-xref-object-to-tag obj id-no-props)
             if tag collect tag)))

(defun citre--xref-definitions (backend &optional symbol-atpt-fn)
  "Find definitions of identifier at point by xref backend BACKEND.
The result is a list of tags, or nil if definitions couldn't be
found or tags couldn't be created.

When SYMBOL-ATPT-FN is non-nil, the symbol name captured by this
function is used as names of the tags."
  (citre--get-tags-by-xref-method
   #'xref-backend-definitions backend symbol-atpt-fn))

(defun citre--xref-references (backend &optional symbol-atpt-fn)
  "Find references of identifier at point by xref backend BACKEND.
The result is a list of tags, or nil if references couldn't be
found or tags couldn't be created.

When SYMBOL-ATPT-FN is non-nil, the symbol name captured by this
function is used as names of the tags."
  (let ((tags (citre--get-tags-by-xref-method
               #'xref-backend-references backend symbol-atpt-fn)))
    (dolist (tag tags)
      (citre-set-tag-field 'extras "reference" tag))
    tags))

;;;; APIs

(cl-defun citre-xref-backend-to-citre-backend
    (backend usable-p &key symbol-atpt-fn after-jump-fn
             no-definition-backend no-reference-backend)
  "Convert xref backend BACKEND to Citre backend.
USABLE-P is a predicate telling if BACKEND is usable in the
buffer.

Citre backends for finding definitions and references are
registered, which can be disabled respectively by keyword
arguments NO-DEFINITION-BACKEND and NO-REFERENCE-BACKEND.

Some xref backends doesn't grab symbol at point correctly, e.g.,
symbols get by `eglot' are always \"LSP identifier at point\",
and the actual information is hidden in its text properties.  For
these backends, provide a SYMBOL-ATPT-FN to get the actual symbol
name at point.

When jumping to a definition, you may want to initialize the
backend to make it work for further jumping.  Provide a
AFTER-JUMP-FN for this."
  (citre-backend-create
   :usable-probe usable-p
   :symbol-at-point-fn (or symbol-atpt-fn
                           (lambda ()
                             (when (funcall usable-p)
                               (xref-backend-identifier-at-point backend))))
   :defs-fn (unless no-definition-backend
              (lambda ()
                (when (funcall usable-p)
                  (citre--xref-definitions backend symbol-atpt-fn))))
   :refs-fn (unless no-reference-backend
              (lambda ()
                (when (funcall usable-p)
                  (citre--xref-references backend symbol-atpt-fn))))
   :after-jump-fn after-jump-fn))

;;;; Implemented xref adapters

;;;;; Eglot

(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-ensure "eglot" ())

(defun citre--eglot-usable-p ()
  "See if eglot is usable in current buffer."
  (when (featurep 'eglot)
    (eglot-managed-p)))

(defun citre--eglot-after-jump ()
  "Make sure eglot is enabled after jump."
  (eglot-ensure)
  ;; It seems eglot is actually made usable in `post-command-hook'.  We need
  ;; this as `citre-peek-through' may get defs/refs in a new buffer without
  ;; executing any command.
  (run-hooks 'post-command-hook))

(defvar citre-eglot-backend
  (citre-xref-backend-to-citre-backend
   'eglot #'citre--eglot-usable-p
   :symbol-atpt-fn #'citre-tags-symbol-at-point
   :after-jump-fn #'citre--eglot-after-jump)
  "Eglot backend.")

(citre-register-backend 'eglot citre-eglot-backend)

(provide 'citre-xref-adapter)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-xref-adapter.el ends here
