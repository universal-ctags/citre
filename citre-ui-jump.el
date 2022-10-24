;;; citre-ui-jump.el --- UI library of the "jump" tool -*- lexical-binding: t -*-

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

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common-tag)
(require 'citre-common-util)
(require 'ring)

;;;; User Options

(defcustom citre-jump-select-item-function
  #'citre-jump-completing-read
  "The function for the user to select an item from a list.
It receives 2 arguments:

- A list of one or more strings representing
  definitions/references.  The function should let the user
  choose one in it.  The list is guaranteed to have one or more
  elements.  When there is only one element, the function can
  decide to let the user confirm, or return it directly.
- A string of the symbol name that's interested in.  The function
  can show it to the user.

See `citre-jump-completing-read' for an example of
implementation."
  :type 'function
  :group 'citre)

(make-obsolete 'citre-jump-select-definition-function
               'citre-jump-select-item-function
               "0.3")

;;;; Internals

(defvar citre-jump--marker-ring (make-ring 50)
  "The marker ring used by `citre-jump'.")

(defun citre-jump-completing-read (items symbol)
  "Select an item in ITEMS, with SYMBOL as a prompt.
This uses the `completing-read' interface.  See
`citre-jump-select-item-function' for the use of this function."
  (pcase (length items)
    (1 (car items))
    (_ (let ((collection
              (lambda (str pred action)
                (if (eq action 'metadata)
                    '(metadata
                      (category . citre-jump)
                      (cycle-sort-function . identity)
                      (display-sort-function . identity))
                  (complete-with-action action items str pred)))))
         (completing-read (format "%s: " symbol) collection nil t)))))

;;;; API

(defun citre-jump-show (tags &optional symbol marker root)
  "Show TAGS using `citre-jump' UI.
SYMBOL is shown as the prompt.  If it's nil, use the `name' field
in the first tag in TAGS as the symbol.

If MARKER is non-nil, push it into the history so we can go back
to it using `citre-jump-back'.  If it's nil, the marker at point
is used as MARKER.  If it's `none', don't push it to the history.

If ROOT is non-nil, show paths relative to ROOT.  If it's nil,
use `citre-project-root' to deduce the project root and show
paths relative to it.  If it's `none', show absolute paths."
  (when tags
    (let* ((symbol (or symbol (citre-get-tag-field 'name (car tags))))
           (marker (if (eq marker 'none) nil
                     (or marker (point-marker))))
           (root (if (eq root 'none) nil
                   (or root (citre-project-root))))
           (loc-alist
            (mapcar (lambda (tag)
                      (cons (citre-make-tag-str
                             tag nil
                             '(annotation)
                             `(location :suffix ":" :root ,root)
                             '(content :ensure t))
                            tag))
                    tags))
           (locations (mapcar #'car loc-alist)))
      (citre-goto-tag (alist-get
                       (funcall citre-jump-select-item-function
                                locations symbol)
                       loc-alist nil nil #'equal))
      (when marker (ring-insert citre-jump--marker-ring marker)))))

;;;; Command

(defun citre-jump-back ()
  "Go back to the position before last `citre-jump'."
  (interactive)
  (let ((ring citre-jump--marker-ring))
    (when (ring-empty-p ring)
      (user-error "No more previous history"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil)
      (run-hooks 'citre-after-jump-hook))))

(provide 'citre-ui-jump)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-ui-jump.el ends here
