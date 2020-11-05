;;; readtags-xref.el --- readtags based xref backend -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
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

;;;; Libraries

(require 'citre-readtags)

;;;;; Tool: jump to definition (based on xref)

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(defun citre--make-xref-object (record)
  "Make xref object of RECORD."
  (let ((kind (citre-readtags-get-field 'kind record))
        (path (citre-readtags-get-field 'ext-abspath record))
        (line (citre-readtags-get-field 'line record))
        (line-content (citre-readtags-get-field 'line-content record)))
    (xref-make
     (concat
      (propertize kind 'face 'warning) " " line-content)
     (xref-make-file-location path line 0))))

(defun citre--xref-find-definition (symbol)
  "Return the xref object of the definition information of SYMBOL."
  (mapcar #'citre--make-xref-object
          (citre-get-definition-records nil symbol)))

(defun citre-xref-backend ()
  "Define the Citre backend for xref."
  'citre)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql citre)))
  "Define method for xref to get symbol at point."
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-definitions ((_backend (eql citre)) symbol)
  "Define method for xref to find definition of SYMBOL."
  (citre--xref-find-definition symbol))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql citre)))
  "Return a function for xref to find all completions of a prefix."
  (lambda (str pred action)
    (let ((collection
           (mapcar (lambda (record) (citre-readtags-get-field 'name record))
                   (citre-get-records nil "" nil :require '(name)))))
      (complete-with-action action collection str pred))))

(provide 'readtags-xref)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; readtags-xref.el ends here
