;; citre-lang-verilog.el --- Language support for (System) Verilog -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 27 July 2021
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

;; Language support for Verilog.  The features are:
;;
;; - Correctly grabbing the name of a macro reference.  When the symbol begins
;;   with a "`", it's dropped by Citre.
;; - When the symbol begins with a "`", tags of constant kind (which ctags uses
;;   to record macro definitions) are sorted above others.

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t.  To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Code:

;;;; Libraries

(require 'citre-tags)

;;;; Get symbol at point

(defun citre-lang-verilog-get-symbol ()
  "Get symbol function for (System) Verilog."
  (or (citre-tags-get-marked-symbol)
      (when-let* ((sym (citre-tags-get-symbol-at-point))
                  (bounds (citre-get-property 'bounds sym)))
        (let (syntax)
          (when (eq (aref sym 0) ?`)
            (setq sym (substring sym 1)
                  bounds (cons (1+ (car bounds)) (cdr bounds))
                  syntax 'macro))
          (citre-put-property sym 'bounds bounds 'syntax syntax)))))

;;;; Finding definitions

(defun citre-lang-verilog-definition-sorter (symbol)
  "Sorter for finding definitions of SYMBOL in (System) Verilog."
  (pcase (citre-get-property 'syntax symbol)
    ('macro
     (citre-readtags-sorter
      citre-tags-sorter-arg-put-references-below
      (citre-tags-sorter-arg-put-kinds-above '("constant"))
      'input '(length name +) 'name))
    (_ citre-tags-definition-default-sorter)))

;;;; Auto-completion

(defun citre-lang-verilog-completion-sorter (symbol)
  "Sorter for auto-completing SYMBOL in (System) Verilog."
  `(<or>
    ,(pcase (citre-get-property 'syntax symbol)
       ('macro
        (citre-readtags-sorter (citre-tags-sorter-arg-put-kinds-above
                                '("constant"))))
       (_ 0))
    ,citre-tags-completion-default-sorter))

;;;; Plugging into the language support framework

(defvar citre-lang-verilog-plist
  `(:get-symbol
    citre-lang-verilog-get-symbol
    :definition-sorter
    citre-lang-verilog-definition-sorter
    :completion-sorter
    citre-lang-verilog-completion-sorter)
  "(System) Verilog support for Citre.")

(citre-tags-register-language-support 'verilog-mode citre-lang-verilog-plist)

(provide 'citre-lang-verilog)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-lang-verilog.el ends here
