;;; citre-lang-c.el --- Language support for C -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 28 Jan 2021
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

;; Language support for C.  The features are:
;;
;; - When the symbol is after "->" or ".", tags of member kind are sorted above
;;   others.
;; - When the symbol is before "(", tags of function/macro kinds are sorted
;;   above others.
;; - When the cursor is on the header file in a "#include" directive, the
;;   header file itself, and the references to that header file (if tagged) is
;;   found as its definitions.  References that uses paths can't be found.
;;   Also, file names will be used for auto-completion.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-util)
(require 'rx)

;;;; Get symbol at point

(defun citre-lang-c--get-header-at-point ()
  "Get header at point."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'filename))))
    (when (and bounds
               (save-excursion
                 (goto-char (car bounds))
                 (looking-back
                  (rx "#include"
                      (* space) (or "<" "\""))
                  (line-beginning-position))))
      ;; Path name can be used in #include directive, so the same header can be
      ;; referenced differently in different files.  We only keep the
      ;; non-directory part.  By doing so we can't match #includes that uses
      ;; paths, but we make use of binary search of readtags.
      (let ((symbol (file-name-nondirectory
                     (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))))
        (citre-put-property symbol
                            'bounds (cons (- (cdr bounds) (length symbol))
                                          (cdr bounds))
                            'syntax 'header)))))

(defun citre-lang-c--get-normal-symbol ()
  "Get non-header symbol at point."
  (when-let ((symbol (citre-get-symbol-default)))
    (let* ((bounds (citre-get-property 'bounds symbol))
           syntax)
      (save-excursion
        (goto-char (car bounds))
        (when (looking-back (rx (or "->" ".")) (- (point) 2))
          (setq syntax 'member)))
      (unless syntax
        (save-excursion
          (goto-char (cdr bounds))
          (when (looking-at (rx (* " ") "("))
            (setq syntax 'function))))
      (citre-put-property symbol 'syntax syntax))))

(defun citre-lang-c-get-symbol ()
  "Get symbol function for C."
  (or (citre-lang-c--get-header-at-point)
      (citre-lang-c--get-normal-symbol)))

;;;; Finding definitions

(defun citre-lang-c-definition-filter (symbol)
  "Filter for finding definitions of SYMBOL in C."
  (let ((tagsfile (citre-get-property 'tags-file symbol)))
    (pcase (citre-get-property 'syntax symbol)
      ('header
       `(or
         ;; The header.
         ,(citre-core-filter-kind "file" tagsfile)
         ;; The references to the header.
         ,(citre-core-filter-kind "header" tagsfile)))
      (_ (citre-definition-default-filter symbol)))))

(defun citre-lang-c-definition-sorter (symbol)
  "Sorter for finding definitions of SYMBOL in C."
  (let* ((tagsfile (citre-get-property 'tags-file symbol)))
    `(<or>
      ;; Put reference below others.
      ,(citre-core-sorter
        `(filter ,(citre-core-filter 'extras "reference" 'csv-contain) -))
      ;; Sort on the kinds.
      ,(pcase (citre-get-property 'syntax symbol)
         ('header
          (citre-core-sorter
           `(filter ,(citre-core-filter-kind "header" tagsfile) -)))
         ('member
          (citre-core-sorter
           `(filter ,(citre-core-filter-kind "member" tagsfile) +)))
         ('function
          (citre-core-sorter
           `(filter (or ,(citre-core-filter-kind "function" tagsfile)
                        ,(citre-core-filter-kind "macro" tagsfile))
                    +)))
         ;; Don't sort for other syntax.
         (_ 0))
      ,(citre-core-sorter 'input '(length name +) 'name))))

;;;; Auto-completion

(defun citre-lang-c-completion-filter (symbol)
  "Filter for auto-completing SYMBOL in C."
  (pcase (citre-get-property 'syntax symbol)
    ('header
     (citre-core-filter-kind "file" (citre-get-property 'tags-file symbol)))
    (_
     (citre-completion-default-filter symbol))))

(defun citre-lang-c-completion-sorter (symbol)
  "Sorter for auto-completing SYMBOL in C."
  (let* ((tagsfile (citre-get-property 'tags-file symbol)))
    `(<or>
      ,(pcase (citre-get-property 'syntax symbol)
         ('member
          (citre-core-sorter
           `(filter ,(citre-core-filter-kind "member" tagsfile) +)))
         ('function
          (citre-core-sorter
           `(filter (or ,(citre-core-filter-kind "function" tagsfile)
                        ,(citre-core-filter-kind "macro" tagsfile))
                    +)))
         (_ 0))
      ,citre-completion-default-sorter)))

;;;; Plugging into the language support framework

(defvar citre-lang-c-plist
  `(:get-symbol
    citre-lang-c-get-symbol
    :definition-filter
    citre-lang-c-definition-filter
    :definition-sorter
    citre-lang-c-definition-sorter
    :completion-filter
    citre-lang-c-completion-filter
    :completion-sorter
    citre-lang-c-completion-sorter)
  "C language support for Citre.")

(setf (alist-get 'c-mode citre-language-support-alist)
      citre-lang-c-plist)

(provide 'citre-lang-c)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-lang-c.el ends here
