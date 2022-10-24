;;; citre-lang-c.el --- Language support for C -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 28 Jan 2021
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

(require 'citre-tags)
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
                  (rx "#" (* space) "include"
                      (* space) (or "<" "\""))
                  (line-beginning-position))))
      (let* ((full-symbol (buffer-substring-no-properties
                           (car bounds) (cdr bounds)))
             ;; Path name can be used in #include directive, so the same header
             ;; can be referenced differently in different files.  We only keep
             ;; the non-directory part.  By doing so we can't match #includes
             ;; that uses paths, but we make use of binary search of readtags.
             (symbol (file-name-nondirectory full-symbol))
             ;; When SYMBOL and FULL-SYMBOL are not equal, we know we are using
             ;; path, and we want to get it to filter the input field of file
             ;; tags.
             (path (when (not (equal symbol full-symbol))
                     ;; Remove upper components above ../
                     ;; ../linux/foo.h => linux/foo.h
                     (if (string-match (rx (* "../") (group (* anything)))
                                       full-symbol)
                         (match-string 1 full-symbol)
                       full-symbol))))
        (citre-put-property symbol
                            'bounds (cons (- (cdr bounds) (length symbol))
                                          (cdr bounds))
                            'syntax 'header
                            'literal-path path)))))

(defun citre-lang-c--get-normal-symbol ()
  "Get non-header symbol at point."
  (when-let ((symbol (citre-tags-get-symbol-default)))
    (let* ((bounds (citre-get-property 'bounds symbol))
           syntax)
      (save-excursion
        (goto-char (car bounds))
        (when (looking-back (rx (or "->" ".")) (- (point) 2))
          (setq syntax 'member)))
      (save-excursion
        (goto-char (cdr bounds))
        (when (looking-at (rx (* " ") "("))
          (setq syntax (if (eq syntax 'member)
                           'callable-member
                         'function))))
      (save-excursion
        (goto-char (car bounds))
        (when (looking-back (rx symbol-start
                                (group (or "struct" "union" "enum" "goto"))
                                (+ space))
                            (line-beginning-position))
          (setq syntax (intern (match-string 1)))))
      (citre-put-property symbol 'syntax syntax))))

(defun citre-lang-c-get-symbol ()
  "Get symbol function for C."
  (or (citre-lang-c--get-header-at-point)
      (citre-lang-c--get-normal-symbol)))

;;;; Finding definitions

(defun citre-lang-c-definition-filter (symbol)
  "Filter for finding definitions of SYMBOL in C."
  (pcase (citre-get-property 'syntax symbol)
    ('header
     `(or
       ;; The header.
       ,(citre-readtags-filter-kind "file")
       ;; The references to the header.
       ,(citre-readtags-filter-kind "header")))
    (_ (citre-tags-definition-default-filter symbol))))

(defun citre-lang-c-definition-sorter (symbol)
  "Sorter for finding definitions of SYMBOL in C."
  `(<or>
    ,(citre-readtags-sorter
      citre-tags-sorter-arg-put-references-below)
    ;; Sort on the kinds.
    ,(pcase (citre-get-property 'syntax symbol)
       ('header
        (apply #'citre-readtags-sorter
               ;; Put the reference tags below.
               `(filter ,(citre-readtags-filter-kind "header") -)
               ;; For definitions (i.e., the file tags of the header), put
               ;; those with the right directory above others.
               (when-let* ((path (citre-get-property 'literal-path symbol)))
                 (list `(filter ,(citre-readtags-filter 'input path 'suffix)
                                +)))))
       ('member
        (citre-readtags-sorter (citre-tags-sorter-arg-put-kinds-above
                                '("member"))))
       ('callable-member
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above '("member"))
         ;; If a member is callable, its typeref field may include
         ;; "(*)" as substring.
         ;;
         ;; e.g. f in
         ;; ---
         ;; struct s {
         ;;    int (*f) (void);
         ;; };
         ;; ---
         ;; is tagged like:
         ;;
         ;; f	input.c	2;"	...typeref:typename:int (*)(void)
         ;; +-----------------------------------------^^^
         ;; `- This is the clue for finding callable members.
         `(filter ,(citre-readtags-filter 'typeref "(*)" 'substr) +)))
       ('function
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above '("function" "macro"))))
       ('goto
        (citre-readtags-sorter (citre-tags-sorter-arg-put-kinds-above
                                '("label"))
                               ;; Several languages defines "label" kind, and
                               ;; we should put labels in C above others.  We
                               ;; do this for C++ too as ctags considers header
                               ;; files to be C++ source files.
                               `(filter ,(citre-readtags-filter-lang "C") +)
                               `(filter ,(citre-readtags-filter-lang "C++")
                                        +)))
       ((and (or 'struct 'union 'enum)
             keyword)
        ;; If a symbol comes after keywords "struct", "union", or "enum",
        ;; the symbol must a name of struct tag, union tag or enum tag.
        ;; We utilize this fact to imporve the order of the list.
        ;;
        ;; e.g. input for ctags:
        ;; --
        ;; struct point { /* struct tag */
        ;;    float x;
        ;;    float y;
        ;; };
        ;;
        ;; struct game {
        ;;    ...
        ;;    score point; /* member */
        ;;    ...
        ;; };
        ;; --
        ;;
        ;; Consider your cursor is at | in the following buffer content:
        ;; --
        ;; struct rectangle {
        ;;    struct po|int a, b;
        ;; };
        ;; --
        ;; When a user presses M-., the user expects citre gives
        ;; the struct tag higher priority than the member in the xref
        ;; list though the struct tag and the member has the same name,
        ;; "point".
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above (list (symbol-name keyword)))))
       ;; Don't sort for other syntax.
       (_ 0))
    ,(citre-readtags-sorter 'input '(length name +) 'name
                            citre-tags-sorter-arg-size-order)))

;;;; Auto-completion

(defun citre-lang-c-completion-filter (symbol)
  "Filter for auto-completing SYMBOL in C."
  (pcase (citre-get-property 'syntax symbol)
    ('header
     citre-tags-filter-file-tags)
    (_
     (citre-tags-completion-default-filter symbol))))

(defun citre-lang-c-completion-sorter (symbol)
  "Sorter for auto-completing SYMBOL in C."
  `(<or>
    ,(pcase (citre-get-property 'syntax symbol)
       ('member
        (citre-readtags-sorter (citre-tags-sorter-arg-put-kinds-above
                                '("member"))))
       ('callable-member
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above '("member"))
         ;; See the comment in `citre-lang-c-definition-sorter'.
         `(filter ,(citre-readtags-filter 'typeref "(*)" 'substr) +)))
       ('function
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above '("function" "member"))))
       ('goto
        (citre-readtags-sorter (citre-tags-sorter-arg-put-kinds-above
                                '("label"))))
       ((and (or 'struct 'union 'enum)
             keyword)
        ;; See the comment in `citre-lang-c-definition-sorter'.
        (citre-readtags-sorter
         (citre-tags-sorter-arg-put-kinds-above (list (symbol-name keyword)))))
       (_ 0))
    ,citre-tags-completion-default-sorter))

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

(citre-tags-register-language-support 'c-mode citre-lang-c-plist)

(provide 'citre-lang-c)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-lang-c.el ends here
