;;; citre.el --- Ctags IDE on the True Editor -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.1.3
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

;; Citre is an advanced Ctags (or actually, readtags) frontend for Emacs.  It
;; offers:

;; - auto-completion (by `completion-at-point').
;; - xref and imenu integration.
;; - `citre-jump', a `completing-read' UI for jumping to definition.
;; - `citre-peek', a powerful code reading tool that lets you go down the
;;   rabbit hole without leaving current buffer.

;; Read README.md to know more about Citre.  It also points you to a detailed
;; user manual.  If you haven't received such a file, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;;;; Libraries

(require 'citre-basic-tools)
(require 'citre-ctags)
(require 'citre-peek)

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre.el ends here
