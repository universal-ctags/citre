;;; citre-config.el --- Default config for Citre -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 14 Feb 2021
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

;; This is a default configuration for Citre.  It enables all language support
;; that Citre provides.  You can use this as a reference and write your own
;; config.

;;; Code:

(require 'citre)

(with-eval-after-load 'cc-mode (require 'citre-lang-c))
(with-eval-after-load 'dired (require 'citre-lang-fileref))

(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

(provide 'citre-config)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-config.el ends here
