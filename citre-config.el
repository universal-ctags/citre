;;; citre-config.el --- Default config for Citre -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 14 Feb 2021
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

;; This is a default configuration for Citre.  It enables all language support
;; that Citre provides.  You can use this as a reference and write your own
;; config.

;;; Code:

;;;; Auto enabling `citre-mode'

(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

;;;; Language supports

(with-eval-after-load 'cc-mode (require 'citre-lang-c))
(with-eval-after-load 'dired (require 'citre-lang-fileref))
(with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

;;;; Autoload

;; These functions are set to autoload from "citre-peek.el" and
;; "citre-basic-tools.el", and these files don't require `citre'.  So if lazy
;; load is used, `citre' is never required. This can happen when `:defer t' is
;; used in `use-package', and the user puts their config in `:config' block.
;; Those config won't be executed.  So, we manually autoloads them, and ask the
;; user to put `(require 'citre-config)' in `:init' block, for lazy load to
;; work.

;; If you don't use `citre-config' and you write your own config, and:
;;
;; - You use simple `require' to load `citre', no `use-package' or other lazy
;;   load tricks.
;; - Or you load `citre-peek' and `citre-basic-tools' directly, not using
;;   `citre'.
;;
;; Then you probably don't need these in your config.

(autoload 'citre-update-tags-file "citre" nil t)
(autoload 'citre-update-this-tags-file "citre" nil t)
(autoload 'citre-edit-tags-file-recipe "citre" nil t)
(autoload 'citre-create-tags-file "citre" nil t)
(autoload 'citre-jump "citre" nil t)
(autoload 'citre-mode "citre" nil t)
(autoload 'citre-peek "citre" nil t)
(autoload 'citre-ace-peek "citre" nil t)
(autoload 'citre-auto-enable-citre-mode "citre" nil t)

(provide 'citre-config)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-config.el ends here
