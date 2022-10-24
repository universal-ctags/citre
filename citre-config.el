;;; citre-config.el --- Default config for Citre -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG

;; Author: Hao WANG <amaikinono@gmail.com>
;; Maintainer: Hao WANG <amaikinono@gmail.com>
;; Created: 14 Feb 2021
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

;; This is a default configuration for Citre.  It enables all language support
;; that Citre provides.  You can use this as a reference and write your own
;; config.

;;; Code:

;;;; Auto enabling `citre-mode'

;; This is autoloaded in citre.el so it's usable.
(declare-function citre-auto-enable-citre-mode "citre")
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

;;;; Language supports

(with-eval-after-load 'cc-mode (require 'citre-lang-c))
(with-eval-after-load 'dired (require 'citre-lang-fileref))
(with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

;;;; Autoload

;; These functions are defined in files that don't require `citre'.  So,
;; although they are marked with the magic ;;;###autoload comment, they will
;; load the files they belong to, and `citre' is not loaded.

;; So, if lazy load is used, and user config is scheduled after loading `citre'
;; (e.g., by `with-eval-after-load' or the `:config' block in `use-package'
;; macro), Those config won't be executed by calling these commands.  So we
;; manually autoloads them, and ask the user to run (require 'citre-config) in
;; the init file, before loading citre (e.g., in the `:init' block in
;; `use-package' macro).

;; If you don't use `citre-config' and you write your own config, and you use
;; simple `require' to load `citre', no `use-package' or other lazy load
;; tricks, then you probably don't need these in your config.

(autoload 'citre-update-tags-file "citre" nil t)
(autoload 'citre-update-this-tags-file "citre" nil t)
(autoload 'citre-edit-tags-file-recipe "citre" nil t)
(autoload 'citre-create-tags-file "citre" nil t)
(autoload 'citre-global-create-database "citre" nil t)
(autoload 'citre-global-update-database "citre" nil t)

(provide 'citre-config)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-config.el ends here
