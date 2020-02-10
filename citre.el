;;; citre.el --- Ctags IDE on the true editor -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/AmaiKinono/ctags-ide
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

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'project)
(require 'cl-lib)

;;;; User options

(defgroup ctags-ide nil
  "Code navigation, completion and help message based on ctags."
  :group 'convenience
  :group 'tools
  :prefix "ctags-ide-"
  :link '(url-link "https://github.com/AmaiKinono/ctags-ide"))

(defcustom ctags-ide-project-denoter-files
  '(".projectile" ".dumbjump" "Makefile" "makefile")
  "List of project denoter files.
If automatic detection for project root fails, put a file with one of these
names in your project root.  The list is in descending order of priority."
  :type '(repeat string))

(defcustom ctags-ide-project-root nil
  "Absolute root directory of current project.
Set this in your .dir-locals.el if automatic detection fails, and for some
reason you can't put a denoter file in the project root."
  :type '(choice (const nil) string))

(make-variable-buffer-local 'ctags-ide-project-root)

;;;; Utilities

(defun ctags-ide--project-root ()
  "Find project root of current file in buffer.
If `ctags-ide-project-root' is set, return it.  Otherwise search up directory
hierarchy for a file in `ctags-ide-project-denoter-files'.  If this fails, use
`project-current'.  If this also fails, use the directory of current file."
  (if ctags-ide-project-root
      ctags-ide-project-root
    (let ((current-file (buffer-file-name))
          (denoter-dir nil))
      (when current-file
        (cl-dolist (denoter ctags-ide-project-denoter-files)
          (setq denoter-dir (locate-dominating-file current-file denoter))
          (when denoter-dir
            (cl-return denoter-dir)))
        (or denoter-dir
            (cdr (project-current nil))
            (file-name-directory current-file))))))

;;;; Commands

(defun ctags-ide-show-project-root ()
  "Show project root of current file in buffer.
Use this command to see if ctags-ide detects the project root corectly."
  (interactive)
  (message (or (ctags-ide--project-root) "Buffer is not visiting a file")))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre.el ends here
