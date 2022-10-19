;;; citre-global.el --- Finding references using GNU Global in Citre -*- lexical-binding:t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 24 Sep 2021
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.2.1
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

;; citre-global is a GNU Global plugin for citre.  It offers commands to create
;; & update gtags database, commands similar to `citre-jump', `citre-peek' to
;; find references of a symbol using Global, and `xref-find-references'
;; integration.

;; Read the following doc to know how to use citre-peek:
;;
;; - docs/user-manual/citre-global.md

;; If you haven't received the doc, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-backend-interface)
;; We only use `citre-get-symbol' in this module.
(require 'citre-tags)
(require 'citre-ui-jump)
(require 'citre-ui-peek)

;;;; User Options

(defcustom citre-gtags-program nil
  "The name or path of the gtags program.
Set this if gtags is not in your PATH, or its name is not
\"gtags\"."
  :type '(set file (const nil))
  :group 'citre)

(defcustom citre-global-program nil
  "The name or path of the GNU Global program.
Set this if global is not in your PATH, or its name is not
\"global\"."
  :type '(set file (const nil))
  :group 'citre)

(defcustom citre-gtags-args '("--compact" "--objdir")
  "Arguments for running gtags.
On Windows, the \"--objdir\" argument may cause \"Objdir not
found\" error.  If this happens, you need to customize this
option to not use \"--objdir\", and gtags will always create
database in the project directory."
  :type '(repeat string)
  :group 'citre)

(defcustom citre-global-completion-case-sensitive t
  "Case sensitivity of auto-completion using global backend."
  :type 'boolean
  :group 'citre)

;;;; Global program interface

;;;;; Internals

(defvar citre-global--args
  '("--color=never"
    "--encode-path= :"
    "--result=grep"
    "--literal")
  "Arguments used for global command.
`citre-global--get-lines' further adds arguments on these.")

(defun citre-global--get-output-lines (args)
  "Get output from global program.
ARGS is the arguments passed to the program."
  (citre-get-output-lines
   (append (list (or citre-global-program "global"))
           args)))

(defun citre-global--get-lines (name &optional mode case-fold start-file)
  "Find tags related to NAME using global and return the outputed lines.
If MODE is

- `completion', find tags that are completions to NAME;
- `definition', find tags that are definitions of NAME;
- `reference', find tags that are references to NAME.

When CASE-FOLD is non-nil, do case-insensitive matching.  When
START-FILE is non-nil, sort the result by nearness (see the help
message of global) start from START-FILE."
  (let* ((name (when name (substring-no-properties name)))
         cmd)
    (push (or citre-global-program "global") cmd)
    (pcase mode
      ('completion (push "--completion" cmd))
      ('definition (push "--definition" cmd))
      ('reference (push "--reference" cmd)
                  (push "--symbol" cmd))
      (_ (error "Invalid MODE")))
    (when case-fold (push "--ignore-case" cmd))
    ;; Global doesn't know how to expand "~", so we need to expand START-FILE.
    (when start-file (push (concat "--nearness=" (expand-file-name start-file))
                           cmd))
    (setq cmd (append (nreverse cmd) citre-global--args
                      (list "--" name)))
    (citre-get-output-lines cmd)))

(defun citre-global--parse-path (path)
  "Translate escaped sequences in PATH.
The path should come from the output of global, with the
\"--encode-path\" option."
  (let ((last 0)
        (i nil)
        (parts nil))
    (while (setq i (string-match "%" path last))
      (push (substring path last i) parts)
      (push (char-to-string (string-to-number
                             (substring path (1+ i) (+ 3 i))
                             16))
            parts)
      (setq last (+ 3 i)))
    (push (substring path last) parts)
    (apply #'concat (nreverse parts))))

(defun citre-global--parse-line (line rootdir &optional name reference)
  "Parse a LINE in the output of global.
ROOTDIR is the working directory when running the global command.
The return value is a tag contains `ext-abspath' and `line'
fields.

If NAME is given, is used as the `name' field.

If REFERENCE is non-nil, \"reference\" is used as the `extras' field."
  (if (string-match (rx line-start
                        (group-n 1 (+ (not (any ":"))))
                        ":"
                        (group-n 2 (+ num))
                        ":")
                    line)
      (let ((path (match-string 1 line))
            (linum (match-string 2 line))
            (tag nil))
        ;; We don't record the pattern field since it's generate in real time,
        ;; so it can't be used to deal with file updates.
        (setq path (expand-file-name (citre-global--parse-path path) rootdir))
        (setq tag
              (citre-make-tag 'name (when name (substring-no-properties name))
                              'ext-abspath path
                              'line linum))
        (when reference (citre-set-tag-field 'extras "reference" tag))
        tag)
    (error "Invalid LINE")))

;;;;; API

(defvar-local citre--global-dbpath nil
  "Buffer-local cache for global database path.
See `citre-global-dbpath' to know how this is used.")

(defun citre-global-dbpath (&optional dir)
  "Get global database path.
This is the directory containing the GTAGS file.  When DIR is
non-nil, find database of that directory, otherwise find the
database of current directory."
  (pcase citre--global-dbpath
    ('none nil)
    ((and val (pred stringp) (pred citre-dir-exists-p)) val)
    (_ (let ((default-directory (or default-directory dir)))
         (condition-case nil
             (setq citre--global-dbpath
                   (car (citre-global--get-output-lines '("--print-dbpath"))))
           (error (setq citre--global-dbpath 'non)
                  nil))))))

(defun citre-global-clear-dbpath-cache ()
  "Clear the cache of buffer -> global database path.
Use this when a new database is created."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (kill-local-variable 'citre--global-dbpath))))

(defun citre-global-get-tags (&optional name mode case-fold start-file)
  "Get tags using global.
When NAME is non-nil, get tags for NAME, otherwise get
tags for the symbol under point.

See `citre-global--get-lines' for valid value of MODE.

When CASE-FOLD is non-nil, do case-insensitive matching.

By default, the result is sort by nearness (see the `--nearness'
option in global) start from the current file or directory.
START-FILE can be nil to keep this behavior, be a string to
specify the start file, or be a symbol (like `alpha') to use the
default alphabetical sort.

Global program is run under current `default-directory'."
  (when (citre-global-dbpath)
    (let ((name (or name (citre-tags-get-symbol)))
          (start-file
           (pcase start-file
             ('nil (with-selected-window (or (minibuffer-selected-window)
                                             (selected-window))
                     (or (buffer-file-name) default-directory)))
             ((pred stringp) start-file)
             ((pred symbolp) nil))))
      ;; `completion' mode needs special treatment as global just prints the
      ;; symbols in this mode, rather than printing in grep format as in other
      ;; modes.
      (if (eq mode 'completion)
          (mapcar (lambda (name)
                    (citre-make-tag 'name name))
                  (citre-global--get-lines
                   name mode case-fold start-file))
        (mapcar (lambda (line)
                  (citre-global--parse-line line default-directory name
                                            (eq mode 'reference)))
                (citre-global--get-lines
                 name mode case-fold start-file))))))

;;;; Tags file generating & updating

;;;;; Commands

;;;###autoload
(defun citre-global-create-database ()
  "Create gtags database."
  (interactive)
  (let* ((project (funcall citre-project-root-function))
         (default-directory
           (read-directory-name
            "I want to tag this dir using gtags: "
            project)))
    (make-process
     :name "gtags"
     :buffer (get-buffer-create "*citre-gtags*")
     :command (append (list (or citre-gtags-program "gtags"))
                      citre-gtags-args)
     :connection-type 'pipe
     :stderr nil
     :sentinel
     (lambda (proc _msg)
       (unwind-protect
           (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 (message "Finished tagging"))
                (s (user-error "Gtags exits %s.  See *citre-gtags* buffer"
                               s))))
             (s (user-error "Abnormal status of gtags: %s.  \
See *citre-gtags* buffer" s)))
         (citre-global-clear-dbpath-cache)))
     :file-handler t)
    (message "Tagging...")))

;;;###autoload
(defun citre-global-update-database ()
  "Update the gtags database in use.
If no database is found, prompt the user to create one."
  (interactive)
  (let ((prog (or citre-global-program "global")))
    (make-process
     :name "global"
     :buffer (get-buffer-create "*citre-global-update*")
     :command (list (or citre-global-program "global")
                    "--update")
     :connection-type 'pipe
     :stderr nil
     :sentinel
     (lambda (proc _msg)
       (pcase (process-status proc)
         ('exit
          (pcase (process-exit-status proc)
            (0 (message "Finished updating"))
            (_ (if (executable-find prog)
                   (when (y-or-n-p "Can't find database.  Create one? ")
                     (citre-global-create-database))
                 (user-error "Can't find global program")))))
         (s (user-error "Abnormal status of global: %s.  \
See *citre-global-update* buffer" s))))
     :file-handler t)
    (message "Updating...")))

;;;; Symbol at point

(citre-register-symbol-at-point-backend 'global #'citre-tags--symbol-at-point)

;;;; Completion backend

;; TODO: Do we need to cache the result like tags backend?
(defun citre-global-get-completions ()
  "Get tags of completions of symbol at point."
  (let ((tags (citre-global-get-tags
               nil 'completion (not citre-global-completion-case-sensitive)
               'alpha)))
    ;; Sort by length
    (sort tags (lambda (a b)
                 (< (length (citre-get-tag-field 'name a))
                    (length (citre-get-tag-field 'name b)))))))

(citre-register-completion-backend 'global #'citre-global-get-definitions)

;;;; Find definitions backend

(defun citre-global-get-definitions ()
  "Get tags of definitions to symbol at point."
  (citre-global-get-tags nil 'definition))

(citre-register-find-definition-backend 'global #'citre-global-get-definitions)

;;;; Find references backend

(defun citre-global-get-references ()
  "Get tags of references to symbol at point."
  (citre-global-get-tags nil 'reference))

(citre-register-find-reference-backend 'global #'citre-global-get-references)

;;;; Auto enable citre-mode

(citre-register-backend-usable-probe 'global #'citre-global-dbpath)

(provide 'citre-global)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-global.el ends here
