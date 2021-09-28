;;; citre-global.el --- Finding references using GNU Global in Citre -*- lexical-binding:t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 24 Sep 2021
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.2
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

(require 'citre-basic-tools)
(require 'citre-peek)
(require 'citre-util)

(defcustom citre-gtags-program nil
  "The name or path of the gtags program.
Set this if gtags is not in your PATH, or its name is not
\"gtags\"."
  :type '(set string (const nil))
  :group 'citre)

(defcustom citre-global-program nil
  "The name or path of the GNU Global program.
Set this if global is not in your PATH, or its name is not
\"global\"."
  :type '(set string (const nil))
  :group 'citre)

(defcustom citre-gtags-args '("--compact" "--objdir")
  "Arguments for running gtags.
On Windows, the \"--objdir\" argument may cause \"Objdir not
found\" error.  If this happens, you need to customize this
option to not use \"--objdir\", and gtags will always create
database in the project directory."
  :type '(repeat string)
  :group 'citre)

;;;; Global program interface

;;;;; Internals

(defvar citre-global--find-references-args
  '("--color=never"
    "--encode-path= :"
    "--result=grep"
    "--literal"
    "--reference"
    "--symbol")
  "Arguments used for finding references using global.
`citre-global--get-reference-lines' may add more arguments on
these.")

;; TODO: Make this a general API.
(defun citre-global--get-output-lines (args)
  "Get output from global program.
ARGS is the arguments passed to the program.

This is designed to allow local quit to terminate the process."
  ;; The implementation of this function is similar to `citre-core--get-lines'.
  (let* ((output-buf (get-buffer-create " *citre-global*"))
         inhibit-message
         proc exit-msg)
    (with-current-buffer output-buf
      (erase-buffer))
    (when (file-remote-p default-directory)
      (setq inhibit-message t))
    (let ((inhibit-quit t))
      (pcase (with-local-quit
               (catch 'citre-done
                 (setq proc
                       (make-process
                        :name "global"
                        :buffer output-buf
                        :command (append (list
                                          (or citre-global-program "global"))
                                         args)
                        :connection-type 'pipe
                        :stderr nil
                        :sentinel (lambda (_proc _msg)
                                    (throw 'citre-done t))
                        :file-handler t))
                 (while (sleep-for 30))))
        ('nil (set-process-sentinel proc #'ignore)
              (if (eq system-type 'windows-nt)
                  (signal-process proc 'sighup)
                (interrupt-process proc))
              nil)
        (_ (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 nil)
                (s (setq exit-msg (format "global exits %s\n" s)))))
             (s (setq exit-msg
                      (format "abnormal status of global: %s\n" s))))
           (let ((output (with-current-buffer output-buf (buffer-string))))
             (if exit-msg
                 (error (concat exit-msg output))
               (split-string output "\n" t))))))))

(defun citre-global--get-reference-lines (name &optional case-fold start-file)
  "Find references to NAME using global and return the outputed lines.
When CASE-FOLD is non-nil, do case-insensitive matching.  When
START-FILE is non-nil, sort the result by nearness (see the help
message of global) start from START-FILE."
  (let* ((name (when name (substring-no-properties name)))
         inhibit-message
         cmd)
    (when case-fold (push "--ignore-case" cmd))
    (push (or citre-global-program "global") cmd)
    ;; Global doesn't know how to expand "~", so we need to expand START-FILE.
    (when start-file (push (concat "--nearness=" (expand-file-name start-file))
                           cmd))
    (setq cmd (append (nreverse cmd) citre-global--find-references-args
                      (list "--" name)))
    (citre-get-output-lines cmd (get-buffer-create " *citre-global*")
                            'get-lines)))

(defun citre-global--read-path (path)
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

(defun citre-global--parse-line (line rootdir &optional name)
  "Parse a LINE in the output of global.
ROOTDIR is the working directory when running the global command.
The return value is a tag contains `ext-abspath', `line', and
`extras' field.  If NAME is given, is used as the `name' field.
The value of `extras' field is \"reference\"."
  (if (string-match (rx line-start
                        (group-n 1 (+ (not (any ":"))))
                        ":"
                        (group-n 2 (+ num))
                        ":")
                    line)
      (let ((path (match-string 1 line))
            (linum (match-string 2 line)))
        ;; We don't record the pattern field since it's generate in real time,
        ;; so it can't be used to deal with file updates.
        (setq path (expand-file-name (citre-global--read-path path) rootdir))
        (citre-make-tag 'name (when name (substring-no-properties name))
                        'ext-abspath path
                        'line linum
                        'extras "reference"))
    (error "Invalid LINE")))

;;;;; API

(defun citre-global-get-references (&optional name case-fold start-file)
  "Get reference tags using global.
When NAME is non-nil, get references of NAME, otherwise get
references of the symbol under point.

When CASE-FOLD is non-nil, do case-insensitive matching.

By default, the result is sort by nearness (see the `--nearness'
option in global) start from the current file or directory.
START-FILE can be nil to keep this behavior, be a string to
specify the start file, or be a symbol (like `alpha') to use the
default alphabetical sort.

Global program is run under current `default-directory'."
  (let ((name (or name (citre-get-symbol)))
        (start-file
         (pcase start-file
           ('nil (with-selected-window (or (minibuffer-selected-window)
                                           (selected-window))
                   (or (buffer-file-name) default-directory)))
           ((pred stringp) start-file)
           ((pred symbolp) nil))))
    (mapcar (lambda (line)
              (citre-global--parse-line line default-directory name))
            (citre-global--get-reference-lines name case-fold start-file))))

;;;; Tags file generating & updating

;;;;; Commands

;;;###autoload
(defun citre-global-create-database ()
  "Create gtags database."
  (interactive)
  (let* ((project (funcall citre-project-root-function))
         (default-directory
           (or (and citre-use-project-root-when-creating-tags
                    project)
               (read-directory-name
                "I want to tag this dir using gtags: "
                project))))
    (make-process
     :name "gtags"
     :buffer (get-buffer-create "*citre-gtags*")
     :command (append (list (or citre-gtags-program "gtags"))
                      citre-gtags-args)
     :connection-type 'pipe
     :stderr nil
     :sentinel
     (lambda (proc _msg)
       (pcase (process-status proc)
         ('exit
          (pcase (process-exit-status proc)
            (0 (message "Finished tagging"))
            (s (user-error "Gtags exits %s.  See *citre-gtags* buffer" s))))
         (s (user-error "Abnormal status of gtags: %s.  \
See *citre-ctags* buffer" s))))
     :file-handler t)
    (message "Tagging...")))

;;;###autoload
(defun citre-global-update-database ()
  "Update the gtags database in use."
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

;;;; `citre-jump' equivalent

;;;###autoload
(defun citre-jump-to-reference ()
  "Jump to the reference of the symbol at point.
This uses the `citre-jump' UI."
  (interactive)
  (let* ((marker (point-marker))
         (symbol (citre-get-symbol))
         (references
          (citre-global-get-references symbol))
         (root (funcall citre-project-root-function)))
    (when (null references)
      (user-error "Can't find references for %s" symbol))
    (citre-jump-show symbol references marker root)))

;;;; `citre-peek' equivalent

;;;;; Internals

(defun citre-global--peek-get-symbol-and-references ()
  "Return the symbol under point and references of it.
This is similar to `citre-peek--get-symbol-and-definitions'."
  (citre-peek--hack-buffer-file-name
    (let* ((symbol (or (citre-get-symbol)
                       (user-error "No symbol at point")))
           (references (or (citre-global-get-references symbol)
                           (user-error "Can't find references for %s"
                                       symbol))))
      (cons symbol references))))

;;;;; Commands

;;;###autoload
(defun citre-peek-references (&optional buf point)
  "Peek the references of the symbol in BUF and POINT.
When BUF or POINT is nil, it's set to the current buffer and
point."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (point (or point (point)))
         (symbol-refs (save-excursion
                        (with-current-buffer buf
                          (goto-char point)
                          (citre-global--peek-get-symbol-and-references))))
         (marker (if (buffer-file-name) (point-marker))))
    (citre-peek-show (car symbol-refs) (cdr symbol-refs) marker)))

;;;###autoload
(defun citre-ace-peek-references ()
  "Peek the references of a symbol on screen using ace jump.
This is similar to `citre-ace-peek'."
  (interactive)
  (when-let ((pt (citre-ace-pick-point)))
    (citre-peek-references (current-buffer) pt)))

;;;###autoload
(defun citre-peek-through-references ()
  "Peek through a symbol in current peek window for its references."
  (interactive)
  (when-let* ((buffer-point (citre-ace-pick-point-in-peek-window))
              (symbol-defs
               (save-excursion
                 (with-current-buffer (car buffer-point)
                   (goto-char (cdr buffer-point))
                   (citre-global--peek-get-symbol-and-references)))))
    (citre-peek-make-current-def-first)
    (citre-peek--make-branch (car symbol-defs) (cdr symbol-defs))))

;;;; `xref-find-references' integration

(defun citre-xref--global-find-reference (symbol)
  "Return the xref object of references of SYMBOL."
  (mapcar #'citre-xref--make-object
          (citre-global-get-references symbol)))

(cl-defmethod xref-backend-references ((_backend (eql citre)) symbol)
  "Define method for xref to find reference of SYMBOL."
  (citre-xref--global-find-reference symbol))

(provide 'citre-global)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-global.el ends here
