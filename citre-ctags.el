;;; citre-ctags.el --- Generate & update tags files -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2020
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

;; Tool for generating & updating tags files.  Read these docs to know how to
;; use it:

;; - README.md
;; - docs/user-manual/about-tags-file.md

;; If you haven't received these docs, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;;;; Libraries

(require 'citre-readtags)
(require 'cl-lib)

;;;; User options

;;;;; Find tags file

(defcustom citre-tags-file-names '(".tags" "tags")
  "List of tags file names.
These are searched up directory hierarchy from the file of
current buffer, or from `default-directory' in current buffer, to
decide which tags file to use.

This list is in descending order of priority (i.e., if we find
one, then the rest will be ignored)."
  :type '(repeat string)
  :group 'citre)

(make-obsolete 'citre-tags-files 'citre-tags-file-names "0.3")

(defcustom citre-tags-file-global-cache-dir "~/.cache/tags/"
  "An absolute directory where you can save all your tags files.
Tags files in it are named using the path to the directory in
which you want to use the tags file.

If you work on a remote machine, this points to directory on the
remote machine."
  :type 'directory
  :group 'citre)

(defcustom citre-tags-file-per-project-cache-dir "./.tags/"
  "A relative directory where you can save all your tags files in the projct.
This directory is expanded to the project root detected by
`citre-project-root-function', and when you are visiting files in
the project, this directory is searched for a tags file.

Tags files in it are named using the relative path to the
directory in which you want to use the tags file."
  :type 'directory
  :group 'citre)

(defcustom citre-tags-file-alist nil
  "Alist of directory -> tags file.
If current file in buffer is in one of the directories, the
corresponding tags file will be used.

This is a buffer-local variable so you can customize it on a
per-project basis.  Relative paths in it will be expanded against
the project root, which is detected by
`citre-project-root-function'.

The global (default) value of this still works as a fallback for
its buffer-local value.  So you can use `setq-default' to
customize this for directories where it's inconvenient to have
dir-local variables."
  :type '(alist :key-type directory :value-type file)
  :group 'citre)

;;;###autoload
(put 'citre-tags-file-alist 'safe-local-variable #'listp)
(make-variable-buffer-local 'citre-tags-file-alist)

;;;;; Create tags file

(defcustom citre-ctags-program nil
  "The name or path of the ctags program.
Citre requires ctags program provided by Universal Ctags.  Set
this if ctags is not in your PATH, or its name is not \"ctags\""
  :type 'file
  :group 'citre)

(defcustom citre-ctags-cmd-buf-default-cmd
  "ctags
-o
%TAGSFILE%
;; programming languages to be scanned, or \"all\" for all supported languages
--languages=all
--kinds-all=*
--fields=*
--extras=*
-R
;; add dirs/files to scan here, one line per dir/file
"
  "Default message in the ctags command line editing buffer."
  :type 'string
  :group 'citre)

(make-obsolete 'citre-edit-cmd-buf-default-cmd
               'citre-ctags-cmd-buf-default-cmd
               "0.3")

(defcustom citre-ctags-cmd-buf-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") 'citre-ctags-cmd-buf-add-lang)
    (define-key map (kbd "C-c f") 'citre-ctags-cmd-buf-add-dir-or-file)
    (define-key map (kbd "C-c C-c") 'citre-ctags-cmd-buf-commit)
    (define-key map (kbd "C-c C-k") 'citre-ctags-cmd-buf-cancel)
    map)
  "Keymap used in the ctags command editing buffer."
  :type 'keymap
  :group 'citre)

(make-obsolete 'citre-edit-cmd-buf-map
               'citre-ctags-cmd-buf-map
               "0.3")

(defcustom citre-default-create-tags-file-location nil
  "Default location to create a tags file.  Can be:
- nil: Ask me to select one of the following schemes.
- `in-dir': In the directory to use it.
- `global-cache': In `citre-tags-file-global-cache-dir'.
- `project-cache': In `citre-tags-file-per-project-cache-dir'.
- `manual': Ask me to pick a directory manually"
  :type '(choice (const :tag "Ask me to select a scheme below" nil)
                 (const :tag "In the directory to use it" in-dir)
                 (const :tag "In global cache dir" global-cache)
                 (const :tag "In per-project cache dir" project-cache)
                 (const :tag "Ask me to select a dir" manual))
  :group 'citre)

(defcustom citre-use-project-root-when-creating-tags nil
  "Non-nil means use project root when creating tags.
This means using the project root detected by
`citre-project-root-function' for:

- The directory in which Ctags runs
- The directory in which you want to use the tags file

and Citre will not prompt the user for these directories."
  :type 'boolean
  :group 'citre)

(defcustom citre-prompt-language-for-ctags-command nil
  "When non-nil, use a simpler way to edit ctags command.
This means when creating a tags file, don't use a buffer to edit
the command, but prompt the user to choose the languages and
generates a command that works for most projects.  In this
situation, Ctags will scan in the root dir it runs.

This requires the ctags program provided by Universal Ctags."
  :type 'boolean
  :group 'citre)

;;;; Helper functions

(defun citre--get-pseudo-tag-value (name &optional tagsfile)
  "Get the value field of pseudo tag NAME in TAGSFILE.
NAME should not start with \"!_\".

When TAGSFILE is nil, find it automatically."
  (when-let ((tagsfile (or tagsfile (citre-tags-file-path)))
             (ptag (citre-readtags-get-pseudo-tags name tagsfile)))
    (nth 1 (car ptag))))

;;;; Find tags file:  Internals

(defvar-local citre--tags-file nil
  "Buffer-local cache for tags file path.
See `citre-tags-file-path' to know how this is used.")

;;;;; By `citre-tags-file-alist'

(defun citre--find-tags-file-by-tags-file-alist (dir project alist)
  "Find the tags file of DIR by ALIST.
ALIST meets the requirements of `citre-tags-file-alist'.  DIR is
an absolute path.  Relative paths in the alist are expanded
against PROJECT, an absolute path."
  (let* ((dir (file-truename dir))
         (expand-file-name-against-project
          (lambda (file)
            (if (file-name-absolute-p file)
                ;; Convert ~/foo to /home/user/foo
                (expand-file-name file)
              (when project
                (expand-file-name file project))))))
    (cl-dolist (pair alist)
      (when-let ((target-dir (funcall expand-file-name-against-project
                                      (car pair)))
                 (target-tags (funcall expand-file-name-against-project
                                       (cdr pair))))
        (when (and (file-equal-p dir target-dir)
                   (citre-non-dir-file-exists-p target-tags))
          (cl-return target-tags))))))

;;;;; By `citre-tags-file-global/per-project-cache-dir'

(defun citre--path-to-cache-tags-file-name (path)
  "Encode PATH into a tagsfile name and return it.
PATH is canonical or relative to the project root.  It's where
you want to use the tags file.  The returned name can be used in
`citre-tags-file-global-cache-dir' or
`citre-tags-file-per-project-cache-dir' as tags file names."
  (when (file-name-absolute-p path)
    (setq path (expand-file-name path))
    ;; Check if it's a Windows path.  We don't use `system-type' as the user
    ;; may work on a remote Windows machine (people really do this?)
    (when (string-match "^[[:alpha:]]:" (file-local-name path))
      ;; We remove the colon after the disk symbol, or Emacs will think
      ;; "d:!project!path" is absolute and refuse to expand it against the
      ;; cache dir.
      (setq path (concat (or (file-remote-p path) "")
                         (char-to-string (aref path 0))
                         (substring path 2)))))
  ;; Escape backslashes
  (setq path (replace-regexp-in-string "\\\\" "\\\\\\&" path))
  ;; Escape exclamation marks
  (setq path (replace-regexp-in-string "!" "\\\\\\&" path))
  (concat (replace-regexp-in-string "/" "!" path) ".tags"))

(defun citre-tags-file-in-global-cache (dir)
  "Return the tags file name of DIR in global cache dir.
DIR is absolute.  The full path of the tags file is returned."
  (expand-file-name
   (citre--path-to-cache-tags-file-name (file-local-name (file-truename dir)))
   ;; TODO: We may want to put this in a function.
   (concat (or (file-remote-p default-directory) "")
           citre-tags-file-global-cache-dir)))

(defun citre-tags-file-in-per-project-cache (dir &optional project)
  "Return the tags file name of DIR in per-project cache dir.
DIR is absolute.  PROJECT is the absolute project root.  If it's
nil, it's detected by `citre-project-root-function'.  The full
path of the tags file is returned."
  (let ((project (or project (funcall citre-project-root-function)))
        (dir (file-truename dir)))
    (if project
        (progn
          (setq project (file-truename project))
          (expand-file-name
           (citre--path-to-cache-tags-file-name
            (file-relative-name dir project))
           (expand-file-name citre-tags-file-per-project-cache-dir project)))
      (error "Can't detect project root"))))

(defun citre--find-tags-file-in-cache-dirs (dir &optional project)
  "Find the tags file of DIR in cache dirs.
DIR is absolute.  PROJECT is the project root.  If it's nil, it's
detected by `citre-project-root-function'.

The full path of the tags file is returned."
  (let ((project (or project (funcall citre-project-root-function))))
    (cl-block nil
      ;; First search in per project cache dir.
      (when (and project citre-tags-file-per-project-cache-dir)
        (let ((tagsfile (citre-tags-file-in-per-project-cache dir project)))
          (when (citre-non-dir-file-exists-p tagsfile)
            (cl-return tagsfile))))
      ;; Then search in global cache dir.
      (when citre-tags-file-global-cache-dir
        (let ((tagsfile (citre-tags-file-in-global-cache dir)))
          (when (citre-non-dir-file-exists-p tagsfile)
            (cl-return tagsfile)))))))

;;;;; By `citre-tags-file-names'

(defun citre--find-tags-file-in-dir (dir)
  "Find the tags file of DIR by `citre-tags-file-names' in DIR.
DIR is an absolute path."
  (cl-dolist (file citre-tags-file-names)
    (let ((tags (expand-file-name file dir)))
      (when (and (citre-non-dir-file-exists-p tags)
                 (not (file-directory-p tags)))
        (cl-return tags)))))

;;;; Find tags file: APIs

(defun citre-tags-file-path ()
  "Return the canonical path of tags file for current buffer.
This finds the tags file up directory hierarchy, and for each
directory, it tries the following methods in turn:

- Use `citre-tags-file-alist'.
- Find in `citre-tags-file-cache-dirs'.
- See if one name in `citre-tags-file-names' exists in this dir.

The result is cached, and can be cleared by
`citre-clear-tags-file-cache'.  It also sets
`citre-readtags--tags-file-cwd-guess-table', so for tags file
without the TAG_PROC_CWD pseudo tag, we can better guess its root
dir."
  (pcase citre--tags-file
    ('none nil)
    ((and val (pred stringp) (pred citre-non-dir-file-exists-p)) val)
    (_ (let* ((current-dir (file-truename (citre-current-dir)))
              (project (funcall citre-project-root-function))
              (tagsfile nil))
         (while (and current-dir (null tagsfile))
           (setq tagsfile
                 (or (and (local-variable-p 'citre-tags-file-alist)
                          (citre--find-tags-file-by-tags-file-alist
                           current-dir project citre-tags-file-alist))
                     (and (default-value 'citre-tags-file-alist)
                          (citre--find-tags-file-by-tags-file-alist
                           current-dir nil (default-value
                                             'citre-tags-file-alist)))
                     (and (or citre-tags-file-global-cache-dir
                              citre-tags-file-per-project-cache-dir)
                          (citre--find-tags-file-in-cache-dirs
                           current-dir project))
                     (and citre-tags-file-names
                          (citre--find-tags-file-in-dir current-dir))))
           (unless tagsfile
             (setq current-dir (citre-directory-of current-dir))))
         (if tagsfile
             (progn
               (setq tagsfile (file-truename tagsfile))
               (puthash tagsfile current-dir
                        citre-readtags--tags-file-cwd-guess-table)
               ;; Only cache the result for file buffers, since non-file
               ;; buffers may change their own default directories, e.g., when
               ;; cd to another project.
               (when buffer-file-name
                 (setq citre--tags-file tagsfile))
               tagsfile)
           (when buffer-file-name
             (setq citre--tags-file 'none)
             nil))))))

(defun citre-read-tags-file-name ()
  "Prompt the user for an existing file.
This should be used for selecting a tags file.  When the current
buffer has a related tags file, it's used as the initial input."
  (let* ((current-tags-file (citre-tags-file-path))
         (dir (when current-tags-file
                (file-name-directory current-tags-file)))
         (initial (when current-tags-file
                    (file-name-nondirectory
                     current-tags-file))))
    (read-file-name "Tags file: " dir nil t initial)))

(defun citre-clear-tags-file-cache ()
  "Clear the cache of buffer -> tagsfile.
Use this when a new tags file is created."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (kill-local-variable 'citre--tags-file))))

;;;; Create tags file: Internals

;; A ctags command has 3 "faces":
;;
;; - Executed command: A list of arguments that's actually executed.
;; - In the ptag: Arguments separated by "|".  The path to the tags file is
;;   replaced by "%TAGSFILE%", and "|"s, "%"s, tabs in the args are escaped.
;; - In the command editing buffer: Arguments separated by newlines.  The path
;;   to the tags file is replaced by "%TAGSFILE%", and "%"s in the args (other
;;   than in "%TAGSFILE%") are escaped.

(defun citre--escape-ctags-cmd-exec-to-file (cmd)
  "Escape cmd arg CMD.
CMD is from a executable command, and is converted to the form in
CITRE_CMD ptag in a tags file."
  ;; Escape tabs, "%" and "|"
  (setq cmd (replace-regexp-in-string "\t" "\\\\t" cmd))
  (setq cmd (replace-regexp-in-string (rx (or "|" "%")) "\\\\\\&" cmd))
  cmd)

(defun citre--escape-ctags-cmd-buf-to-file (cmd)
  "Escape cmd arg CMD.
CMD is from the command editing buffer, and is converted to the
form in CITRE_CMD ptag in a tags file."
  ;; Escape tabs and "|".  We don't escape "%" since in edit command buffer we
  ;; explicitely ask for them to be manaully escaped.
  (setq cmd (replace-regexp-in-string "\t" "\\\\t" cmd))
  (setq cmd (replace-regexp-in-string "|" "\\\\\\&" cmd))
  cmd)

(defun citre--unescape-ctags-cmd-file-to-exec (cmd)
  "Unescape cmd arg CMD.
CMD is from the CITRE_CMD ptag, and is converted to an arg in an
executable command."
  ;; Unescape tabs, "%" and "|"
  (setq cmd (replace-regexp-in-string
             (rx (group (* "\\\\") "\\" "t"))
             "\\1t" cmd))
  (setq cmd (replace-regexp-in-string
             (rx (group (* "\\\\")) "\\" (group (or "%" "|")))
             "\\1\\2" cmd))
  cmd)

(defun citre--unescape-ctags-cmd-file-to-buf (cmd)
  "Unescape cmd arg CMD.
CMD is from the CITRE_CMD ptag, and is converted to an arg in the
command editing buffer."
  ;; Unescape tabs and "|"
  (setq cmd (replace-regexp-in-string
             (rx (group (* "\\\\") "\\" "t"))
             "\\1t" cmd))
  (setq cmd (replace-regexp-in-string
             (rx (group (* "\\\\")) "\\" "|")
             "\\1\\2" cmd))
  cmd)

(defun citre--replace-tagsfile-variable (arg tagsfile)
  "Replace \"%TAGSFILE%\" in ARG by local path of TAGSFILE.
This won't do anything if one of the \"%\"s is escaped."
  (replace-regexp-in-string
   ;; (rx (group (or line-start (not (any "\\"))) (* "\\\\"))
   ;;     "%TAGSFILE%")
   "\\(\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\)%TAGSFILE%"
   (concat "\\1" (citre--escape-ctags-cmd-exec-to-file
                  ;; Seems `make-process' doesn't know "~" in the commannd.
                  (expand-file-name (file-local-name tagsfile))))
   arg 'fixedcase))

(defun citre--ctags-cmd-ptag-to-exec (ptag tagsfile)
  "Convert PTAG into an executable command CMD (a list).
PTAG is the value of the CITRE_CMD ptag in TAGSFILE.  When
TAGSFILE is nil, this won't translate the \"%TAGSFILE%\" part in
PTAG."
  (let ((pos 0)
        last cmd)
    ;; Find unescaped "|"
    (while (progn (setq last pos)
                  (setq pos (string-match
                             ;; I have absolutely no idea why this fails on
                             ;; Emacs 26.

                             ;; (rx (or line-start (not (any "\\")))
                             ;;     (* "\\\\") "|")
                             "\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*|"
                             ptag pos)))
      ;; Jump over possible backslashes.
      (setq pos (string-match "|" ptag pos))
      (let ((c (substring ptag last pos)))
        ;; Translate %TAGSFILE%
        (when tagsfile
          (setq c (citre--replace-tagsfile-variable c tagsfile)))
        (setq c (citre--unescape-ctags-cmd-file-to-exec c))
        (push c cmd))
      ;; Move over the "|"
      (cl-incf pos))
    (push (substring ptag last) cmd)
    (setq cmd (nreverse cmd))
    (when (equal (nth 0 cmd) "ctags")
      (setf (nth 0 cmd) (or citre-ctags-program "ctags")))
    cmd))

(defun citre--ctags-cmd-ptag-to-buf (ptag)
  "Convert PTAG to command for inserting into an editing bufer.
PTAG is the value of the CITRE_CMD ptag in TAGSFILE"
  (let ((pos 0)
        last cmd)
    ;; Find unescaped "|"
    (while (progn (setq last pos)
                  (setq pos (string-match
                             ;; (rx (or line-start (not (any "\\")))
                             ;;     (* "\\\\") "|")
                             "\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*|"
                             ptag pos)))
      ;; Jump over possible backslashes.
      (setq pos (string-match "|" ptag pos))
      (push (citre--unescape-ctags-cmd-file-to-buf (substring ptag last pos))
            cmd)
      ;; Move over the "!"
      (cl-incf pos))
    (push (substring ptag last) cmd)
    (string-join (nreverse cmd) "\n")))

(defun citre--ctags-cmd-buf-to-ptag ()
  "Generate CITRE_CMD ptag from current command editing buffer."
  (let (cmd)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at (rx (or ";;" (seq (* space) line-end)))) nil)
         (t (push (citre--escape-ctags-cmd-buf-to-file
                   (buffer-substring (line-beginning-position)
                                     (line-end-position)))
                  cmd)))
        (forward-line)))
    (string-join (nreverse cmd) "|")))

(defun citre--ctags-cmd-ptag-from-languages ()
  "Read languages, return a CITRE_CMD ptag.
This requires ctags program provided by Universal Ctags.  The
generated command should work for most projects"
  (let* ((langs (with-temp-buffer
                  (process-file (or citre-ctags-program "ctags")
                                nil (current-buffer) nil
                                "--list-languages")
                  (split-string (buffer-string) "\n" t)))
         (langs (completing-read-multiple
                 "Choose languages. Empty input means scan all languages. "
                 langs)))
    (concat "ctags|-o|%TAGSFILE%|"
            (if langs (concat "--languages=" (string-join langs ",") "|") "")
            "--kinds-all=*|--fields=*|--extras=*|-R")))

(defun citre--write-ctags-recipe (tagsfile cmd-ptag cwd)
  "Write recipe to TAGSFILE.
CMD-PTAG is the value of CITRE_CMD ptag, CWD is the working
directory of Ctags.  It's expanded and convert to a local path."
  (citre-readtags-write-pseudo-tag
   tagsfile "CITRE_CMD" cmd-ptag
   "command line to generate this tags file")
  (setq cwd (file-local-name (expand-file-name cwd)))
  ;; Ctags on windows generates disk symbol in capital letter.
  (when (<= ?a (aref cwd 0) ?z)
    (setq cwd (citre-upcase-first-letter cwd)))
  (citre-readtags-write-pseudo-tag
   tagsfile "TAG_PROC_CWD" (file-local-name cwd)
   "dir in which ctags runs"))

;;;;; Edit tags file generation recipe

(defvar citre-ctags-cmd-buf-help-msg
  ";; Edit the command line for creating the tags file
;;
;; Syntax:
;;
;; - One command line argument in one line
;; - Lines start with ;; are ignored
;; - Use %TAGSFILE% to refer to the tags file
;; - \"%\" (other than those in %TAGSFILE%) needs escaping
;;
;; Commands:
;;
;; - \\[citre-ctags-cmd-buf-add-lang]: Insert a language (needs Universal \
Ctags)
;; - \\[citre-ctags-cmd-buf-add-dir-or-file]: Insert a dir or file
;; - \\[citre-ctags-cmd-buf-commit]: Commit
;; - \\[citre-ctags-cmd-buf-cancel]: Cancel

"
  "Help message in the command line editing buffer.")

(defvar-local citre--ctags-cmd-buf-cwd nil
  "The cwd of ctags program, recorded in the edit cmd buffer.")

(defvar-local citre--ctags-cmd-buf-tagsfile nil
  "The tagsfile, recorded in the edit cmd buffer.")

(defvar-local citre--ctags-cmd-buf-callback nil
  "The callback function, recorded in the edit cmd buffer.")

(defvar-local citre--ctags-cmd-buf-prev-buf nil
  "Previous buffer before switching to edit cmd buffer.")

(defun citre--read-ctags-cwd (&optional tagsfile)
  "Prompt the user to choose cwd for Ctags command.
When TAGSFILE is non-nil and TAG_PROC_CWD ptag is found in it,
use it as the default directory.

The full path is returned."
  (let (cwd)
    (when (and tagsfile
               (citre-non-dir-file-exists-p tagsfile)
               (setq cwd (citre--get-pseudo-tag-value "TAG_PROC_CWD"
                                                      tagsfile)))
      (when-let (remote-id (file-remote-p tagsfile))
        (setq cwd (concat remote-id cwd))))
    (unless cwd (setq cwd (funcall citre-project-root-function)))
    (expand-file-name
     (read-directory-name "Root dir to run ctags: " cwd))))

(defun citre--read-ctags-cwd-and-cmd (callback &optional tagsfile cwd)
  "Read the root dir (cwd) and command to generate a tags file.
If TAGSFILE is non-nil and there's a CITRE_CMD ptag in it,
initialize the command editing buffer using this existing ptag,
otherwise using `citre-edit-cmd-buf-default-cmd'.

If CWD is non-nil, don't ask the user to select a root dir to run
ctags, but use CWD.

CALLBACK is called when the user commits in the command editing
buffer.  It's called with 3 args:
- The tagsfile (if TAGSFILE is nil, it's nil.)
- The cwd.
- The CITRE_CMD ptag to be written into the tags file."
  (let (cmd)
    (unless cwd
      (setq cwd (citre--read-ctags-cwd)))
    (when (and tagsfile
               (citre-non-dir-file-exists-p tagsfile)
               (setq cmd (citre--get-pseudo-tag-value "CITRE_CMD" tagsfile)))
      (setq cmd (citre--ctags-cmd-ptag-to-buf cmd)))
    (let ((buf (current-buffer)))
      (pop-to-buffer (generate-new-buffer "*ctags-command-line*")
                     '(display-buffer-same-window))
      (text-mode)
      (setq citre--ctags-cmd-buf-prev-buf buf))
    (let ((map (copy-keymap citre-ctags-cmd-buf-map)))
      (set-keymap-parent map (current-local-map))
      (use-local-map map))
    (setq citre--ctags-cmd-buf-tagsfile tagsfile)
    (setq citre--ctags-cmd-buf-cwd cwd)
    (setq citre--ctags-cmd-buf-callback callback)
    (insert (substitute-command-keys citre-ctags-cmd-buf-help-msg))
    (if cmd (insert cmd) (insert citre-ctags-cmd-buf-default-cmd))))

(defun citre-ctags-cmd-buf-add-dir-or-file ()
  "Insert a directory or file in the command editing buffer.
When it's in the cwd, it's converted to relative path."
  (interactive)
  (let ((dir (read-file-name "Dir: " citre--ctags-cmd-buf-cwd)))
    (if (file-in-directory-p dir citre--ctags-cmd-buf-cwd)
        (progn
          (setq dir (file-relative-name dir citre--ctags-cmd-buf-cwd))
          ;; For cwd itself, if we use "./" in ctags command, a file named
          ;; "file" under cwd will be "./file" in the input field.  But if we
          ;; use ".", it will be "file", which saves some space.
          (when (equal dir "./") (setq dir ".")))
      ;; Expand "~" in the file name as ctags program doesn't expand it.
      (setq dir (file-local-name (expand-file-name dir))))
    (insert dir "\n")))

(defun citre-ctags-cmd-buf-add-lang ()
  "Insert a language in the command editing buffer.
This command requires the ctags program from Universal Ctags."
  (interactive)
  (when-let* ((ctags (or citre-ctags-program "ctags"))
              (langs (with-temp-buffer
                       (ignore-errors
                         (process-file (or citre-ctags-program "ctags")
                                       nil (current-buffer) nil
                                       "--list-languages")
                         (split-string (buffer-string) "\n" t))))
              (lang (completing-read "Select a language: " langs)))
    (insert lang ",")))

(defun citre-ctags-cmd-buf-commit ()
  "Commit in the command editing buffer."
  (interactive)
  (funcall citre--ctags-cmd-buf-callback
           citre--ctags-cmd-buf-tagsfile
           citre--ctags-cmd-buf-cwd
           (citre--ctags-cmd-buf-to-ptag))
  (let ((buf (current-buffer)))
    (pop-to-buffer citre--ctags-cmd-buf-prev-buf
                   '(display-buffer-same-window))
    (kill-buffer buf)))

(defun citre-ctags-cmd-buf-cancel ()
  "Quit the command editing."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-to-buffer citre--ctags-cmd-buf-prev-buf
                   '(display-buffer-same-window))
    (kill-buffer buf)))

;;;; Create tags file: APIs

;;;;; Tags file updating

(defun citre-update-updatable-tags-file (&optional tagsfile sync)
  "Update TAGSFILE that contains recipe for updating itself.
If the recipe can't be found, throw an error.

When SYNC is non-nil, update TAGSFILE synchronously.

Return t if the ctags process starts successfully (when updating
asynchronously), or the updating is finished (when updating
synchronously).  Otherwise return nil."
  (when-let* ((tagsfile
               (or tagsfile (citre-read-tags-file-name)))
              (cmd-ptag (citre--get-pseudo-tag-value "CITRE_CMD" tagsfile))
              (cmd (citre--ctags-cmd-ptag-to-exec cmd-ptag tagsfile))
              (cwd-ptag (citre--get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
              (cwd (if-let ((remote-id (file-remote-p tagsfile)))
                       (concat remote-id cwd-ptag) cwd-ptag))
              (after-process (lambda ()
                               (citre-clear-tags-file-cache)
                               (citre--write-ctags-recipe
                                tagsfile cmd-ptag cwd-ptag))))
    ;; Workaround: If we put this let into the above `if-let*' spec, even
    ;; if it stops before let-binding `default-directory', later there'll
    ;; be some timer errors.
    (let ((default-directory cwd))
      (if sync
          (progn (apply #'process-file (car cmd) nil
                        (get-buffer-create "*citre-ctags*") nil (cdr cmd))
                 (funcall after-process))
        (make-process
         :name "ctags"
         :buffer (get-buffer-create "*citre-ctags*")
         :command cmd
         :connection-type 'pipe
         :stderr nil
         :sentinel
         (lambda (proc _msg)
           (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 (funcall after-process)
                   (message "Finished updating %s" tagsfile))
                (s (user-error "Ctags exits %s.  See *citre-ctags* buffer"
                               s))))
             (s (user-error "Abnormal status of ctags: %s.  \
See *citre-ctags* buffer" s))))
         :file-handler t)
        (message "Updating %s..." tagsfile))
      t)))

;;;;; Get & manipulate update recipe

(defun citre-get-tags-file-recipe (&optional tagsfile target)
  "Return ctags command and its cwd from TAGSFILE.
When TAGSFILE is nil, find it automatically.  TARGET is the tags
file to be written.  If it's nil, then the \"%TAGSFILE\" in the
updating recipe is not translated.

Notice to use the recipe to create/update a remote tags file, you
should use a remote TAGSFILE and local part of TARGET.

Command and cwd is returned by a cons pair.  The command is a
list whose car is the program, and cdr is a list of the args. If
the tagsfile doesn't contain a recipe, nil is returned."
  (when-let* ((tagsfile (or tagsfile (citre-tags-file-path)))
              (cmd-ptag (citre--get-pseudo-tag-value "CITRE_CMD" tagsfile))
              (cmd (citre--ctags-cmd-ptag-to-exec cmd-ptag target))
              (cwd-ptag (citre--get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
              (cwd (if-let ((remote-id (file-remote-p tagsfile)))
                       (concat remote-id cwd-ptag) cwd-ptag)))
    (cons cmd cwd)))

(defun citre-ctags-command-replace-scan-files (cmd scan-files)
  "Replace the files to scan in ctags command CMD by scan-files.
CMD is a list as returned by `citre-get-ctags-command-and-cwd',
and \"%TAGSFILE%\" in it shouldn't be translated.  SCAN-FILES is
a list.

Even CMD is to be run on a remote machine, SCAN-FILES should be
the local parts of themselves."
  (let (idx-before-target)
    ;; Let's start from the end of cmd
    (setq cmd (nreverse cmd))
    (cl-dotimes (n (length cmd))
      ;; when the argument is an option
      (when (or (eq (aref (nth n cmd) 0) ?-)
                ;; or contains "%TAGSFILE"
                (string-match
                 ;; (rx (group (or line-start (not (any "\\"))) (* "\\\\"))
                 ;;     "%TAGSFILE%")
                 "\\(\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\)%TAGSFILE%"
                 (nth n cmd)))
        ;; We stop count.  The idea is the files/dirs to scan should appear
        ;; at the end of the command.
        (setq idx-before-target n)
        (cl-return)))
    ;; We make current file the file to scan.
    (setq cmd (nconc scan-files (nthcdr idx-before-target cmd)))
    (nreverse cmd)))

(defun citre-ctags-command-replace-target (cmd target)
  "Replace the tags file in ctags command CMD by TARGET.
CMD is a list as returned by `citre-get-ctags-command-and-cwd',
and \"%TAGSFILE%\" in it shouldn't be translated.

Even CMD is to be run on a remote machine, TARGET should be the
local part of itself."
  (mapcar (lambda (arg) (citre--replace-tagsfile-variable arg target))
          cmd))

(defun citre-get-ctags-recipe-and-replace-parts
    (&optional tagsfile scan-files target)
  "Get ctags command and its cwd from TAGSFILE.
When TAGSFILE is nil, find it automatically.  When SCAN-FILES is
non-nil, replace the files to scan with SCAN-FILES in the
command.  When TARGET is non-nil, replace tags file by TARGET in
the command.

Notice to use the recipe to create/update a remote tags file, you
should use a remote TAGSFILE and local SCAN-FILES and TARGET.

Command and cwd is returned by a cons pair.  The command is a
list whose car is the program, and cdr is a list of the args.  If
the tagsfile doesn't contain a recipe, nil is returned."
  (when-let* ((tagsfile (or tagsfile (citre-tags-file-path)))
              (cmd-and-cwd (citre-get-tags-file-recipe tagsfile))
              (cmd (car cmd-and-cwd))
              (cwd (cdr cmd-and-cwd)))
    (when scan-files
      (setq cmd (citre-ctags-command-replace-scan-files cmd scan-files)))
    (if target
        (setq cmd (citre-ctags-command-replace-target cmd target))
      (setq cmd (citre-ctags-command-replace-target cmd tagsfile)))
    (cons cmd cwd)))

;;;; Command

;;;###autoload
(defun citre-update-tags-file (&optional tagsfile sync)
  "Update TAGSFILE.
When called interactively, ask the user to pick a tags file.

If Citre can't find an updating recipe in the tagsfile, ask the
user to edit one and save it to TAGSFILE.

When SYNC is non-nil, update TAGSFILE synchronously if it
contains a recipe."
  (interactive)
  (setq tagsfile
        (or tagsfile (citre-read-tags-file-name)))
  (or (citre-update-updatable-tags-file tagsfile sync)
      (when (y-or-n-p (format "%s doesn't contain recipe for updating.  \
Edit its recipe? " tagsfile))
        (citre-edit-tags-file-recipe tagsfile))))

;;;###autoload
(defun citre-update-this-tags-file (&optional sync)
  "Update the currently used tags file.
When no such tags file is found, ask the user to create one.

When a tags file is found, but Citre can't find an updating
recipe in the tagsfile, ask the user to edit one and save it to
the tags file.

When SYNC is non-nil, update the tags file synchronously."
  (interactive)
  (if-let ((tagsfile (citre-tags-file-path)))
      (citre-update-tags-file tagsfile sync)
    (when (y-or-n-p "Can't find tags file for this buffer.  Create one? ")
      (citre-create-tags-file))))

;;;###autoload
(defun citre-edit-tags-file-recipe (&optional tagsfile cmd-ptag cwd noconfirm)
  "Edit the recipe of TAGSFILE.
When called interactively, ask the user to select a tags file.

When CMD-PTAG is non-nil, don't use a command-editing buffer, but
write it to CITRE_CMD ptag directly.

When CWD is non-nil, don't ask the user to pick a root dir to run Ctags.

When NOCONFIRM is non-nil, don't ask the user whether to update
the tags file now (update it directly instead)."
  (interactive)
  (let ((tagsfile (or tagsfile
                      (let* ((f (citre-tags-file-path))
                             (dir (when f (file-name-directory f)))
                             (file (when f (file-name-nondirectory f))))
                        (read-file-name "Choose a tags file: "
                                        dir nil t file))))
        (callback (lambda (tagsfile cwd ptag)
                    (unless (and tagsfile
                                 (citre-non-dir-file-exists-p tagsfile))
                      (unless (file-exists-p (file-name-directory tagsfile))
                        (mkdir (file-name-directory tagsfile)))
                      (write-region "" nil tagsfile))
                    (citre--write-ctags-recipe tagsfile ptag cwd)
                    (when (or noconfirm
                              (y-or-n-p (format "Update %s now? " tagsfile)))
                      ;; WORKAROUND: When `noconfirm' is non-nil, what we do
                      ;; here is `citre-readtags-write-pseudo-tag' to a
                      ;; tagsfile, then `citre-update-tags-file' it.  It seems
                      ;; there's some race conditions happening.  If you eval a
                      ;; `progn' form to do these two things, the readtags
                      ;; process may freeze.  Strangely this only happens to
                      ;; certain tags file paths (even if they are actually the
                      ;; same), and seems to have something to do with its path
                      ;; depth.  Here we just return and schedule the update
                      ;; 0.15 secs later, so the user won't feel it.
                      (run-at-time 0.15 nil
                                   #'citre-update-tags-file tagsfile)))))
    (if cmd-ptag
        (funcall
         callback tagsfile
         (or (and cwd (expand-file-name cwd))
             (citre--read-ctags-cwd))
         cmd-ptag)
      (citre--read-ctags-cwd-and-cmd callback tagsfile cwd))))

;;;###autoload
(defun citre-create-tags-file ()
  "Create a new tags file.
An updating recipe is written to it so later it can be updated by
`citre-update-tags-file'."
  (interactive)
  (let* ((project (funcall citre-project-root-function))
         (read-dir (lambda ()
                     (or (and citre-use-project-root-when-creating-tags
                              project)
                         (read-directory-name
                          "I want to use the tags file when in this dir: "
                          project))))
         scheme
         (read-scheme
          (lambda ()
            (setq scheme
                  (pcase (read-char-choice "Save tags file to ...
[1] the directory where I want to use it.
[2] global cache directory.
[3] project cache directory.
[4] I'll choose a file myself (modify `citre-tags-file-alist' \
is then required to use it).
==> Please type a number (1-4) to choose: "
                                           '(?1 ?2 ?3 ?4))
                    (?1 'in-dir)
                    (?2 'global-cache)
                    (?3 'project-cache)
                    (?4 'manual)))))
         (warning
          (lambda (msg)
            (read-char (concat msg "Press any key to pick another scheme"))))
         tagsfile)
    (setq scheme citre-default-create-tags-file-location)
    (while (null tagsfile)
      (unless scheme (funcall read-scheme))
      (pcase scheme
        ('in-dir
         (if (null citre-tags-file-names)
             (funcall warning "`citre-tags-file-names' \
should be non-nil to use this scheme. ")
           (let ((dir (funcall read-dir))
                 (tags-nondir (completing-read
                               "Tags file name: " citre-tags-file-names nil t
                               nil nil (car citre-tags-file-names))))
             (setq tagsfile (expand-file-name tags-nondir dir)))))
        ('global-cache
         (if (null citre-tags-file-global-cache-dir)
             (funcall warning "`citre-tags-file-global-cache-dir' \
should be non-nil to use this scheme. ")
           (let ((dir (funcall read-dir)))
             (setq tagsfile (citre-tags-file-in-global-cache dir)))))
        ('project-cache
         (cond
          ((null citre-tags-file-per-project-cache-dir)
           (funcall warning "`citre-tags-file-per-project-cache-dir' \
should be non-nil to use this scheme. "))
          ((null (funcall citre-project-root-function))
           (funcall warning "Can't detect project root. "))
          (t (let ((dir (funcall read-dir)))
               (setq tagsfile (citre-tags-file-in-per-project-cache dir))))))
        ('manual (setq tagsfile (read-file-name "Tags file: " project))))
      (setq scheme nil))
    (when (or (not (file-exists-p tagsfile))
              (and (citre-non-dir-file-exists-p tagsfile)
                   (y-or-n-p (format "%s already exists.  Overwrite it? "
                                     tagsfile)))
              (and (citre-dir-exists-p tagsfile)
                   (y-or-n-p (format "%s already exists, and is a directory.  \
Delete it first? "
                                     tagsfile))
                   (progn (delete-directory tagsfile 'recursive)
                          t)))
      (unless (file-exists-p (file-name-directory tagsfile))
        (make-directory (file-name-directory tagsfile) 'parents))
      (citre-edit-tags-file-recipe
       tagsfile
       (when citre-prompt-language-for-ctags-command
         (citre--ctags-cmd-ptag-from-languages))
       (when citre-use-project-root-when-creating-tags
         (funcall citre-project-root-function))
       'noconfirm))))

(provide 'citre-ctags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-ctags.el ends here
