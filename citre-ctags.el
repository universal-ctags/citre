;;; citre-ctags.el --- Generate & update tags files -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.1.1
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

(require 'cl-lib)
(require 'citre-util)

;;;; User options

(defcustom citre-update-tags-file-when-no-definitions t
  "Non-nil means ask me to update the tags file when no definitions are found.
When the tags file in use doesn't contain a recipe, this has no
effect."
  :type 'string
  :group 'citre)

(defcustom citre-ctags-program nil
  "The name or path of the ctags program.
Citre requires ctags program provided by Universal Ctags.  Set
this if ctags is not in your PATH, or its name is not \"ctags\""
  :type 'string
  :group 'citre)

(defcustom citre-edit-cmd-buf-default-cmd
  "ctags
-o
%TAGSFILE%
--languages=[add languages here]
--kinds-all=*
--fields=*
--extras=*
-R
;; add dirs/files to scan here, one line per dir/file
"
  "Default message in the command line editing buffer."
  :type 'string
  :group 'citre)

(defcustom citre-edit-cmd-buf-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") 'citre-edit-cmd-buf-add-lang)
    (define-key map (kbd "C-c f") 'citre-edit-cmd-buf-add-dir-or-file)
    (define-key map (kbd "C-c C-c") 'citre-edit-cmd-buf-commit)
    (define-key map (kbd "C-c C-k") 'citre-edit-cmd-buf-cancel)
    map)
  "Keymap used in the command editing buffer."
  :type 'keymap
  :group 'citre)

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

;;;; Internals

(defun citre--escape-cmd-exec-to-file (cmd)
  "Escape cmd arg CMD.
CMD is from a executable command, and is converted to the form in
CITRE_CMD ptag in a tags file."
  ;; Escape backslashes
  (setq cmd (replace-regexp-in-string "\\\\" "\\\\\\&" cmd))
  ;; Escape tabs, "%" and "|"
  (setq cmd (replace-regexp-in-string "\t" "\\\\t" cmd))
  (setq cmd (replace-regexp-in-string (rx (or "|" "%")) "\\\\\\&" cmd))
  cmd)

(defun citre--escape-cmd-buf-to-file (cmd)
  "Escape cmd arg CMD.
CMD is from the command editing buffer, and is converted to the
form in CITRE_CMD ptag in a tags file."
  ;; Escape tabs and "|".  We don't escape "\" and "%" since in edit command
  ;; buffer we explicitely ask for them to be manaully escaped.
  (setq cmd (replace-regexp-in-string "\t" "\\\\t" cmd))
  (setq cmd (replace-regexp-in-string "|" "\\\\\\&" cmd))
  cmd)

(defun citre--unescape-cmd-file-to-exec (cmd)
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
  ;; Unescape backslashes
  (setq cmd (replace-regexp-in-string
             (rx (group (* "\\\\") "\\\\"))
             "\\1\\\\" cmd))
  cmd)

(defun citre--unescape-cmd-file-to-buf (cmd)
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
   (concat "\\1" (citre--escape-cmd-exec-to-file
                  ;; Seems `make-process' doesn't know "~" in the commannd.
                  (expand-file-name (file-local-name tagsfile))))
   arg 'fixedcase))

(defun citre--cmd-ptag-to-exec (ptag tagsfile)
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
        (setq c (citre--unescape-cmd-file-to-exec c))
        (push c cmd))
      ;; Move over the "!"
      (cl-incf pos))
    (push (substring ptag last) cmd)
    (setq cmd (nreverse cmd))
    (when (equal (nth 0 cmd) "ctags")
      (setf (nth 0 cmd) (or citre-ctags-program "ctags")))
    cmd))

(defun citre--cmd-ptag-to-buf (ptag)
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
      (push (citre--unescape-cmd-file-to-buf (substring ptag last pos)) cmd)
      ;; Move over the "!"
      (cl-incf pos))
    (push (substring ptag last) cmd)
    (string-join (nreverse cmd) "\n")))

(defun citre--cmd-buf-to-ptag ()
  "Generate CITRE_CMD ptag from current command editing buffer."
  (let (cmd)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at (rx (or ";;" (seq (* space) line-end)))) nil)
         (t (push (citre--escape-cmd-buf-to-file
                   (buffer-substring (line-beginning-position)
                                     (line-end-position)))
                  cmd)))
        (forward-line)))
    (string-join (nreverse cmd) "|")))

(defun citre--cmd-ptag-from-languages ()
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

(defun citre--write-recipe (tagsfile cmd-ptag cwd)
  "Write recipe to TAGSFILE.
CMD-PTAG is the value of CITRE_CMD ptag, CWD is the working
directory of Ctags.  It's expanded and convert to a local path."
  (citre-core-write-pseudo-tag
   tagsfile "CITRE_CMD" cmd-ptag
   "command line to generate this tags file")
  (setq cwd (file-local-name (expand-file-name cwd)))
  ;; Ctags on windows generates disk symbol in capital letter.
  (when (<= ?a (aref cwd 0) ?z)
    (setq cwd (citre-upcase-first-letter cwd)))
  (citre-core-write-pseudo-tag
   tagsfile "TAG_PROC_CWD" (file-local-name cwd)
   "dir in which ctags runs"))

;;;;; Edit tags file generation recipe

(defvar citre-edit-cmd-buf-help-msg
  ";; Edit the command line for creating the tags file
;;
;; Syntax:
;;
;; - One command line argument in one line
;; - Lines start with ;; are ignored
;; - Use %TAGSFILE% to refer to the tags file
;; - \"%\" (other than those in %TAGSFILE%) and \"\\\" need escaping
;;
;; Commands:
;;
;; - \\[citre-edit-cmd-buf-add-lang]: Insert a language (needs Universal Ctags)
;; - \\[citre-edit-cmd-buf-add-dir-or-file]: Insert a dir or file
;; - \\[citre-edit-cmd-buf-commit]: Commit
;; - \\[citre-edit-cmd-buf-cancel]: Cancel

"
  "Help message in the command line editing buffer.")

(defvar-local citre--edit-cmd-buf-cwd nil
  "The cwd of ctags program, recorded in the edit cmd buffer.")

(defvar-local citre--edit-cmd-buf-tagsfile nil
  "The tagsfile, recorded in the edit cmd buffer.")

(defvar-local citre--edit-cmd-buf-callback nil
  "The callback function, recorded in the edit cmd buffer.")

(defvar-local citre--edit-cmd-buf-prev-buf nil
  "Previous buffer before switching to edit cmd buffer.")

(defun citre--read-cwd (&optional tagsfile)
  "Prompt the user to choose cwd for Ctags command.
When TAGSFILE is non-nil and TAG_PROC_CWD ptag is found in it,
use it as the default directory.

The full path is returned."
  (let (cwd)
    (when (and tagsfile
               (citre-non-dir-file-exists-p tagsfile)
               (setq cwd (citre-get-pseudo-tag-value "TAG_PROC_CWD" tagsfile)))
      (when-let (remote-id (file-remote-p tagsfile))
        (setq cwd (concat remote-id cwd))))
    (unless cwd (setq cwd (funcall citre-project-root-function)))
    (expand-file-name
     (read-directory-name "Root dir to run ctags: " cwd))))

(defun citre--read-cwd-and-cmd (callback &optional tagsfile cwd)
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
      (setq cwd (citre--read-cwd)))
    (when (and tagsfile
               (citre-non-dir-file-exists-p tagsfile)
               (setq cmd (citre-get-pseudo-tag-value "CITRE_CMD" tagsfile)))
      (setq cmd (citre--cmd-ptag-to-buf cmd)))
    (let ((buf (current-buffer)))
      (pop-to-buffer (generate-new-buffer "*ctags-command-line*")
                     '(display-buffer-same-window))
      (text-mode)
      (setq citre--edit-cmd-buf-prev-buf buf))
    (let ((map (copy-keymap citre-edit-cmd-buf-map)))
      (set-keymap-parent map (current-local-map))
      (use-local-map map))
    (setq citre--edit-cmd-buf-tagsfile tagsfile)
    (setq citre--edit-cmd-buf-cwd cwd)
    (setq citre--edit-cmd-buf-callback callback)
    (insert (substitute-command-keys citre-edit-cmd-buf-help-msg))
    (if cmd (insert cmd) (insert citre-edit-cmd-buf-default-cmd))))

(defun citre-edit-cmd-buf-add-dir-or-file ()
  "Insert a directory or file in the command editing buffer.
When it's in the cwd, it's converted to relative path."
  (interactive)
  (let ((dir (read-file-name "Dir: " citre--edit-cmd-buf-cwd)))
    (if (file-in-directory-p dir citre--edit-cmd-buf-cwd)
        (progn
          (setq dir (file-relative-name dir citre--edit-cmd-buf-cwd)))
      (setq dir (file-local-name dir)))
    ;; For cwd itself, if we use "./" in ctags command, a file named "file"
    ;; under cwd will be "./file" in the input field.  But if we use ".", it
    ;; will be "file", which saves some space.
    (when (equal dir "./") (setq dir "."))
    (insert dir "\n")))

(defun citre-edit-cmd-buf-add-lang ()
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

(defun citre-edit-cmd-buf-commit ()
  "Commit in the command editing buffer."
  (interactive)
  (funcall citre--edit-cmd-buf-callback
           citre--edit-cmd-buf-tagsfile
           citre--edit-cmd-buf-cwd
           (citre--cmd-buf-to-ptag))
  (let ((buf (current-buffer)))
    (pop-to-buffer citre--edit-cmd-buf-prev-buf
                   '(display-buffer-same-window))
    (kill-buffer buf)))

(defun citre-edit-cmd-buf-cancel ()
  "Quit the command editing."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-to-buffer citre--edit-cmd-buf-prev-buf
                   '(display-buffer-same-window))
    (kill-buffer buf)))

;;;; APIs

;;;;; Tags file updating

(defun citre-tags-file-updatable-p (&optional tagsfile)
  "Return t if TAGSFILE contains recipe for updating itself.
If TAGSFILE is nil, use the tags file for current buffer."
  (and (citre-get-pseudo-tag-value "CITRE_CMD" tagsfile)
       (citre-get-pseudo-tag-value "TAG_PROC_CWD" tagsfile)
       t))

(defun citre-update-updatable-tags-file (&optional tagsfile sync)
  "Update TAGSFILE that contains recipe for updating itself.
If the recipe can't be found, throw an error.

When SYNC is non-nil, update TAGSFILE synchronously.

Return t if the ctags process starts successfully (when updating
asynchronously), or the updating is finished (when updating
synchronously).  Otherwise return nil."
  (when-let* ((tagsfile (or tagsfile (read-file-name "Tags file: "
                                                     (citre-tags-file-path))))
              (cmd-ptag (citre-get-pseudo-tag-value "CITRE_CMD" tagsfile))
              (cmd (citre--cmd-ptag-to-exec cmd-ptag tagsfile))
              (cwd-ptag (citre-get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
              (cwd (if-let ((remote-id (file-remote-p tagsfile)))
                       (concat remote-id cwd-ptag) cwd-ptag))
              (after-process (lambda ()
                               (citre-clear-tags-file-cache)
                               (citre--write-recipe
                                tagsfile cmd-ptag cwd-ptag))))
    ;; Workaround: If we put this let into the above `if-let*' spec, even
    ;; if it stops before let-binding `default-directory', later there'll
    ;; be some timer errors.
    (let ((default-directory cwd))
      (if sync
          (progn (apply #'process-file (car cmd) nil
                        (get-buffer-create "*ctags*") nil (cdr cmd))
                 (funcall after-process))
        (make-process
         :name "ctags"
         :buffer (get-buffer-create "*ctags*")
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
                (s (user-error "Ctags exits %s.  See *ctags* buffer" s))))
             (s (user-error "Abnormal status of ctags: %s.  \
See *ctags* buffer" s))))
         :file-handler t)
        (message "Updating %s..." tagsfile))
      t)))

(defun citre-get-definitions-maybe-update-tags-file (&optional symbol tagsfile)
  "Get definitions of SYMBOL from TAGSFILE.
When the definitions are not found, and
`citre-update-tags-file-when-no-definitions' is non-nil, update
TAGSFILE if it contains recipe for updating, and try again.  If
still no definitions found, return nil.

See `citre-get-definitions' to know the behavior of \"getting
definitions\"."
  (let ((tagsfile (or tagsfile (citre-tags-file-path))))
    (or (citre-get-definitions symbol tagsfile)
        (when (and citre-update-tags-file-when-no-definitions
                   (citre-tags-file-updatable-p tagsfile)
                   (y-or-n-p "Can't find definition.  \
Update the tags file and search again? "))
          (citre-update-tags-file tagsfile 'sync)
          ;; WORKAROUND: If we don't sit for a while, the readtags process will
          ;; freeze.  See the comment above `citre-core-write-pseudo-tag'.
          (sit-for 0.01)
          (citre-get-definitions symbol tagsfile)))))

;;;;; Get & manipulate update recipe

(defun citre-get-recipe (&optional tagsfile target)
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
              (cmd-ptag (citre-get-pseudo-tag-value "CITRE_CMD" tagsfile))
              (cmd (citre--cmd-ptag-to-exec cmd-ptag target))
              (cwd-ptag (citre-get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
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

(defun citre-get-recipe-and-replace-parts
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
              (cmd-and-cwd (citre-get-recipe tagsfile))
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
  (setq tagsfile (or tagsfile (read-file-name "Tags file: "
                                              (citre-tags-file-path))))
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
                    (citre--write-recipe tagsfile ptag cwd)
                    (when (or noconfirm
                              (y-or-n-p (format "Update %s now? " tagsfile)))
                      ;; WORKAROUND: When `noconfirm' is non-nil, what we do
                      ;; here is `citre-core-write-pseudo-tag' to a tagsfile,
                      ;; then `citre-update-tags-file' it.  It seems there's
                      ;; some race conditions happening.  If you eval a `progn'
                      ;; form to do these two things, the readtags process may
                      ;; freeze.  Strangely this only happens to certain tags
                      ;; file paths (even if they are actually the same), and
                      ;; seems to have something to do with its path depth.
                      ;; Here we just return and schedule the update 0.15 secs
                      ;; later, so the user won't feel it.
                      (run-at-time 0.15 nil
                                   #'citre-update-tags-file tagsfile)))))
    (if cmd-ptag
        (funcall
         callback tagsfile
         (or (and cwd (expand-file-name cwd))
             (citre--read-cwd))
         cmd-ptag)
      (citre--read-cwd-and-cmd callback tagsfile cwd))))

;;;###autoload
(defun citre-create-tags-file ()
  "Create a new tags file.
An updating recipe is written to it so later it can be updated by
`citre-update-tags-file'."
  (interactive)
  (let* ((project (funcall citre-project-root-function))
         (read-dir (lambda ()
                     (or (and citre-use-project-root-when-creating-tags
                              (funcall citre-project-root-function))
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
         (if (null citre-tags-files)
             (funcall warning "`citre-tags-files' \
should be non-nil to use this scheme. ")
           (let ((dir (funcall read-dir))
                 (tags-nondir (completing-read
                               "Tags file name: " citre-tags-files nil t
                               nil nil (car citre-tags-files))))
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
         (citre--cmd-ptag-from-languages))
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
