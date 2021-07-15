;;; citre-basic-tools.el --- Integration of Citre with Emacs built-in tools -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0
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

;; Basic user tools, including:

;; Tools for creating and updating tags files.
;; - `completion-at-point', xref and imenu integration.
;; - `citre-jump', a `completing-read' UI for jumping to definition.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-util)
(require 'ring)
(require 'subr-x)

;;;; User options

;;;;; Options: Enabled tools

(defcustom citre-enable-xref-integration t
  "Enable xref integration."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-xref-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-xref-integration)

(defcustom citre-enable-capf-integration t
  "Enable auto-completion by `completion-at-point'."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-capf-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-capf-integration)

(defcustom citre-enable-imenu-integration t
  "Enable imenu integration."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-imenu-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-imenu-integration)

;;;;; Options: Generate/update tags file

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

;;;;; Options: `citre-jump' related

(defcustom citre-jump-select-definition-function
  #'citre-jump-completing-read
  "The function for the user to select a definition from a list.
It receives 2 arguments:

- A list of one or more strings to show the definitions.  The
  function should let the user choose one in it.  The list is
  guaranteed to have one or more elements.  When there are only
  one element, the function can decide to let the user confirm,
  or return it directly.
- A string of the symbol name that's interested in.  The function
  can show it to the user.

See `citre-jump-completing-read' for an example of
implementation."
  :type 'function
  :group 'citre)

;;;;; Options: capf related

(defcustom citre-capf-substr-completion nil
  "Whether do substring completion.
Non-nil means to match tags *containing* the symbol to be
completed, Otherwise match tags *start with* the symbol to be
completed.

Notice that when listing the candidates, Emacs itself will
further filter the completions we supply, and its behavior is
controlled by `completion-styles'.  If you want substring
completion, you need to set `citre-capf-substr-completion' to
non-nil, *and* add `substring' to `completion-styles' (for Emacs
27, there is also a `flex' style that will work)."
  :type 'boolean
  :group 'citre)

(defcustom citre-capf-optimize-for-popup t
  "Non-nil means optimize for popup completion.
This caches latest completion result, and allows typing while
calculating completions, making it slicker to use.

`company' and `auto-complete' users should leave this as t.  For
other users, set this to nil may be slightly better, since a
completion session can be interrupted when you call
`completion-at-point', and while it's calculating, you press some
key by mistake, but that doesn't happen very often."
  :type 'boolean
  :group 'citre)

;;;; Tool: Generate/update tags file

;;;;; Internals

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

(defun citre--cmd-ptag-to-exec (ptag tagsfile)
  "Convert PTAG into an executable command CMD (a list).
PTAG is the value of the CITRE_CMD ptag in TAGSFILE,"
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
        (setq c (replace-regexp-in-string
                 ;; (rx (group (or line-start (not (any "\\"))) (* "\\\\"))
                 ;;     "%TAGSFILE%")
                 "\\(\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\)%TAGSFILE%"
                 (concat "\\1" (citre--escape-cmd-exec-to-file
                                ;; Seems `make-process' doesn't know "~" in the
                                ;; commannd.
                                (expand-file-name (file-local-name tagsfile))))
                 c 'fixedcase))
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
  "Reads languages, returns a CITRE_CMD ptag.
This requires ctags program provided by Universal Ctags.  The
generated command should work for most projects"
  (let* ((langs (with-temp-buffer
                  (call-process (or citre-ctags-program "ctags")
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
    (setf (aref cwd 0)
          (upcase (aref cwd 0))))
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
    (when (and tagsfile (citre-non-dir-file-exists-p tagsfile))
      (when (setq cwd (nth 1 (car (citre-core-get-pseudo-tags
                                   "TAG_PROC_CWD" tagsfile))))
        (when-let (remote-id (file-remote-p tagsfile))
          (setq cwd (concat remote-id cwd)))))
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
    (when (and tagsfile (citre-non-dir-file-exists-p tagsfile))
      (when (setq cmd (nth 1 (car (citre-core-get-pseudo-tags
                                   "CITRE_CMD" tagsfile))))
        (setq cmd (citre--cmd-ptag-to-buf cmd))))
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
        (setq dir (file-relative-name dir citre--edit-cmd-buf-cwd))
      (setq dir (file-local-name dir)))
    (insert dir "\n")))

(defun citre-edit-cmd-buf-add-lang ()
  "Insert a language in the command editing buffer.
This command requires the ctags program from Universal Ctags."
  (interactive)
  (when-let* ((ctags (or citre-ctags-program "ctags"))
              (langs (with-temp-buffer
                       (ignore-errors
                         (call-process (or citre-ctags-program "ctags")
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

;;;;; Command

(defun citre-update-tags-file (&optional tagsfile)
  "Update TAGSFILE.
When called interactively, ask the user to pick a tags file.

If Citre can't find an updating recipe in the tagsfile, ask the
user to edit one and save it to TAGSFILE."
  (interactive)
  (if-let* ((tagsfile (or tagsfile (read-file-name "Tags file: "
                                                   (citre-tags-file-path))))
            (cmd-ptag (when-let ((ptag (citre-core-get-pseudo-tags
                                        "CITRE_CMD" tagsfile)))
                        (nth 1 (car ptag))))
            (cmd (citre--cmd-ptag-to-exec cmd-ptag tagsfile))
            (cwd-ptag (when-let ((ptag (citre-core-get-pseudo-tags
                                        "TAG_PROC_CWD" tagsfile)))
                        (nth 1 (car ptag))))
            (cwd (if-let ((remote-id (file-remote-p tagsfile)))
                     (concat remote-id cwd-ptag) cwd-ptag)))
      ;; Workaround: If we put this let into the above `if-let*' spec, even
      ;; if it stops before let-binding `default-directory', later there'll
      ;; be some timer errors.
      (let ((default-directory cwd))
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
                (0 (citre-clear-tags-file-cache)
                   (message "Finished updating %s" tagsfile)
                   (citre--write-recipe tagsfile cmd-ptag cwd-ptag))
                (s (user-error "Ctags exits %s.  See *ctags* buffer" s))))
             (s (user-error "Abnormal status of ctags: %s.  \
See *ctags* buffer" s))))
         :file-handler t)
        (message "Updating %s..." tagsfile))
    (when (y-or-n-p (format "%s doesn't contain recipe for updating.  \
Edit its recipe? " tagsfile))
      (citre-edit-tags-file-recipe tagsfile))))

(defun citre-update-this-tags-file ()
  "Update the currently used tags file.
When no such tags file is found, ask the user to create one.

When a tags file is found, but Citre can't find an updating
recipe in the tagsfile, ask the user to edit one and save it to
the tags file."
  (interactive)
  (if-let ((tagsfile (citre-tags-file-path)))
      (citre-update-tags-file tagsfile)
    (when (y-or-n-p "Can't find tags file for this buffer.  Create one? ")
      (citre-create-tags-file))))

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
                      (read-file-name "Choose a tags file: "
                                      (citre-tags-file-path)) nil t))
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
                          "In which dir you want to use the tags file? "
                          project))))
         scheme
         (read-scheme
          (lambda ()
            (setq scheme
                  (pcase (read-char-choice "Choose a place to save tags file:
[1] In the directory to use it
[2] In the global cache dir
[3] In the project cache dir
[4] I'll select a file myself
NOTE: If you choose 4, you need to modify `citre-tags-file-alist' \
for it to to be used in a dir
"
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

;;;; Tool: Xref integration

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(defvar citre-xref--filter
  `(not ,(citre-core-filter 'extras "anonymous" 'csv-contain))
  "Filter for finding definitions when the symbol is inputted by user.")

(defvar citre-xref--completion-table-cache
  '(:tage-file nil :time nil :collection nil)
  "Plist for caching identifier completions.
Its props and vals are:

- `:tags-file': Canonical path of tags file.
- `:time': Last modified time of tags file.
- `:collection': The completions.")

;; NOTE: In the worst situation, this will create and kill a temporary buffer
;; when processing every tag.  If we get bug report on the performance, we
;; could use the temp buffer technique in citre-peek, so we only need to do
;; this once for every file.
(defun citre-xref--get-linum (tag)
  "Get the line number of TAG.
If there's no buffer visiting the file containing the tag, this
openes it temporarily, and clean it up on exit.

When the file pointed to by TAG doesn't exist, this returns the
line number in TAG, or 0 if it doesn't record the line number.
This is because we don't want to fail an xref session only
because one file is lost, and users may manually use the line
number if they know the file is renamed/moved to which file."
  (let* ((path (citre-core-get-field 'ext-abspath tag))
         (buf-opened (find-buffer-visiting path))
         buf linum)
    (if (not (citre-non-dir-file-exists-p path))
        (or (citre-core-get-field 'extra-line tag) 0)
      (if buf-opened
          (setq buf buf-opened)
        (setq buf (generate-new-buffer (format " *citre-xref-%s*" path)))
        (with-current-buffer buf
          (insert-file-contents path)))
      (with-current-buffer buf
        (setq linum (citre-core-locate-tag tag 'use-linum)))
      (unless buf-opened
        (kill-buffer buf))
      linum)))

(defun citre-xref--make-object (tag)
  "Make xref object of TAG."
  (let* ((path (citre-core-get-field 'ext-abspath tag))
         (file-existance
          (if (citre-non-dir-file-exists-p path) ""
            citre-definition-missing-file-mark))
         (line (citre-xref--get-linum tag)))
    (xref-make
     (citre-make-tag-str tag nil
                         '(annotation :prefix "(" :suffix ")")
                         '(content))
     (xref-make-file-location (concat file-existance path) line 0))))

(defun citre-xref--get-definition-for-completed-symbol (symbol)
  "Get definition for SYMBOL without text property.
When xref prompts for user input for the symbol, we can't get
information from the environment of the symbol at point, so we
have to bypass the whole filter/sort mechanism of Citre and use
simple tag name matching.  This function is for it."
  (citre-get-tags nil symbol 'exact
                  :filter citre-xref--filter
                  :sorter citre-definition-default-sorter
                  :require '(name ext-abspath pattern)
                  :optional '(ext-kind-full line typeref extras)))

(defun citre-xref--find-definition (symbol)
  "Return the xref object of the definition information of SYMBOL."
  (mapcar #'citre-xref--make-object
          (if (citre-get-property 'xref-get-at-point symbol)
              (citre-get-definitions symbol)
            (citre-xref--get-definition-for-completed-symbol symbol))))

(defun citre-xref-backend ()
  "Define the Citre backend for xref."
  'citre)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql citre)))
  "Define method for xref to get symbol at point."
  (citre-put-property (citre-get-symbol)
                      'xref-get-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql citre)) symbol)
  "Define method for xref to find definition of SYMBOL."
  (citre-xref--find-definition symbol))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql citre)))
  "Return a function for xref to find all completions of a prefix."
  (lambda (str pred action)
    (let* ((tagsfile (citre-tags-file-path))
           (update-time (gethash 'time (citre-core-tags-file-info tagsfile)))
           (collection
            (if (and (equal tagsfile
                            (plist-get citre-xref--completion-table-cache
                                       :tags-file))
                     (equal update-time
                            (plist-get citre-xref--completion-table-cache
                                       :time)))
                (plist-get citre-xref--completion-table-cache :collection)
              (let ((collection
                     (cl-remove-duplicates
                      (mapcar
                       (lambda (tag) (citre-core-get-field 'name tag))
                       (citre-get-tags
                        nil str nil
                        :filter citre-xref--filter
                        :sorter (citre-core-sorter '(length name +) 'name)
                        :require '(name)))
                      :test #'equal)))
                (plist-put citre-xref--completion-table-cache
                           :tags-file tagsfile)
                (plist-put citre-xref--completion-table-cache
                           :time update-time)
                (plist-put citre-xref--completion-table-cache
                           :collection collection)
                collection))))
      (complete-with-action action collection str pred))))

;;;; Tool: `citre-jump'

;;;;; Internals

(defvar citre-jump--marker-ring (make-ring 50)
  "The marker ring used by `citre-jump'.")

(defun citre-jump-completing-read (definitions symbol)
  "Select an element in DEFINITIONS, with SYMBOL as a prompt.
This uses the `completing-read' interface.  See
`citre-jump-select-definition-function' for the use of this function."
  (pcase (length definitions)
    (1 (car definitions))
    (_ (completing-read (format "%s: " symbol) definitions nil t))))

;;;;; Commands

;;;###autoload
(defun citre-jump ()
  "Jump to the definition of the symbol at point.
When there's multiple definitions, it lets you pick one using the
`completing-read' UI, or you could use your own UI by customizing
`citre-select-definition-function'."
  (interactive)
  (let* ((marker (point-marker))
         (symbol (citre-get-symbol))
         (definitions (citre-get-definitions))
         (root (funcall citre-project-root-function))
         (loc-alist
          (mapcar (lambda (def)
                    (cons
                     (citre-make-tag-str
                      def nil
                      '(annotation)
                      `(location :suffix ":" :root ,root)
                      '(content))
                     def))
                  definitions))
         (locations (mapcar #'car loc-alist)))
    (if (null locations)
        (user-error "Can't find definition for %s" symbol)
      (citre-goto-tag (alist-get
                       (funcall citre-jump-select-definition-function
                                locations symbol)
                       loc-alist nil nil #'equal))
      (unless (citre-tags-file-path)
        (setq citre--tags-file
              (with-current-buffer (marker-buffer marker)
                (citre-tags-file-path))))
      (ring-insert citre-jump--marker-ring marker))))

(defun citre-jump-back ()
  "Go back to the position before last `citre-jump'."
  (interactive)
  (let ((ring citre-jump--marker-ring))
    (when (ring-empty-p ring)
      (user-error "No more previous history"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil)
      (run-hooks 'citre-after-jump-hook))))

;;;; Tool: Capf integration

;;;;; Internals

(defvar citre-capf--cache
  `(:file nil :symbol nil :bounds nil :substr nil :collection nil)
  "A plist for completion cache.
Its props are:

- `:file': The file where the completion happens.
- `:symbol': The symbol that's been completed.
- `:bounds': The bound positions of `:symbol'.
- `:substr': Whether substring completion is used.  This is
  needed since in the same position, user may use popup
  completion that does prefix completion, and use their own
  command that let binds `citre-capf-substr-completion' to t and
  call `completion-at-point'.
- `:collection': The completion string collection.")

(defun citre-capf--get-annotation (str)
  "Generate annotation for STR.
STR is a candidate in a capf session.  See the implementation of
`citre-completion-at-point'."
  (let* ((kind (citre-get-property 'kind str))
         (type (citre-get-property 'type str))
         (face 'citre-definition-annotation-face))
    (when (or kind type)
      (concat
       (propertize " (" 'face face)
       (propertize (or kind "") 'face face)
       (if (and kind type) citre-definition-annotation-separator "")
       (propertize (or type "") 'face face)
       (propertize ")" 'face face)))))

(defun citre-capf--make-collection (tags)
  "Make auto-completion string collection from TAGS."
  (let* ((collection
          (mapcar
           (lambda (tag)
             (citre-put-property
              (citre-make-tag-str tag nil '(name))
              'kind
              (citre-core-get-field 'ext-kind-full tag)
              'type
              (citre-core-get-field 'typeref tag 'after-colon)
              'signature
              (citre-core-get-field 'signature tag 'after-colon)))
           tags))
         ;; `equal-including-properties' doesn't work. I don't know why, maybe
         ;; it uses `eq' to compare the properties.
         (str-equal
          (lambda (str1 str2)
            (and (equal str1 str2)
                 (null (cl-position
                        nil
                        (mapcar (lambda (prop)
                                  (equal (citre-get-property prop str1)
                                         (citre-get-property prop str2)))
                                '(kind type signature))))))))
    (cl-remove-duplicates
     collection :test str-equal)))

(defun citre-capf--get-collection (symbol)
  "Get completion collection of SYMBOL for capf."
  (if citre-capf-optimize-for-popup
      (let* ((cache citre-capf--cache)
             (file (buffer-file-name))
             (bounds (citre-get-property 'bounds symbol)))
        (if (and citre-capf-optimize-for-popup
                 (equal (plist-get cache :file) file)
                 (string-prefix-p (plist-get cache :symbol) symbol)
                 ;; We also need to make sure we are in the process of
                 ;; completing the same whole symbol, since same symbol in
                 ;; different positions can produce different results,
                 ;; depending on the language support implementation.
                 (eq (car (plist-get cache :bounds)) (car bounds))
                 ;; Just in case the user set `citre-capf-substr-completion' to
                 ;; something can't compare by `eq', we use `null' to make sure
                 ;; we are comparing t or nil.
                 (eq (null (plist-get cache :substr))
                     (null citre-capf-substr-completion)))
            (plist-get cache :collection)
          ;; Make sure we get a non-nil collection first, then setup the cache,
          ;; since the calculation can be interrupted by user input, and we get
          ;; nil, which aren't the actual completions.
          (when-let ((citre-core-stop-process-on-input t)
                     (completions
                      (citre-get-completions
                       symbol nil citre-capf-substr-completion))
                     (collection
                      (pcase (while-no-input
                               (citre-capf--make-collection completions))
                        ('t nil)
                        (val val))))
            (plist-put cache :file file)
            (plist-put cache :symbol (substring-no-properties symbol))
            (plist-put cache :bounds bounds)
            (plist-put cache :substr citre-capf-substr-completion)
            (plist-put cache :collection collection)
            collection)))
    (citre-capf--make-collection
     (citre-get-completions symbol nil citre-capf-substr-completion))))

;;;;; Entry point

(defun citre-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((symbol (citre-get-symbol))
              (bounds (citre-get-property 'bounds symbol))
              (start (car bounds))
              (end (cdr bounds))
              (collection (citre-capf--get-collection symbol))
              (collection
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     '(metadata
                       (category . citre)
                       (cycle-sort-function . identity)
                       (display-sort-function . identity))
                   (complete-with-action action collection str pred))))
              (get-docsig
               (lambda (cand)
                 (citre-get-property 'signature cand))))
    (list start end collection
          :annotation-function #'citre-capf--get-annotation
          :company-docsig get-docsig
          ;; This makes our completion function a "non-exclusive" one, which
          ;; means to try the next completion function when current completion
          ;; table fails to match the text at point (see the docstring of
          ;; `completion-at-point-functions').  This is the desired behavior
          ;; but actually it breaks our substring completion.  This is a bug of
          ;; Emacs, see the FIXME comment in the code of
          ;; `completion--capf-wrapper'.  I believe I've fixed it, so let's
          ;; leave this line commented rather than delete it, and see if my
          ;; patch will get itself into Emacs
          ;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39600).

          ;; It actually doesn't cause much inconvenience.  Our completion
          ;; function works well, and the only problem is it won't fallback to
          ;; the next one when no tags are matched, which I believe to also
          ;; happen in other completion functions.

          ;; :exclusive 'no
          )))

;;;; Tool: Imenu integration

(defvar-local citre-imenu--create-index-function-orig nil
  "Original value of `imenu-create-index-function' in buffer.")

(defun citre-imenu--classify-tags (tags)
  "Classify TAGS based on the `ext-kind-full' field.
This creates an alist, its key is `kind' field value, and value
is a list of tags of that kind."
  (let ((result nil))
    (dolist (tag tags)
      (cl-symbol-macrolet ((place (alist-get kind result nil nil #'equal)))
        (let ((kind (citre-core-get-field 'ext-kind-full tag)))
          (unless place
            (setf place nil))
          (push tag place))))
    (dotimes (i (length result))
      (setf (cdr (nth i result))
            (nreverse (cdr (nth i result)))))
    (cl-sort result (lambda (str1 str2)
                      (compare-strings str1 nil nil str2 nil nil))
             :key #'car)))

(defun citre-imenu--make-index (tag)
  "Create Imenu index for TAG."
  (cons (citre-make-tag-str tag nil
                            '(name)
                            '(annotation :no-kind t :prefix "(" :suffix ")")
                            '(location :no-path t))
        (citre-core-locate-tag tag)))

(defun citre-imenu-create-index-function ()
  "Create imenu index."
  (let* ((file (buffer-file-name))
         (tags-file (citre-tags-file-path))
         (tags (citre-get-tags
                nil nil nil
                :filter
                `(and ,(citre-core-filter-input file tags-file)
                      (not ,(citre-core-filter
                             'extras
                             '("anonymous" "reference" "inputFile")
                             'csv-contain)
                           ,(citre-core-filter-kind "file")))
                :sorter (citre-core-sorter 'line)
                :require '(name pattern)
                :optional '(ext-kind-full line typeref extras)))
         (tags (citre-imenu--classify-tags tags)))
    (dotimes (i (length tags))
      (setf (cdr (nth i tags))
            (mapcar #'citre-imenu--make-index (cdr (nth i tags)))))
    tags))

;;;; Tool: Citre mode

;;;###autoload
(define-minor-mode citre-mode
  "Enable `completion-at-point', xref and imenu integration."
  :lighter " Citre"
  (cond
   (citre-mode
    ;; Make sure we can find a tags file first.
    (unless (citre-tags-file-path)
      (setq citre-mode nil)
      (user-error "Can't find a tags file"))
    (when citre-enable-xref-integration
      (add-hook 'xref-backend-functions #'citre-xref-backend nil t))
    (when citre-enable-capf-integration
      (add-hook 'completion-at-point-functions
                #'citre-completion-at-point nil t))
    (when citre-enable-imenu-integration
      (setq citre-imenu--create-index-function-orig
            imenu-create-index-function)
      (setq imenu-create-index-function #'citre-imenu-create-index-function)))
   (t
    (remove-hook 'xref-backend-functions #'citre-xref-backend t)
    (remove-hook 'completion-at-point-functions
                 #'citre-completion-at-point t)
    (when citre-enable-imenu-integration
      (setq imenu-create-index-function
            citre-imenu--create-index-function-orig)))))

;;;###autoload
(defun citre-auto-enable-citre-mode ()
  "Enable `citre-mode' when a tags file can be found.
Put this in `find-file-hook' to automatically enable `citre-mode'
when opening a file."
  (when (citre-tags-file-path) (citre-mode)))

(provide 'citre-basic-tools)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-basic-tools.el ends here
