;;; citre-ctags.el --- Generate & update tags files -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.4
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

;; Tool for finding, generating and updating tags files.

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

(defvar-local citre-tags-file nil
  "A buffer-local variable of the absolute path of tags file.
Set this as a directory-local or file-local variable to specify
the tags file used.")

;;;###autoload
(put 'citre-tags-file 'safe-local-variable #'stringp)

(make-obsolete 'citre-tags-file-per-project-cache-dir
               "Put the tags file directly in the tagged dir, or the global \
cache dir instead."
               "0.4")

(make-obsolete 'citre-tags-file-alist
               "Set `citre-tags-file' as a directory variable instead."
               "0.4")

;;;;; Create tags file

(defcustom citre-ctags-program nil
  "The name or path of the ctags program.
Citre requires ctags program provided by Universal Ctags.  Set
this if ctags is not in your PATH, or its name is not \"ctags\""
  :type 'file
  :group 'citre)

(defcustom citre-ctags-default-options
  "-o
%TAGSFILE%
-L
%LISTFILE%
# Programming languages to be scanned, or \"all\" for all supported languages
--languages=%LANGUAGES%
--kinds-all=*
--fields=*
--extras=*
--recurse
"
  "Default content of the option file, the \"recipe\" for creating a tags file.
It needs to contain \"%TAGSFILE%\", \"%LISTFILE%\" and
\"%LANGUAGES%\" as placeholders."
  :type 'string
  :group 'citre)

(make-obsolete 'citre-ctags-cmd-buf-default-cmd
               'citre-ctags-default-options
               "0.4")

(make-obsolete 'citre-edit-cmd-buf-default-cmd
               'citre-ctags-default-options
               "0.3")

(defcustom citre-ctags-cmd-buf-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") 'citre-ctags-cmd-buf-add-langs)
    (define-key map (kbd "C-c f") 'citre-ctags-cmd-buf-add-dir-or-file)
    (define-key map (kbd "C-c C-c") 'citre-ctags-cmd-buf-commit)
    (define-key map (kbd "C-c C-k") 'citre-ctags-cmd-buf-cancel)
    map)
  "Keymap used when editing tags recipe."
  :type 'keymap
  :group 'citre)

(make-obsolete 'citre-edit-cmd-buf-map
               'citre-ctags-cmd-buf-map
               "0.3")

(defcustom citre-default-create-tags-file-location nil
  "Default location to create a tags file.  Can be:
- nil: Ask me to select one every time.
- `in-dir': In the directory to use it.
- `global-cache': In `citre-tags-file-global-cache-dir'."
  :type '(choice (const :tag "Ask me to select a scheme every time" nil)
                 (const :tag "In the directory to use it" in-dir)
                 (const :tag "In global cache dir" global-cache))
  :group 'citre)

(defcustom citre-edit-ctags-options-manually t
  "When non-nil, edit the ctags options manually when creating a tags file.
Otherwise, prompt the user to choose the languages to scan, and
generate a option file using `citre-ctags-default-options'."
  :type 'boolean
  :group 'citre)

(make-obsolete 'citre-use-project-root-when-creating-tags
               "Please pick a directory manually when creating a tags file."
               "0.4")

(make-obsolete 'citre-prompt-language-for-ctags-command
               'citre-edit-ctags-options-manually
               "0.4")

;;;; Helper functions

(defun citre--ctags-language-list ()
  "Return a list of language names supported by Universal Ctags."
  (let ((lines (with-temp-buffer
                 (process-file (or citre-ctags-program "ctags")
                               nil (current-buffer) nil
                               "--list-languages")
                 (split-string (buffer-string) "\n" t)))
        result)
    (dolist (l lines)
      ;; There may be a " [disabled]" following languages that are not enabled
      ;; in the current directory, due to the option file in use.
      (push (car (split-string l)) result))
    (nreverse result)))

(defun citre--ctags-read-languages (prompt)
  "Read languages from Universal Ctags supported languages.
PROMPT is the prompt shown to the user."
  (completing-read-multiple prompt (citre--ctags-language-list)))

(defun citre--encode-path-to-filename (path)
  "Encode local absolute PATH into a file name."
  (let ((segs (split-string
               ;; expand the "~" so Unix paths will start with a slash. This
               ;; also collapses consecutive slashes.
               (expand-file-name path)
               ;; Use different separators for Windows/Unix style paths.
               (if (eq (aref path 0) ?/) (rx "/") (rx (or "/" "\\")))))
        result)
    (dolist (seg segs)
      (setq seg (replace-regexp-in-string (rx (group (or "%" "!")))
                                          "%\\1" seg 'fixedcase))
      ;; Also replace ":" to make it legal in Windows.
      (setq seg (replace-regexp-in-string ":" "%;" seg 'fixedcase 'literal))
      (push seg result))
    (string-join (nreverse result) "!")))

(defun citre--decode-filename-to-path (filename)
  "Decode FILENAME encoded by `citre--encode-path-to-filename'."
  (setq filename (replace-regexp-in-string
                  ;; Find non-escaped "!".
                  (rx (group (or line-start (not (any "%"))) (* "%%")) "!")
                  "\\1/"
                  filename 'fixedcase))
  (setq filename (replace-regexp-in-string
                  ;; Find escaped ";"
                  (rx (group (or line-start (not (any "%")))) (* "%%") "%;")
                  "\\1:"
                  filename 'fixedcase))
  (setq filename (replace-regexp-in-string
                  (rx "%" (group (or "%" "!"))) "\\1" filename
                  'fixedcase))
  filename)

;;;; Find tags file: Internals

(defvar-local citre--tags-file nil
  "Buffer-local cache for tags file path.
See `citre-tags-file-path' to know how this is used.")

;;;;; By `citre-tags-file-global-cache-dir'

(defun citre--tags-global-cache-dir ()
  "Return the global cache dir of tagsfile.
This also works on remote machine."
  (when citre-tags-file-global-cache-dir
    (concat (or (file-remote-p default-directory) "")
            citre-tags-file-global-cache-dir)))

(defun citre--tags-file-in-global-cache (dir)
  "Return the tags file name of DIR in global cache dir.
DIR is absolute.  The full path of the tags file is returned."
  (expand-file-name
   (concat (citre--encode-path-to-filename
            (file-local-name (file-truename dir)))
           ".tags")
   (citre--tags-global-cache-dir)))

(defun citre--find-tags-file-in-global-cache (dir)
  "Find the tags file of DIR in global cache dir.
DIR is absolute.  The full path of the tags file is returned."
  (when citre-tags-file-global-cache-dir
    (let ((tagsfile (citre--tags-file-in-global-cache dir)))
      (when (citre-non-dir-file-exists-p tagsfile)
        tagsfile))))

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
              (tagsfile nil))
         ;; See if `citre-tags-file' is set by the user.
         (setq tagsfile citre-tags-file)
         ;; Go up directory hierarchy and see if the tags file is in global
         ;; cache dir, or the directory itself.
         (while (and (null tagsfile) current-dir)
           (setq tagsfile
                 (or (citre--find-tags-file-in-global-cache current-dir)
                     (citre--find-tags-file-in-dir current-dir)))
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

(defvar citre-ctags-cmd-buf-help-msg
  "# This block is the help message.
#
# The next block is the Universal Ctags option file. Put one
# command line argument per line. Comments start with \"#\"
#
# The last block contains the files to scan, one file/dir per
# line. You can use paths relative to %ROOT%.
#
# Commands:
#
# - \\[citre-ctags-cmd-buf-add-langs]: Insert a language
# - \\[citre-ctags-cmd-buf-add-dir-or-file]: Insert a dir or file
# - \\[citre-ctags-cmd-buf-commit]: Save, close and update the tags file
# - \\[citre-ctags-cmd-buf-cancel]: Cancel
"
  "Help message in the command line editing buffer.")

(defvar citre--ctags-cmd-buf-divider
  "---------------8<---------------
"
  "The divider in the command line editing buffer.")

(defvar citre--ctags-cmd-buf-default-file-list
  ".
"
  "The default files to scan.")

;;;;; Create recipe files

(defun citre--generate-recipe-file-path (tagsfile &optional listfile)
  "Generate a recipe file path for TAGSFILE.
If LISTFILE is nil, it returns an option file path.  Otherwise, a
list file path, which contains the files to scan."
  (let ((cache-dir (citre--tags-global-cache-dir))
        (ext (if listfile "list" "ctags")))
    (if (file-in-directory-p tagsfile cache-dir)
        (concat (file-name-sans-extension tagsfile) "." ext)
      (expand-file-name (concat ".ctags.d/0." ext)
                        (file-name-directory tagsfile)))))

(defun citre--default-tags-option-file-content (tagsfile &optional languages)
  "Create default option file content for TAGSFILE.
LANGUAGES are the languages to scan, `all' means to scan all
supported languages.  It can also be nil but then the returned
content needs to be edited manually to add languages."
  (let ((in-global (file-in-directory-p tagsfile
                                        (citre--tags-global-cache-dir)))
        (tagsfile (expand-file-name tagsfile)))
    (let ((cmd citre-ctags-default-options))
      (setq cmd (replace-regexp-in-string
                 "%TAGSFILE%"
                 (if in-global
                     (file-local-name tagsfile)
                   (file-name-nondirectory tagsfile))
                 cmd 'fixedcase 'literal))
      (setq cmd (replace-regexp-in-string
                 "%LISTFILE%"
                 (if in-global
                     (file-local-name
                      (concat (file-name-sans-extension tagsfile) ".list"))
                   ".ctags.d/0.list")
                 cmd 'fixedcase 'literal))
      (setq cmd (replace-regexp-in-string
                 "%LANGUAGES%"
                 (if (eq languages 'all) "all" (string-join languages ","))
                 cmd 'fixedcase 'literal))
      cmd)))

;;;;; Find recipe files

(defun citre--find-tags-file-recipe (tagsfile &optional listfile)
  "Find the recipe files of TAGSFILE.
If LISTFILE is nil, find the option file.  Otherwise find the
list file."
  (let ((file (citre--generate-recipe-file-path tagsfile listfile)))
    (when (citre-non-dir-file-exists-p file)
      file)))

;;;;; Generate/update tags file

(defun citre--ctags-cwd-of-tags-file (tagsfile)
  "Return the cwd of ctags program to generate TAGSFILE."
  (if (file-in-directory-p tagsfile (citre--tags-global-cache-dir))
      (concat (or (file-remote-p tagsfile) "")
              (citre--decode-filename-to-path (file-name-base tagsfile)))
    (file-name-directory tagsfile)))

(defun citre--ctags-cmd-option-file-to-read (tagsfile)
  "Return an option file to read to update TAGSFILE.
Nil if an option file is not found, or ctags doesn't need to read
an option file."
  (when (file-in-directory-p tagsfile (citre--tags-global-cache-dir))
    (file-local-name (citre--find-tags-file-recipe tagsfile))))

(defun citre--update-updatable-tags-file (tagsfile &optional callback sync)
  "Update TAGSFILE if it has a recipe.
If CALLBACK is non-nil, call it when finish updating.  If SYNC is
non-nil, update synchronously.

If the tags file is updated or the process is started, return t,
otherwise return nil."
  (when (and (citre--find-tags-file-recipe tagsfile)
             (citre--find-tags-file-recipe tagsfile 'listfile))
    (let* ((option-file (citre--ctags-cmd-option-file-to-read tagsfile))
           (cwd (citre--ctags-cwd-of-tags-file tagsfile))
           (cmd (list (or citre-ctags-program "ctags")))
           (default-directory cwd))
      (when option-file (push (format "--options=%s" option-file) cmd))
      (setq cmd (nreverse cmd))
      (if sync
          (progn (apply #'process-file (car cmd)
                        nil (get-buffer-create "*citre-ctags*") nil
                        (cdr cmd))
                 (when callback (funcall callback)))
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
                (0 (when callback (funcall callback))
                   (message "Finished updating %s" tagsfile))
                (s (user-error "Ctags exits %s.  See *citre-ctags* buffer"
                               s))))
             (s (user-error "Abnormal status of ctags: %s.  \
See *citre-ctags* buffer" s))))
         :file-handler t)
        (message "Updating %s..." tagsfile))
      t)))

;;;;; Edit option files

(defvar-local citre--ctags-cmd-buf-tagsfile nil
  "The tags file that'll be generated.")

(defvar-local citre--ctags-cmd-buf-optionfile nil
  "The option file that'll be written.")

(defvar-local citre--ctags-cmd-buf-listfile nil
  "The list file that'll be written.")

(defvar-local citre--ctags-cmd-buf-cwd nil
  "The cwd of ctags program.")

(defvar-local citre--ctags-cmd-buf-create-new-p nil
  "Whether a new tags file will be created.")

(defun citre--ctags-edit-recipe (tagsfile &optional new)
  "Edit the recipe of TAGSFILE.
If NEW is non-nil, it means this will create a new tags file, and
Citre will clean the buffer -> tags file cache."
  (let* ((tagsfile (expand-file-name tagsfile))
         (optionfile (citre--find-tags-file-recipe tagsfile))
         (listfile (citre--find-tags-file-recipe tagsfile 'listfile))
         (cwd (citre--ctags-cwd-of-tags-file tagsfile))
         help-msg)
    (unless optionfile
      (user-error "No option file found for tags file %s.  Please recreate \
the tags file" tagsfile))
    (unless listfile
      (user-error "No list file found for tags file %s.  Please recreate \
the tags file" tagsfile))
    (pop-to-buffer (generate-new-buffer "*ctags-command-line*")
                   '(display-buffer-same-window))
    (conf-mode)
    (setq citre--ctags-cmd-buf-tagsfile tagsfile)
    (setq citre--ctags-cmd-buf-optionfile optionfile)
    (setq citre--ctags-cmd-buf-listfile listfile)
    (setq citre--ctags-cmd-buf-cwd cwd)
    (setq citre--ctags-cmd-buf-create-new-p new)
    (let ((map (copy-keymap citre-ctags-cmd-buf-map)))
      (set-keymap-parent map (current-local-map))
      (use-local-map map))
    (setq help-msg (replace-regexp-in-string
                    "%ROOT%" cwd citre-ctags-cmd-buf-help-msg
                    'fixedcase 'literal))
    (setq help-msg (substitute-command-keys help-msg))
    (save-excursion
      (insert help-msg)
      (insert citre--ctags-cmd-buf-divider)
      (forward-char (nth 1 (insert-file-contents optionfile)))
      (insert citre--ctags-cmd-buf-divider)
      (forward-char (nth 1 (insert-file-contents listfile))))))

(defun citre-ctags-cmd-buf-add-dir-or-file ()
  "Insert a directory or file in the recipe editing buffer.
When it's in the working directory that ctags will run, it's
converted to relative path."
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

(defun citre-ctags-cmd-buf-add-langs ()
  "Insert languages in the option file.
This command requires the ctags program from Universal Ctags."
  (interactive)
  (insert (string-join (citre--ctags-read-languages "Pick languages: ") ",")))

(defun citre-ctags-cmd-buf-commit ()
  "Save and close the recipe, and update the tags file."
  (interactive)
  (let ((blocks (save-excursion
                  (split-string
                   (buffer-string)
                   (regexp-quote citre--ctags-cmd-buf-divider)))))
    (unless (eq (length blocks) 3)
      (user-error "More then 3 blocks found in the buffer"))
    (with-temp-file citre--ctags-cmd-buf-optionfile
      (insert (nth 1 blocks)))
    (with-temp-file citre--ctags-cmd-buf-listfile
      (insert (nth 2 blocks))))
  (citre--update-updatable-tags-file
   citre--ctags-cmd-buf-tagsfile
   (when citre--ctags-cmd-buf-create-new-p #'citre-clear-tags-file-cache))
  (kill-buffer))

(defun citre-ctags-cmd-buf-cancel ()
  "Quit the command editing."
  (interactive)
  (kill-buffer))

;;;; Command

;;;###autoload
(defun citre-update-tags-file (&optional tagsfile sync)
  "Update TAGSFILE.
When called interactively, ask the user to pick a tags file.

If Citre can't find an option file for the tagsfile, an error is
signaled.  When SYNC is non-nil, update TAGSFILE synchronously."
  (interactive)
  (setq tagsfile (or tagsfile (citre-read-tags-file-name)))
  (or (citre--update-updatable-tags-file tagsfile nil sync)
      (user-error "Recipe for %s not found, please regenerate one using \
`citre-create-tags-file'"
                  tagsfile)))

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
(defun citre-edit-tags-file-recipe (&optional tagsfile)
  "Edit the recipe of TAGSFILE.
When called interactively, ask the user to select a tags file.

When CMD-PTAG is non-nil, don't use a command-editing buffer, but
write it to CITRE_CMD ptag directly.

When CWD is non-nil, don't ask the user to pick a root dir to run Ctags.

When NOCONFIRM is non-nil, don't ask the user whether to update
the tags file now (update it directly instead)."
  (interactive)
  (let ((tagsfile (or tagsfile (citre-read-tags-file-name))))
    (citre--ctags-edit-recipe tagsfile)))

;;;###autoload
(defun citre-create-tags-file ()
  "Create a new tags file."
  (interactive)
  (let* ((project (funcall citre-project-root-function))
         (dir (read-directory-name
               "I want to use the tags file when in this dir: " project))
         location
         (read-location
          (lambda ()
            (setq location
                  (pcase (read-char-choice (format "Save tags file to ...
[1] The directory where I want to use it.
[2] Global cache directory %s.
==> Please type a number (1-2) to choose: " citre-tags-file-global-cache-dir)
                                           '(?1 ?2))
                    (?1 'in-dir)
                    (?2 'global-cache)))))
         (warning
          (lambda (msg)
            (read-char
             (concat msg "\n==> Press any key to pick another location."))))
         tagsfile optionfile listfile)
    (setq location citre-default-create-tags-file-location)
    (while (null tagsfile)
      (unless location (funcall read-location))
      (pcase location
        ('in-dir
         (if (null citre-tags-file-names)
             (funcall warning "`citre-tags-file-names' \
should be non-nil to use this location.")
           (setq tagsfile
                 (expand-file-name
                  (completing-read
                   "Tags file name: " citre-tags-file-names nil t
                   nil nil (car citre-tags-file-names))
                  dir))))
        ('global-cache
         (if (null citre-tags-file-global-cache-dir)
             (funcall warning "`citre-tags-file-global-cache-dir' \
should be non-nil to use this location.")
           (setq tagsfile (citre--tags-file-in-global-cache dir)))))
      (setq location nil))
    (when (citre-dir-exists-p tagsfile)
      (user-error "%s already exists, and is a directory" tagsfile))
    (when (or (not (file-exists-p tagsfile))
              (and (citre-non-dir-file-exists-p tagsfile)
                   (y-or-n-p (format "%s already exists.  Overwrite it? "
                                     tagsfile))))
      (setq optionfile (citre--generate-recipe-file-path tagsfile))
      (setq listfile (citre--generate-recipe-file-path tagsfile 'listfile))
      (dolist (f (list optionfile tagsfile))
        (unless (file-exists-p (file-name-directory f))
          (make-directory (file-name-directory f) 'parents)))
      (with-temp-buffer
        (insert (citre--default-tags-option-file-content
                 tagsfile
                 (unless citre-edit-ctags-options-manually
                   (or (citre--ctags-read-languages
                        "Pick languages to scan, or pick none to scan all \
supported languages: ")
                       'all))))
        (write-file optionfile))
      (with-temp-buffer
        (insert citre--ctags-cmd-buf-default-file-list)
        (write-file listfile))
      (if citre-edit-ctags-options-manually
          (citre--ctags-edit-recipe tagsfile 'new)
        (citre--update-updatable-tags-file tagsfile
                                           #'citre-clear-tags-file-cache)))))

;;;###autoload
(defun citre-relocate-tags-files ()
  "Notify tags backend that it needs to relocate tags files.
Call this after creating a tags file manually.  This is not
needed if you use Citre commands to generate a tags file."
  (interactive)
  (citre-clear-tags-file-cache))

(provide 'citre-ctags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-ctags.el ends here
