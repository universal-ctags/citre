;;; citre.el --- Ctags IDE on the True Editor -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
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

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'thingatpt)

;;;; User options

(defgroup citre nil
  "Code navigation, completion and help message based on ctags."
  :group 'convenience
  :group 'tools
  :prefix "citre-"
  :link '(url-link "https://github.com/AmaiKinono/citre"))

(defcustom citre-project-denoter-files
  '(".projectile" ".dumbjump" "Makefile" "makefile")
  "List of project denoter files.
If automatic detection for project root fails, put a file with one of these
names in your project root.  The list is in descending order of priority."
  :type '(repeat string))

(defcustom citre-project-root nil
  "Absolute root directory of current project.
Set this in your .dir-locals.el if automatic detection fails, and for some
reason you can't put a denoter file in the project root.  If you don't set this
manually, it will automatically be set when enabling `citre-mode'."
  :type '(choice (const nil) string))

(make-variable-buffer-local 'citre-project-root)

(defcustom citre-project-size-threshold 100
  "Size threshold (in MiB) between small and large projects.
In a large project, `citre-excluded-patterns-in-large-project' will also be
used in the default ctags command."
  :type 'integer)

(defcustom citre-find-definition-command
  #'citre-find-definitions-completing-read
  "The command called by `citre-find-definition'.
Customize this to use your own command.  See README.md to find an example of
user-defined command."
  :type 'function)

(defcustom citre-enabled-languages
  ;; Languages with a ";" are not officially supported by universal ctags.
  '("Ada"
    "Asm"
    "Basic"
    "C"
    "C#"
    "C++"
    "CPreProcessor"
    "Clojure"
    "Cobol"
    "CoffeeScript" ;
    "Coq" ;
    "Crystal" ;
    "D"
    "Dart" ;
    "Eiffel"
    "Elixir"
    "Elm"
    "Erlang"
    "F#" ;
    "Falcon"
    "Faust" ;
    "Fortran"
    "Go"
    "Groovy" ;
    "Haskell" ;
    "ITcl"
    "Java"
    "JavaScript"
    "Julia" ;
    "Kotlin" ;
    "Lisp"
    "Lua"
    "Matlab"
    "Moose"
    "Myrddin"
    "Nim"
    "Nix" ;
    "ObjectiveC"
    "OCaml"
    "Pascal"
    "Perl"
    "Perl6"
    "PHP"
    "QtMoc"
    "R"
    "REXX"
    "Ruby"
    "RSpec"
    "Rust"
    "Scala" ;
    "Scheme"
    "SLang"
    "SML"
    "Swift" ;
    "Tcl"
    "TclOO"
    "TypeScript"
    "Vala" ;
    "Verilog"
    "VHDL"
    "YACC"
    "Zephir")
  "Languages used for default ctags command.
Notice that in the default value, some languages are not officially supported
by ctags, but you can extend ctags with your own regex to support them."
  :type '(repeat string))

(defcustom citre-excluded-patterns
  '(".*"
    "SCSS"
    "RCS"
    "CVS"
    "MCVS"
    "_darcs"
    "_MTN"
    "test"
    "tests")
  "Excluded patterns to use in default ctags command."
  :type '(repeat string))

(defcustom citre-excluded-patterns-in-large-project
  '("node_modules")
  "Excluded patterns to use in default ctags command for large projects."
  :type '(repeat string))

;;;; Misc

;; `define-minor-mode' actually defines this for us.  But since it's used in
;; the code before we define the minor mode, we need to define the variable
;; here to suppress compiler warning.
(defvar citre-mode nil
  "Non-nil if Citre mode is enabled.
Use the command `citre-mode' to change this variable.")

;;;; Utilities: project

(defvar citre--project-info-alist nil
  "Alist to record project informations.
The key is the absolute path of the project, the value is a plist consists of
size, tags generation recipe and tags use recipe.")

(defun citre--get-project-info (key &optional buffer)
  "Get info of project in current buffer, or BUFFER if it's non-nil.
KEY specifies the info type."
  (plist-get
   (alist-get (citre--project-root buffer)
              citre--project-info-alist nil nil #'equal)
   key))

(defun citre--set-project-info (key val &optional buffer)
  "Set info of project in current buffer, or BUFFER if it's non-nil.
KEY specifies the info type, VAL is the value."
  (plist-put
   (alist-get (citre--project-root buffer)
              citre--project-info-alist nil nil #'equal)
   key val))

(defun citre--project-root (&optional buffer)
  "Find project root of current file.
Return `citre-project-root' directly if it's set.  Otherwise, search up
directory hierarchy for a file in `citre-project-denoter-files'.  If this
fails, use `project-current'.  If this also fails, use the directory of current
file.  After project root is found, set `citre-project-root' in current
buffer.

When BUFFER is non-nil, find project root for the file in BUFFER instead."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (or citre-project-root
          (let ((filename (buffer-file-name))
                (denoter-dir nil)
                (project-current-dir nil))
            (when filename
              (cl-dolist (denoter citre-project-denoter-files)
                (setq denoter-dir (locate-dominating-file filename denoter))
                (when denoter-dir
                  (cl-return (file-name-directory
                              (expand-file-name denoter-dir)))))
              (when (project-current)
                (setq project-current-dir
                      (file-name-directory
                       (expand-file-name (cdr (project-current nil))))))
              (setq citre-project-root
                    (or denoter-dir
                        project-current-dir
                        (file-name-directory filename)))))))))

(defun citre--write-project-size ()
  "Calculate size (in MiB) of DIR and write it to project info.
Patterns in `citre-ignored-patterns' will be ignored.

This uses the \"du\" program. If it doesn't exist, or takes more than 3 second
to run, nothing will be written, and DIR will be treated as a large project by
citre."
  (let* ((default-directory (citre--project-root))
         (args nil)
         (du-process nil))
    (push "--summarize" args)
    (push "--block-size=1048576" args)
    (dolist (pattern citre-excluded-patterns)
      (push (concat "--exclude=.*/" pattern) args))
    (if (executable-find "du")
        (progn
          (setq du-process (apply #'start-file-process "du" nil "du" args))
          (set-process-filter
           du-process
           (lambda (_process output)
             (when (string-match "^\\([0-9]+\\)" output)
               (citre--set-project-info
                :size (string-to-number (match-string 1 output))))))
          (run-with-timer 3 nil
                          (lambda ()
                            (when (eq (process-status du-process) 'run)
                              (interrupt-process du-process)
                              (citre--set-project-info
                               :size 'large)))))
      (message "Program \"du\" not found")
      (citre--set-project-info :size 'large))))

(defun citre--wait-for-project-size (&optional buffer)
  "Wait for current project size to be set.
If BUFFER is non-nil, use the project in BUFFER instead.

This is only needed for functions that depends directly on the project size.
Functions built on the APIs of citre don't need to care about this."
  (while (not (citre--get-project-info :size buffer))
    (sleep-for 0.05)))

(defun citre--default-ctags-command (&optional buffer)
  "Return default ctags command for current project.
If BUFFER is non-nil, use the project in BUFFER instead."
  (let ((buffer (or buffer (current-buffer)))
        (excludes
         (concat "--exclude="
                 (string-join
                  citre-excluded-patterns " --exclude=")))
        (extra-excludes
         (concat "--exclude="
                 (string-join
                  citre-excluded-patterns-in-large-project " --exclude=")))
        (languages
         (concat "--languages="
                 (string-join citre-enabled-languages ",")))
        (extra-args
         (concat
          "--file-scope=no "
          "--fields=KznS "
          "-R"))
        (size (citre--get-project-info :size buffer)))
    (citre--wait-for-project-size buffer)
    (if (or (eq size 'large) (> size citre-project-size-threshold))
        (string-join
         (list "ctags" excludes extra-excludes languages extra-args) " ")
      (string-join (list "ctags" excludes languages extra-args) " "))))

;;;; APIs

(defun citre-get-lines (symbol match &optional num buffer)
  "Get lines in ctags output that match SYMBOL, return a list of the lines.
SYMBOL is a string.  MATCH is a symbol, which can be:

- \\='prefix: Match all lines whose tag begins with SYMBOL, case insensitively
- \\='substring: Match all lines whose tag contains SYMBOL, case insensitively.
- \\='exact: Match all lines whose tag is exactly SYMBOL, case sensitively.

If NUM is non-nil, it specifies the maximum number of lines.  if BUFFER is
non-nil, use the project in BUFFER instead."
  (let ((buffer (or buffer (current-buffer))))
    (if (not (buffer-local-value citre-mode buffer))
        (error "Citre mode not enabled")
      (let* ((regex
              (pcase match
                ('prefix    (concat "^" symbol))
                ('substring (concat "^(?!!_)\\S*" symbol))
                ('exact     (concat "^" symbol "\\t"))))
             (extra-arg
              (concat
               (pcase match
                 ('exact "")
                 (_      "-i "))
               (if num (format "--max-count=%d " num)
                 "")))
             (command
              (concat
               (citre--default-ctags-command buffer)
               " -f- 2>/dev/null | grep "
               extra-arg
               "-P --color=never -e "
               "'" regex "'")))
        (let ((default-directory (citre--project-root buffer)))
          (split-string
           (shell-command-to-string command)
           "\n" t))))))

(defun citre-parse-line (line)
  "Parse a line in ctags output.
LINE is the line to be parsed.  This returns a list consists of the tag, its
kind, signature, relative path of the file and line number."
  (let* ((elts (split-string line "\t" t))
         (extension (nthcdr 3 elts))
         (file nil)
         (kind nil)
         (linum nil)
         (signature nil))
    (setq file (nth 1 elts))
    (dolist (elt extension)
      (when (string-match "^kind:\\(.*\\)" elt)
        (setq kind (match-string 1 elt)))
      (when (string-match "^line:\\(.*\\)" elt)
        (setq linum (string-to-number (match-string 1 elt))))
      (when (string-match "^signature:\\(.*\\)" elt)
        (setq signature (match-string 1 elt))))
    (when (not kind)
      (setq kind (car extension)))
    (list (car elts) kind signature file linum)))

(defun citre-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre-parse-line'.  FIELD is a symbol which can be:

- \\='tag: The tag name, i.e. the symbol name.
- \\='kind: The kind.  This tells if the symbol is a variable or function, etc.
- \\='signature: The signature of a callable symbol.
- \\='file: The relative path of the file containing the symbol.  It does not
   start with a dot.
- \\='path: The absolute path of the file containing the symbol.
- \\='linum: The line number of the symbol in the file.
- \\='line: The line containing the symbol.  Whitespaces at the beginning and
   end are trimmed.

`citre-get-field' and `citre-get-records' are the 2 main APIs that interactive
commands should use, and ideally should only use."
  (cond
   ((eq field 'line)
    (with-temp-buffer
      (insert-file-contents (citre-get-field 'path record))
      (goto-char (point-min))
      (forward-line (1- (citre-get-field 'linum record)))
      (string-trim (buffer-substring (line-beginning-position)
                                     (line-end-position)))))
   (t
    (let* ((n (pcase field
                ('tag 0)
                ('kind 1)
                ('signature 2)
                ('file 3)
                ('path 3)
                ('linum 4)))
           (val (nth n record)))
      (pcase field
        ('path  (expand-file-name val (citre--project-root)))
        (_      val))))))

(defun citre-get-records (symbol match &optional num buffer)
  "Get parsed tags information of project in current buffer.
SYMBOL is the symbol to search.  MATCH is how should the tags match SYMBOL.
See the docstring of `citre-get-lines' for detail.  NUM is the maximum number
of records.  If BUFFER is non-nil, use project in BUFFER instead.

Normally, there's no need to set BUFFER.  But there are situations when
`citre-get-records' are called in a buffer which is not what we want.  For
example, when getting records during a minibuffer session, or some interactive
UI that uses its own buffer.  In these situations, the commands that build on
top of `citre-get-records' are responsible to offer the right BUFFER.

This uses `citre-get-lines' to get ctags output, and `citre-parse-line' to
parse each line in the output.  See their docstrings to get an idea how this
works.  `citre-get-records' and `citre-get-field' are the 2 main APIs that
interactive commands should use, and ideally should only use."
  (cl-map 'list #'citre-parse-line
       (citre-get-lines symbol match num buffer)))

;;;; Tools
;; These are functions to use by interactive commands.

(defun citre--current-buffer ()
  "Return current buffer or the buffer before entering minibuffer.
See the docstring of `citre-get-records' to know when to use it."
  (if (minibufferp)
      (window-buffer (minibuffer-selected-window))
    (current-buffer)))

(defun citre--propertize-destructive (str &rest properties)
  "Propertize STR destructively and return it.
PROPERTIES should form a sequence of PROPERTY VALUE pairs."
  (cl-loop for (prop val) on properties by #'cddr
           do (put-text-property 0 (length str) prop val str))
  str)

(defun citre--open-file-and-jump-to-line (path linum)
  "Open file PATH and jump to line LINUM."
  (switch-to-buffer (find-file-noselect path))
  (goto-char (point-min))
  (forward-line linum)
  (recenter))

;;;; Commands: xref

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(defun citre--make-xref-object (record)
  "Make xref object of RECORD."
  (let ((kind (citre-get-field 'kind record))
        (path (citre-get-field 'path record))
        (linum (citre-get-field 'linum record))
        (line (citre-get-field 'line record)))
    (xref-make
     (concat
      (citre--propertize-destructive
       kind 'face 'warning)
      " " line)
     (xref-make-file-location path linum 0))))

(defun citre--xref-find-definition (symbol)
  "Return the xref object of the definition information of SYMBOL."
  (cl-map 'list #'citre--make-xref-object
       (citre-get-records symbol 'exact)))

(defun citre-xref-backend ()
  "Define the Citre backend for xref."
  'citre)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql citre)))
  "Define method for xref to get symbol at point."
  (thing-at-point 'symbol))

(cl-defmethod xref-backend-definitions ((_backend (eql citre)) symbol)
  "Define method for xref to find definition of SYMBOL."
  (citre--xref-find-definition symbol))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql citre)))
  "Return a function for xref to find all completions of a prefix."
  (lambda (str pred action)
    (let ((collection
           (cl-map 'list (apply-partially #'citre-get-field 'tag)
                   ;; `citre--current-buffer' is used here because this
                   ;; anonymous function may be called in a minibuffer.
                   (citre-get-records str 'prefix (citre--current-buffer)))))
      (complete-with-action action collection str pred))))

;;;; Commands: jump to definition

(defun citre-find-definition-records (&optional symbol)
  "Get records whose tags match SYMBOL exactly."
  (let ((symbol (or symbol (thing-at-point 'symbol))))
    (citre-get-records symbol 'exact)))

(defun citre-find-definitions-completing-read ()
  "Find definition of the symbol at point.
When there are more than 1 possible definitions, it will let you choose one in
the minibuffer.

The kind, line number and path information are stored in \\='citre-kind,
\\='citre-linum and \\='citre-path properties of each candidate, so if you want
to build a more informative UI using some minibuffer completing framework, you
can use them directly."
  (interactive)
  (let ((candidate-generator
         (lambda (record)
           (citre--propertize-destructive
            (concat (citre--propertize-destructive
                     (citre-get-field 'file record)
                     'face 'warning)
                    ": "
                    (citre-get-field 'line record))
            'citre-kind (citre-get-field 'kind record)
            'citre-linum (citre-get-field 'linum record)
            'citre-path (citre-get-field 'path record))))
        (records (citre-find-definition-records))
        (target nil))
    (pcase (length records)
      (0 (message "Can't find definition"))
      (1 (let* ((record (car records))
                (path (citre-get-field 'path record))
                (linum (citre-get-field 'linum record)))
           (switch-to-buffer (find-file-noselect path))
           (goto-char (point-min))
           (forward-line (1- linum))
           (recenter)))
      (_ (setq target
               (completing-read "location: "
                                (cl-map 'list candidate-generator records)
                                nil t))
         (citre--open-file-and-jump-to-line
          (get-text-property 0 'citre-path target)
          (get-text-property 0 'citre-linum target))))))

(defun citre-find-definition ()
  "Find definition of the symbol at point."
  (interactive)
  (call-interactively citre-find-definition-command))

;;;; Commands: misc

(defun citre-show-project-root ()
  "Show project root of current file in buffer.
Use this command to see if citre detects the project root corectly."
  (interactive)
  (message (or (citre--project-root) "Buffer is not visiting a file")))

;;;; citre-mode

;;;###autoload
(define-minor-mode citre-mode
  "Ctags IDE on the True Editor"
  :lighter " citre"
  (cond
   ((not (buffer-file-name))
    (message "Can't enable citre mode: buffer is not visiting a file")
    (setq citre-mode nil))
   (citre-mode
    (setf (alist-get (citre--project-root)
                     citre--project-info-alist nil nil #'equal)
          '(:size nil :tags-recipe nil :tags-use nil))
    (citre--write-project-size)
    (require 'xref)
    (add-hook 'xref-backend-functions #'citre-xref-backend nil t))
   (t
    (setq citre--project-info-alist
          (cl-delete (citre--project-root)
                     citre--project-info-alist
                     :key #'car :test #'equal))
    (remove-hook 'xref-backend-functions #'citre-xref-backend t))))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre.el ends here
