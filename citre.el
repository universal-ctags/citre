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
(require 'ring)
(require 'subr-x)
(require 'thingatpt)

;;;; User options

(defgroup citre nil
  "Code editing & reading solution based on Universal Ctags."
  :group 'convenience
  :group 'tools
  :prefix "citre-"
  :link '(url-link "https://github.com/AmaiKinono/citre"))

;;;;; Project related options

(defcustom citre-project-denoter-files
  '(".citre" ".projectile" ".dumbjump")
  "List of project denoter files.
If the project root detection fails, put a file with one of these
names in your project root.  This list is in descending order of
priority (i.e., if we find one, then the rest will be ignored).

See `citre--project-root' to know how Citre detects the project
root."
  :type '(repeat string))

(defcustom citre-project-fallback-denoter-files
  '("Makefile" "makefile" "tags" ".tags")
  "List of project denoter files used as fallbacks.
These are files that may appear in some parent directory of the
project, thus they are used only when normal detection methods
fail.  This list is in descending order of priority (i.e., if we
find one, then the rest will be ignored).

See `citre--project-root' to know how Citre detects the project
root."
  :type '(repeat string))

(defcustom citre-project-root nil
  "Absolute path of project root directory.
Set this in your .dir-locals.el if the project root detection
fails, and for some reason you can't put a denoter file in the
project root (see `citre-project-denoter-files').

If you don't set this manually, Citre will detect the project
root and set it automatically.  See `citre--project-root' to know
how this is done."
  :type '(choice (const nil) string))

(make-variable-buffer-local 'citre-project-root)

(defcustom citre-project-size-threshold 100
  "Size threshold (in MiB) between small and large projects.
In a large project, `citre-excluded-patterns-in-large-project'
will also be used in the default ctags command."
  :type 'integer)

(defcustom citre-tags-files '(".tags" "tags")
  "List of tags file paths.
Relative paths to the project root or absolute paths can both be
used as elements.  This list is in descending order of
priority (i.e., if we find one, then the rest will be ignored)."
  :type '(repeat string))

(make-variable-buffer-local 'citre-tags-files)

;;;;; Ctags command related options

(defcustom citre-ctags-program nil
  "The path to the ctags program.
Set this if ctags is not in your PATH.  Citre requires ctags
program provided by Universal Ctags."
  :type 'string)

(defcustom citre-readtags-program nil
  "The path to the readtags program.
Set this if readtags is not in your PATH.  Citre requires
readtags program provided by Universal Ctags."
  :type 'string)

;; TODO: this may be better replaced by a `citre-excluded-languages'.  We use
;; it to exclude uninterested languages like markup languages.
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
Notice that in the default value, some languages are not
officially supported by ctags, but you can extend ctags with your
own regex to support them."
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
  "Excluded patterns in default ctags command."
  :type '(repeat string))

(defcustom citre-excluded-patterns-in-large-project
  '("node_modules")
  "Excluded patterns in default ctags command for large projects."
  :type '(repeat string))

;;;;; Code navigation related options

(defcustom citre-select-location-function
  #'citre-select-location-completing-read
  "The function for the user to select a location from a list.
It accepts a list of one or more strings, and returns one of
them.  This is used for `citre-jump'.

The strings are in the format of \"relative-file-path:
line-content\", and the function should show it to the user. Each
string also has `kind' and `linum' properties (see
`citre-get-field'), which can be read by `citre--get-property'.
The function can choose to also show them to the user.

The list is guaranteed to have one or more elements. When there
are only one element, the function can decide to let the user
confirm, or return it directly.

See `citre-select-location-completing-read' for an example of
implementation."
  :type 'function)

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after jumping to a location."
  :type 'hook)

(defcustom citre-peek-file-content-height 12
  "Number of lines displaying file contents in the peek window."
  :type 'integer)

(defcustom citre-peek-locations-height 3
  "Number of locations displayed in the peek window."
  :type 'integer)

(defcustom citre-peek-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'citre-peek-next-line)
    (define-key map (kbd "M-p") 'citre-peek-prev-line)
    (define-key map (kbd "M-N") 'citre-peek-next-location)
    (define-key map (kbd "M-P") 'citre-peek-prev-location)
    (define-key map [remap keyboard-quit] ' citre-peek-abort)
    map)
  "Keymap used for `citre-peek' sessions."
  :type 'keymap)

(defface citre-peek-border-face
  '((((background light))
     :height 15 :background "#ffcbd3")
    (t
     :height 15 :background "#db8e93"))
  "Face used for borders of peek windows.
You can customize the appearance of the borders by setting the
height and background properties of this face.

In the terminal version of Emacs, a dashed pattern is used as the
border, and only the background property of this face is used, as
the color of the dashes.")

(defface citre-peek-current-location-face
  '((((background light))
     :background "#c0c0c0")
    (t
     :background "#666666"))
  "Face used for the current location in the peek window.")

;;;;; Eldoc & citre-peek-function related options

(defcustom citre-find-function-name-limit 1500
  "The limit of chars that Citre goes back to find function name.
This is used in `citre-peek-function' and eldoc integration."
  :type 'integer)

(defcustom citre-find-function-name-pos-function-alist
  '(((lisp-mode emacs-lisp-mode) . citre--find-function-name-pos-lisp)
    (t                           . citre--find-function-name-pos-generic))
  "The function for finding current function name position in different modes.
The key should be a major mode, or a list of major modes.  The
value should be the function to use when current major mode is a
derived mode of its key.  The last element in this list should
have a key of t, then the funcion will be used as a fallback.

This is used in `citre-peek-function' and eldoc integration."
  :type '(alist
          :key-type (choice (const :tag "Fallback" t)
                            symbol (repeat symbol))
          :value-type function))

;;;;; Auto-completion related options

(defcustom citre-do-substring-completion t
  "Whether do substring completion.
Non-nil means to match tags *containing* the symbol to be
completed, Otherwise match tags *start with* the symbol to be
completed.

Notice that when listing the candidates, Emacs itself will
further filter the completions we supply, and its behavior is
controlled by `completion-styles'.  If you want substring
completion, you need to set `citre-do-substring-completion' to
non-nil, *and* add `substring' to `completion-styles' (for Emacs
27, there is also a `flex' style that will work)."
  :type 'boolean)

(defcustom citre-completion-in-region-function
  #'citre-completion-in-region
  "The function used for `completion-in-region-function'.
This is called by `completion-at-point' in buffers where Citre
mode is enabled."
  :type 'function)

(defcustom citre-case-sensitivity 'smart
  "Case sensitivity of auto-completion.  Can be:

- `sensitive': Always do case sensitive completion.
- `insensitive': Always do case insensitive completion.
- `smart': Be sensive when completing a symbol with uppercase
  letters, otherwise be insensitive.

Note for developers: Actually this doesn't affect auto-completion
directly.  This option controls the behavior of
`citre--get-lines' when its argument MATCH is `prefix' or
`substring', and in Citre, these two match styles are only used
by auto completion."
  :type '(choice (const :tag "Sensitive" sensitive)
                 (const :tag "Insensitive" insensitive)
                 (const :tag "Smart" smart)))

;;;; Internals

;;;;; Misc

;; `define-minor-mode' actually defines this for us.  But since it's used in
;; the code before we define the minor mode, we need to define the variable
;; here to suppress the compiler warning.
(defvar citre-mode nil
  "Non-nil if Citre mode is enabled.
Use the command `citre-mode' to change this variable.")

;;;;; Dealing with projects

(defvar citre--project-info-alist nil
  "Alist for storing project info.
The keys are the absolute paths of project roots, the values are
plists containing the info of the projects.")

(defun citre--find-dir-with-denoters (file denoters)
  "Search up directory hierarchy from FILE for a denoter file.
DENOTERS is a list of denoter files, in the order of
precedence (i.e., if we find one, then the rest will be ignored).
The directory containing the denoter file will be returned.  If
such directory doesn't exist, nil will be returned."
  (cl-dolist (denoter denoters)
    (let ((dir (locate-dominating-file file denoter)))
      (when dir
        (cl-return (file-name-directory (expand-file-name dir)))))))

(defun citre--project-root (&optional buffer)
  "Find the project root of current file.
The following methods are tried in turn, and the first succeeded
one determines the project root:

- Return `citre-project-root' directly if it's set.
- Search up directory hierarchy for a file in
  `citre-project-denoter-files'.
- Use `project-current'.  Currently it only deals with projects
  under version control.
- Search up directory hierarchy for a file in
  `citre-project-fallback-denoter-files'.
- Use the directory of current file.

After the project root is found, `citre-project-root' in current
buffer is set.  When BUFFER is non-nil, find project root for the
file in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (or citre-project-root
        (when-let* ((file (buffer-file-name))
                    (dir (file-name-directory file)))
          (setq citre-project-root
                (or (citre--find-dir-with-denoters
                     file citre-project-denoter-files)
                    (when-let ((project (project-current nil dir)))
                      (expand-file-name (cdr project)))
                    (citre--find-dir-with-denoters
                     file citre-project-fallback-denoter-files)
                    dir))))))

(defun citre--get-project-info (key &optional project)
  "Get info of current project.
KEY specifies the info type.  If project root PROJECT is non-nil,
use that project instead."
  (plist-get
   (alist-get (or project (citre--project-root))
              citre--project-info-alist nil nil #'equal)
   key))

(defun citre--set-project-info (key val &optional project)
  "Set info of current project.
KEY specifies the info type, VAL is its value.  If project root
PROJECT is non-nil, use that project instead."
  (plist-put
   (alist-get (or project (citre--project-root))
              citre--project-info-alist nil nil #'equal)
   key val))

(defun citre--write-project-size ()
  "Calculate size (in MiB) of current project and write it to project info.
Patterns in `citre-ignored-patterns' will be ignored.

This uses the \"du\" program. If it doesn't exist, or takes more
than 3 second to run, the project will be treated as a large
project by citre."
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

(defun citre--wait-for-project-size (&optional project)
  "Wait for current project size to be set.
If project root PROJECT is non-nil, use that project instead.

This is only needed for functions that depends directly on the
project size.  Functions built on the APIs of citre don't need to
care about this."
  (unless citre-mode
    (error "`citre--wait-for-project-size' called with Citre mode disabled"))
  (let ((project (or project (citre--project-root))))
    (while (not (citre--get-project-info :size project))
      (sleep-for 0.05))))

;;;;; Ctags command

(defun citre--default-ctags-command (&optional project)
  "Return the default ctags command for current project.
If project root PROJECT is non-nil, use that project instead."
  (let ((project (or project (citre--project-root)))
        (program (if citre-ctags-program
                     (format "'%s'" citre-ctags-program)
                   "ctags"))
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
          "--extras='-{fileScope}' "
          "--fields='K{kind}{line}{signature}' "
          "-R"))
        (size (citre--get-project-info :size project)))
    (citre--wait-for-project-size project)
    (if (or (eq size 'large) (> size citre-project-size-threshold))
        (string-join
         (list program excludes extra-excludes languages extra-args) " ")
      (string-join (list program excludes languages extra-args) " "))))

;;;;; Fetch and parse ctags output.

(defun citre--get-lines (symbol match file &optional project)
  "Get lines in tags file FILE that match SYMBOL.
This function returns a list of the lines.  SYMBOL is a string.
MATCH is a symbol, which can be:

- `prefix': Match all lines whose tags begin with SYMBOL, case
  insensitively
- `substring': Match all lines whose tags contain SYMBOL, case
  insensitively.
- `exact': Match all lines whose tags are exactly SYMBOL, case
  sensitively.

if project root PROJECT is non-nil, use that project instead."
  (let ((project (or project (citre--project-root))))
    (unless (cl-member project citre--project-info-alist
                       :key #'car :test #'equal)
      (user-error "Citre mode not enabled for %s" project))
    (let* ((program (or citre-readtags-program "readtags"))
           ;; Strip the text properties first so we can eval it in a backquote
           ;; form later to get just the symbol itself.
           (symbol (substring-no-properties symbol))
           (case-sensitive (pcase citre-case-sensitivity
                             ('sensitive t)
                             ('insensitive nil)
                             ('smart (if (eq match 'exact)
                                         t
                                       (if (string= (downcase symbol) symbol)
                                           nil t)))))
           (op (pcase match
                 ('prefix 'prefix?)
                 ('substring 'substr?)
                 ('exact 'eq?)))
           (symbol-expr (if case-sensitive
                            symbol
                          ;; Since SYMBOL is a string, when we format this list
                          ;; with "%s" later, we automatically get double
                          ;; quotes around SYMBOL.
                          `(downcase ,symbol)))
           (name-expr (if case-sensitive
                          '$name
                        '(downcase $name)))
           (command (format "'%s' -t '%s' -Q '%S' -nel" program file
                            `(,op ,name-expr ,symbol-expr)))
           (default-directory project))
      (split-string
       (shell-command-to-string command)
       "\n" t))))

(defun citre--parse-line (line &optional project)
  "Parse a line from readtags output.
LINE is the line to be parsed.  This returns a list consists of
the tag, its kind, signature, absolute path of the file and line
number, which can be utilized by `citre-get-field'.

If the file field in the line uses relative path, it's expanded
to absolute path using current project root, or PROJECT if it's
non-nil."
  (let* ((project (or project (citre--project-root)))
         (elts (split-string line "\t" t))
         kind signature path linum
         found-kind found-signature found-linum found-any)
    ;; NOTE: `expand-file-name' will return PATH directly when PATH is an
    ;; absolute path. This is the desired behavior.
    (setq path (expand-file-name (nth 1 elts) project))
    (cl-dolist (elt (nthcdr 3 elts))
      (setq found-any nil)
      (when (string-match "^\\([^:]+\\):\\(.*\\)" elt)
        (when (and (not found-any) (not found-kind)
                   (string= (match-string 1 elt) "kind"))
          (setq kind (match-string 2 elt))
          (setq found-kind t)
          (setq found-any t))
        (when (and (not found-any) (not found-signature)
                   (string= (match-string 1 elt) "signature"))
          (setq signature (match-string 2 elt))
          (setq found-signature t)
          (setq found-any t))
        (when (and (not found-any) (not found-linum)
                   (string= (match-string 1 elt) "line"))
          (setq linum (string-to-number (match-string 2 elt)))
          (setq found-any t)))
      (when (and found-kind found-signature found-linum)
        (cl-return)))
    (unless kind
      (setq kind (nth 3 elts)))
    (list (car elts) kind signature path linum)))

;;;; APIs

;;;;; Main APIs

;; TODO: When format a nil field with "%s", it becomes "nil", which is not
;; suitable for showing to the user.  Currently I don't know what's the best
;; way to deal with this, but thinking from a tags file's perspective, since it
;; never produce a field with only the field name but no value, there's no
;; difference if we use nil or empty string to represent it, so it's good to
;; directly use empty string in the records, or make `citre-get-field' not
;; return nil.
(defun citre-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre--parse-line'.  FIELD is a symbol
which can be:

- `tag': The tag name, i.e. the symbol name.
- `kind': The kind.  This tells if the symbol is a variable or
  function, etc.
- `signature': The signature of a callable symbol.
- `path': The absolute path of the file containing the symbol.
- `linum': The line number of the symbol in the file.
- `line': The line containing the symbol.  Leading and trailing
  whitespaces are trimmed.

`citre-get-field' and `citre-get-records' are the 2 main APIs
that interactive commands should use, and ideally should only
use."
  (cond
   ((eq field 'line)
    (when (file-exists-p (citre-get-field 'path record))
      (with-temp-buffer
        (insert-file-contents (citre-get-field 'path record))
        (goto-char (point-min))
        (forward-line (1- (citre-get-field 'linum record)))
        (string-trim (buffer-substring (line-beginning-position)
                                       (line-end-position))))))
   (t
    (let* ((n (pcase field
                ('tag 0)
                ('kind 1)
                ('signature 2)
                ('path 3)
                ('linum 4))))
      (nth n record)))))

(defun citre-get-records (symbol match &optional project)
  "Get records of tags in current project that match SYMBOL.
MATCH is how should the tags match SYMBOL.  See the docstring of
`citre--get-lines' for details.  If project root PROJECT is
non-nil, use that project instead.

Each element in the returned value is a list containing the tag
and some of its fields, which can be utilized by
`citre-get-field'.

Normally, there's no need to set PROJECT, since current project
will be used.  But there are situations where `citre-get-records'
is called in a buffer which is not in the project.  For example,
when getting records during a minibuffer session, or in some
interactive UI that uses its own buffer.  In these situations,
the project root needs to be provided.  The normal way is to call
`citre--project-root', and let bound its returned value to a
variable at the entry of the command, and before entering the
interactive UI, so you can use it later:

- For a simple UI, you can then pass it to `citre-get-records'.
- For a complex UI that may call `citre-get-records' multiple
  times, you can just set its `citre-project-root' to this
  variable, so Citre will treat it as in the project.

This function uses `citre--get-lines' to get lines from tags
file, and `citre--parse-line' to parse each line.  See their
docstrings to get an idea of how this works.  `citre-get-records'
and `citre-get-field' are the 2 main APIs that interactive
commands should use, and ideally should only use."
  (let ((tags-file (cl-some
                    (lambda (file)
                      (let ((tags-file (expand-file-name file
                                                         (or project
                                                             (citre--project-root)))))
                        (when (file-exists-p tags-file) tags-file)))
                    citre-tags-files)))
    (unless tags-file
      (user-error "Tags file not found"))
    (mapcar (lambda (line) (citre--parse-line line project))
            (citre--get-lines symbol match tags-file project))))

;;;;; Helper functions

(defun citre--propertize (str record &rest fields)
  "Propertize STR by FIELDS in RECORD.
Added text properties are prefixed by \"citre-\".  For example,
the `kind' field will be stored in the `citre-kind' property.

Notice that this is destructive, which is different from
`propertize'.  The propertized STR is returned."
  (let ((len (length str)))
    (dolist (field fields)
      (put-text-property 0 len
                         (intern (concat "citre-" (symbol-name field)))
                         (citre-get-field field record)
                         str))
    str))

(defun citre--get-property (str field)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `citre--propertize' or
`citre--put-property'.

What it actually does is prefix the FIELD by `citre-', and get
that text property."
  (get-text-property 0 (intern (concat "citre-" (symbol-name field))) str))

(defun citre--put-property (str prop val)
  "Set the text property corresponding to PROP in STR.
The value is specified by VAL.  The text property added is
prefixed by \"citre-\".  Propertized STR is returned."
  (put-text-property 0 (length str)
                     (intern (concat "citre-" (symbol-name prop)))
                     val str)
  str)

(defun citre--open-file-and-goto-line (path linum &optional window)
  "Open file PATH and goto the line LINUM.
WINDOW can be:

- nil: Use current window.
- `other-window': Use other window.
- `other-window-noselect': Use other window but don't select it."
  ;; TODO: I actually don't know well about this whole display-buffer,
  ;; pop-to-buffer and switch-to-buffer thing.  Will come back and see if this
  ;; docstring describes the behavior well.
  (let ((buf (current-buffer)))
    (if window
        (pop-to-buffer (find-file-noselect path))
      (switch-to-buffer (find-file-noselect path)))
    (goto-char (point-min))
    (forward-line (1- linum))
    (run-hooks 'citre-after-jump-hook)
    (when (eq window 'other-window-noselect)
      (pop-to-buffer buf))))

(defun citre--add-face (str face)
  "Add FACE to STR, and return it.
This is mainly for displaying STR in an overlay.  For example, if
FACE specifies background color, then STR will have that
background color, with all other face attributes preserved.

`default' face is appended to make sure the display in overlay is
not affected by its surroundings."
  (let ((len (length str)))
    (add-face-text-property 0 len face nil str)
    (add-face-text-property 0 len 'default 'append str)
    str))

(defun citre--relative-path (path &optional project)
  "Return PATH but relative to current project root.
If PATH is not under the project, it's directly returned.  If
project root PROJECT is specified, use that project instead."
  (let* ((project (when project (expand-file-name project)))
         (project (or project (citre--project-root)))
         (path (expand-file-name path)))
    (if (string-prefix-p project path)
        (file-relative-name path project)
      path)))

;;;; Action: jump to definition (based on xref)

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
      (propertize kind 'face 'warning) " " line)
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
  (let ((project (citre--project-root)))
    (lambda (str pred action)
      (let ((collection
             (cl-map 'list (apply-partially #'citre-get-field 'tag)
                     ;; No need to use `substring' match style here when the
                     ;; completion style is `substring' or `flex'.  Since Emacs
                     ;; knows nothing about the internal of a collection
                     ;; function, it will call this closure with an empty STR
                     ;; to get the whole collection anyway.
                     (citre-get-records str 'prefix project))))
        (complete-with-action action collection str pred)))))

;;;; Action: peek definition

;;;;; Helpers

(defun citre--subseq (seq interval)
  "Return the subsequence of SEQ in INTERVAL.
INTERVAL is a cons pair, its car is the starting index, cdr is
the ending index (not included).  Cdr can be smaller than car,
then the result will go from the index car, to the end of SEQ,
then back to the start of SEQ, and end before the index cdr."
  (let ((start (car interval))
        (end (cdr interval)))
    (if (<= start end)
        (cl-subseq seq start end)
      (append
       (cl-subseq seq start)
       (cl-subseq seq 0 end)))))

(defun citre--index-in-interval (num interval wrapnum)
  "Return the index of NUM inside INTERVAL, or nil if it's not inside.
INTERVAL is a cons pair of integers.  The car is included, and
cdr is not included.  Cdr can be smaller than car, which means
the interval goes from car to WRAPNUM (not included), then from 0
to cdr (not included)."
  (let* ((start (car interval))
         (end (cdr interval))
         (len (if (<= start end)
                  (- end start)
                (+ (- wrapnum start) end)))
         (index (- num start)))
    (when (< num wrapnum)
      (when (< index 0)
        (setq index (+ index wrapnum)))
      (when (< index len)
        index))))

(defun citre--file-total-linum (path)
  "Return the total number of lines of file PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (line-number-at-pos (point-max))))

(defun citre--fit-line (str)
  "Fit STR in current window.
When STR is too long, it will be truncated, and \"...\" is added
at the end."
  ;; Depending on the wrapping behavior, in some terminals, a line with exact
  ;; (window-body-width) characters can be wrapped. So we minus it by one.
  (let ((limit (1- (window-body-width))))
    (if (> (length str) limit)
        (concat (substring str 0 (- limit 3))
                "...")
      str)))

;; Ref: https://oremacs.com/2015/04/28/blending-faces/
(defun citre--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexadecimal strings.  ALPHA is a number between 0.0
and 1.0 which is the influence of C1 on the result."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x"
                   (ash r -8)
                   (ash g -8)
                   (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

;;;;; Internals

(defvar-local citre-peek--ov nil
  "Current overlay used for peeking.")

(defvar-local citre-peek--locations nil
  "List of definition locations used when peeking.
Each element is a string to be displayed, with text properties
`citre-path' being the absolute path, and `citre-linum' being the
line number.")

(defvar-local citre-peek--displayed-locations-interval nil
  "The interval of displayed locations in `citre-peek--locations'.
This is a cons pair, its car is the index of the first displayed
location, and cdr is the index of the last one.")

(defvar-local citre-peek--location-index nil
  "The index of current location in `citre-peek--locations'.")

(defvar-local citre-peek--temp-buffer-alist nil
  "Files and their temporary buffers that don't exist before peeking.
Its keys are file paths, values are buffers.  The buffers will be
killed after `citre-peek-abort'.")

(defvar citre-peek--bg nil
  "Background color used for file contents when peeking.")

(defvar citre-peek--bg-alt nil
  "Background color used for unselected locations when peeking.")

;; Actually we can make Emacs believe our temp buffer is visiting FILENAME (by
;; setting `buffer-file-name' and `buffer-file-truename'), but then the buffer
;; is not hidden (Emacs hides buffers whose name begin with a space, but those
;; visiting a file are not hidden), and Emacs ask you to confirm when killing
;; it because its content are modified.  Rather than trying to workaround these
;; issues, it's better to create this function instead.
(defun citre-peek--find-buffer-visiting (filename)
  "Return the buffer visiting file FILENAME.
This is like `find-buffer-visiting', but it also searches
`citre-peek--temp-buffer-alist', so it can handle temporary
buffers created during peeking."
  (or (alist-get filename citre-peek--temp-buffer-alist)
      (find-buffer-visiting filename)))

(defun citre-peek--get-content (path linum n)
  "Get file contents for peeking.
PATH is the path of the file.  LINUM is the starting line.  N is
the number of lines.

If there's no buffer visiting PATH currently, create a new
temporary buffer for it.  It will be killed by `citre-abort'."
  (delay-mode-hooks
    (let* ((buf-opened (citre-peek--find-buffer-visiting path))
           (buf nil))
      (if buf-opened
          (setq buf buf-opened)
        (setq buf (generate-new-buffer (format " *citre-peek-%s*" path)))
        (with-current-buffer buf
          (insert-file-contents path)
          ;; `set-auto-mode' checks `buffer-file-name' to set major mode.
          (let ((buffer-file-name path))
            (delay-mode-hooks
              (set-auto-mode))))
        (push (cons path buf) citre-peek--temp-buffer-alist))
      (with-current-buffer buf
        (let ((beg nil)
              (end nil))
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- linum))
            (setq beg (point))
            (forward-line (1- n))
            (setq end (point-at-eol))
            (font-lock-fontify-region beg end)
            (concat (buffer-substring beg end) "\n")))))))

(defun citre-peek--location-index-forward (n)
  "In a peek window, move current location N steps forward.
N can be negative."
  (let ((start (car citre-peek--displayed-locations-interval))
        (end (cdr citre-peek--displayed-locations-interval))
        (len (length citre-peek--locations)))
    (setq citre-peek--location-index
          (mod (+ n citre-peek--location-index) len))
    (unless (citre--index-in-interval
             citre-peek--location-index
             citre-peek--displayed-locations-interval len)
      (setcar citre-peek--displayed-locations-interval
              (mod (+ n start) len))
      (setcdr citre-peek--displayed-locations-interval
              (mod (+ n end) len)))))

(defun citre-peek--line-forward (n)
  "In a peek window, scroll N lines forward.
N can be negative."
  (let* ((loc (nth citre-peek--location-index
                   citre-peek--locations))
         (target (+ n (citre--get-property loc 'linum)))
         (total-linum (citre--get-property loc 'total-linum))
         (target (cond
                  ((< target 1) 1)
                  ((> target total-linum) total-linum)
                  (t target))))
    (citre--put-property loc 'linum target)))

(defun citre-peek--make-border ()
  "Return the border to be used in peek windows."
  (if (display-graphic-p)
      (propertize "\n" 'face 'citre-peek-border-face)
    (propertize
     (concat (make-string (1- (window-body-width)) ?-) "\n")
     'face (list :inherit 'default
                 :foreground
                 (face-attribute 'citre-peek-border-face
                                 :background)))))

(defun citre-peek--post-command-function ()
  "Deal with the update of contents in peek windows."
  (unless (minibufferp)
    (let ((overlay-pos (min (point-max) (1+ (point-at-eol)))))
      (move-overlay citre-peek--ov overlay-pos overlay-pos))
    (let* ((loc (nth citre-peek--location-index citre-peek--locations))
           (initial-newline (if (= (point-at-eol) (point-max))
                                "\n" ""))
           (border (citre-peek--make-border))
           (file-content (citre-peek--get-content
                          (citre--get-property loc 'path)
                          (citre--get-property loc 'linum)
                          citre-peek-file-content-height))
           (displayed-locs (citre--subseq
                            citre-peek--locations
                            citre-peek--displayed-locations-interval))
           (displayed-loc-nums
            (- (cdr citre-peek--displayed-locations-interval)
               (car citre-peek--displayed-locations-interval)))
           (displayed-index
            (citre--index-in-interval citre-peek--location-index
                                      citre-peek--displayed-locations-interval
                                      displayed-loc-nums)))
      ;; Trim the location strings.
      (setq displayed-locs
            (mapcar #'citre--fit-line displayed-locs))
      ;; Add faces.
      (citre--add-face file-content
                       (list :background citre-peek--bg))
      (dotimes (n (length displayed-locs))
        (let ((line (concat (nth n displayed-locs) "\n")))
          (if (= n displayed-index)
              (setf (nth n displayed-locs)
                    (citre--add-face line 'citre-peek-current-location-face))
            (setf (nth n displayed-locs)
                  (citre--add-face line
                                   (list :background citre-peek--bg-alt))))))
      ;; And peek it!
      (overlay-put citre-peek--ov 'after-string
                   (concat initial-newline border file-content
                           (string-join displayed-locs)
                           border)))))

;;;;; Commands

(defun citre-peek ()
  "Peek the definition of the symbol at point."
  (interactive)
  ;; Quit existing peek sessions.
  (when (overlayp citre-peek--ov)
    (citre-peek-abort))
  ;; Fetch informations to show.
  (setq citre-peek--locations (citre-get-definition-locations))
  (when (null citre-peek--locations)
    (user-error "Can't find definition"))
  (dolist (loc citre-peek--locations)
    (citre--put-property loc 'total-linum
                         (citre--file-total-linum
                          (citre--get-property loc 'path)))
    (citre--put-property loc 'buffer-exist-p
                         (find-buffer-visiting
                          (citre--get-property loc 'path))))
  ;; Setup environment for peeking.
  (setf (alist-get 'citre-mode minor-mode-overriding-map-alist)
        citre-peek-keymap)
  (setq citre-peek--ov (make-overlay (1+ (point-at-eol)) (1+ (point-at-eol))))
  (setq citre-peek--displayed-locations-interval
        (cons 0 (min citre-peek-locations-height
                     (length citre-peek--locations))))
  (setq citre-peek--location-index 0)
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified-p (string= (face-background 'default)
                                    "unspecified-bg"))
         (bg (cond
              ((and bg-unspecified-p (eq bg-mode 'dark)) "#333333")
              ((and bg-unspecified-p (eq bg-mode 'light)) "#dddddd")
              (t (face-background 'default)))))
    (cond
     ((eq bg-mode 'dark)
      (setq citre-peek--bg (citre--color-blend "#ffffff" bg 0.03))
      (setq citre-peek--bg-alt (citre--color-blend "#ffffff" bg 0.1)))
     (t
      (setq citre-peek--bg (citre--color-blend "#000000" bg 0.03))
      (setq citre-peek--bg-alt (citre--color-blend "#000000" bg 0.1)))))
  (add-hook 'post-command-hook #'citre-peek--post-command-function nil 'local))

(defun citre-peek-function ()
  "When in a function call, peek the definition of the function."
  (interactive)
  (let ((func-pos (citre--find-function-name-pos)))
    (when func-pos
      (save-excursion
        (goto-char func-pos)
        (citre-peek)))))

(defun citre-peek-next-line ()
  "Scroll to the next line in a peek window."
  (interactive)
  (citre-peek--line-forward 1))

(defun citre-peek-prev-line ()
  "Scroll to the previous line in a peek window."
  (interactive)
  (citre-peek--line-forward -1))

(defun citre-peek-next-location ()
  "Peek the next location of definition."
  (interactive)
  (unless (citre--get-property (nth citre-peek--location-index
                                    citre-peek--locations)
                               'buffer-exist-p))
  (citre-peek--location-index-forward 1))

(defun citre-peek-prev-location ()
  "Peek the previous location of definition."
  (interactive)
  (unless (citre--get-property (nth citre-peek--location-index
                                    citre-peek--locations)
                               'buffer-exist-p))
  (citre-peek--location-index-forward -1))

(defun citre-peek-abort ()
  "Abort peeking."
  (interactive)
  (delete-overlay citre-peek--ov)
  (mapc (lambda (pair)
          (kill-buffer (cdr pair)))
        citre-peek--temp-buffer-alist)
  (setq citre-peek--temp-buffer-alist nil)
  (setq citre-peek--ov nil)
  (setq citre-peek--locations nil)
  (setq citre-peek--displayed-locations-interval nil)
  (setq citre-peek--location-index nil)
  (setq citre-peek--bg nil)
  (setq citre-peek--bg-alt nil)
  (setq minor-mode-overriding-map-alist
        (cl-delete 'citre-mode minor-mode-overriding-map-alist :key #'car))
  (remove-hook 'post-command-hook #'citre-peek--post-command-function 'local))

;;;; Action: jump to definition

;;;;; Internals

(defvar citre--marker-ring (make-ring 50)
  "The marker ring used by `citre-jump'.")

(defun citre-get-definition-locations (&optional symbol)
  "Get locations of symbol at point, or SYMBOL if it's non-nil.
The result is a list of strings, each string consists of relative
file path and the line content, with text properties containing
the kind, linum and absolute path of the tag."
  (let ((symbol (or symbol (thing-at-point 'symbol)))
        (location-str-generator
         (lambda (record)
           (citre--propertize
            (concat (propertize
                     (citre--relative-path
                      (citre-get-field 'path record))
                     'face 'warning)
                    ": "
                    (citre-get-field 'line record))
            record 'kind 'linum 'path))))
    (unless symbol
      (user-error "No symbol at point"))
    (cl-map 'list location-str-generator
            (citre-get-records symbol 'exact))))

(defun citre-recenter-and-blink ()
  "Recenter point and blink after point."
  (recenter)
  (pulse-momentary-highlight-region (point) (1+ (line-end-position))))

(defun citre-select-location-completing-read (locations)
  "Select an element in LOCATIONS.
This uses the `completing-read' interface.  See
`citre-select-location-function' for the use of this function."
  (pcase (length locations)
    (1 (car locations))
    (_ (completing-read "location: " locations nil t))))

;;;;; Commands

(defun citre-jump ()
  "Jump to the definition of the symbol at point.
During an active `citre-peek' session, this jumps to the
definition that is currently peeked."
  (interactive)
  (let ((marker (point-marker))
        (target nil))
    (if (overlayp citre-peek--ov)
        (progn
          (setq target (nth citre-peek--location-index
                            citre-peek--locations))
          (citre-peek-abort))
      (let ((locations (citre-get-definition-locations)))
        (if (null locations)
            (user-error "Can't find definition")
          (setq target (funcall citre-select-location-function locations)))))
    (citre--open-file-and-goto-line
     (citre--get-property target 'path)
     (citre--get-property target 'linum))
    (ring-insert citre--marker-ring marker)))

(defun citre-jump-back ()
  "Go back to the position before last `citre-jump'."
  (interactive)
  (let ((ring citre--marker-ring))
    (when (ring-empty-p ring)
      (user-error "No more previous history"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil)
      (run-hooks 'citre-after-jump-hook))))

;;;; Action: auto completion

;;;;; Internals

(defvar-local citre-completion-in-region-function-orig nil
  "This stores the original `completion-in-region-function'.")

(defun citre-get-completions (&optional symbol)
  "Get completions of symbol at point, or SYMBOL if it's non-nil.
The result is a list of strings, each string is a tag name, with
its text properties containing the kind and signature fields.

It returns nil when the completion can't be done."
  ;; TODO: When inside a symbol, don't grab the part after point.
  (let ((symbol (or symbol (thing-at-point 'symbol)))
        (match (if citre-do-substring-completion
                   'substring 'prefix))
        (candidate-str-generator
         (lambda (record)
           (citre--propertize
            (citre-get-field 'tag record)
            record 'kind 'signature))))
    (when symbol
      (cl-map 'list candidate-str-generator
              (citre-get-records symbol match)))))

(defun citre-completion-in-region (start end collection &optional predicate)
  "A function replacing the default `completion-in-region-function'.
This completes the text between START and END using COLLECTION.
PREDICATE says when to exit.

When there are multiple candidates, this uses the standard
`completing-read' interface, while the default
`completion--in-region' pops a *Completions* buffer to show them.
When combined with some minibuffer completion framework, this is
more user-friendly then the default one.

Notice when `completing-read-function' is
`completing-read-default' (i.e., not enhanced by a minibuffer
completion framework), this falls back to the default
`completion--in-region'."
  (if (eq completing-read-function #'completing-read-default)
      (completion--in-region start end collection predicate)
    (let* ((str (buffer-substring-no-properties start end))
           (completion-ignore-case (string= str (downcase str)))
           (candidates
            (nconc
             (completion-all-completions str collection predicate (- end start))
             nil))
           (completion nil))
      (pcase (length candidates)
        (0 (message "No completions"))
        (1 (setq completion (car candidates)))
        (_ (setq completion (completing-read (format "(%s): " str)
                                             candidates predicate t))))
      (when completion
        (delete-region start end)
        (insert (substring-no-properties completion))))))

;;;;; Commands

(defun citre-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (start (car bounds))
              (end (cdr bounds))
              (collection (citre-get-completions))
              (get-annotation
               (lambda (candidate)
                 (concat
                  " (" (citre--get-property candidate 'kind) ")")))
              (get-docsig
               (lambda (candidate)
                 (citre--get-property candidate 'signature))))
    (list start end collection
          :annotation-function get-annotation
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

;;;; Action: eldoc

(defun citre--pos-in-code-p (&optional pos)
  "Non-nil if position POS is in code.
This means POS is not in comments or strings.  When POS is not
specified, use current point.

Notice that its behavior at boundaries of comment/strings may
vary, depending on whether font lock mode is enabled."
  (let* ((pos (or pos (point))))
    ;; `syntax-ppss' is not always reliable, so we only use it when font lock
    ;; mode is disabled.
    (if font-lock-mode
        (let ((pos-faces (get-text-property pos 'face)))
          (unless (listp pos-faces)
            (setq pos-faces (list pos-faces)))
          (not
           (cl-intersection '(font-lock-comment-face
                              font-lock-comment-delimiter-face
                              font-lock-doc-face
                              font-lock-string-face)
                            pos-faces)))
      (not (save-excursion
             (or (nth 4 (syntax-ppss pos))
                 (nth 3 (syntax-ppss pos))))))))

(defun citre--search-backward-in-code (str &optional bound noerror count)
  "Search backward from point for STR, and skip comments and strings.
About the optional arguments BOUND, NOERROR and COUNT, see the
docstring of `search-backward'.

This function will return the point at the beginning of the first
matched STR.  When the search fails, it won't signal an error,
but return nil.  This is different from `search-backward'."
  (let ((pos-orig (point))
        (pos nil))
    (save-excursion
      (cl-loop
       while
       (ignore-errors (progn
                        (search-backward str bound noerror count)
                        t))
       do
       (when (citre--pos-in-code-p)
         (setq pos (point))
         (cl-return))))
    (if pos
        (goto-char pos)
      (goto-char pos-orig))))

;; TODO: Current implementation (of this and the next function) assumes the
;; expression is balanced (since we use `up-list' and `forward-list'). We
;; should get rid of this limitation.
(defun citre--find-function-name-pos-generic (&optional pos)
  "When in a function call, return the beginning position of the function name.
When POS is specified, use it as the position inside function
call.

It's assumed that the function call has the form of:

  function_name(arg1, arg2, ...)

and there can be whitespaces between function_name and its
arglist."
  (let* ((pos (or pos (point)))
         (pos-limit (max (point-min)
                         (- pos citre-find-function-name-limit)))
         (left-paren-pos nil)
         (sym-atpt-bound nil)
         (func-beg nil))
    (save-excursion
      (goto-char pos)
      ;; skip over whitespaces first.
      (skip-chars-backward "\s\t")
      ;; If there's a symbol at point, and it has the form of a function call,
      ;; then it is the function name we are looking for.
      (when (setq sym-atpt-bound (bounds-of-thing-at-point 'symbol))
        (progn
          (goto-char (cdr sym-atpt-bound))
          (skip-chars-forward "\s\t")
          (when (eq (char-after) ?\()
            (setq func-beg (car sym-atpt-bound)))))
      ;; If the above detection fails, keep searching for open parenthesis
      ;; backward, and see 1. is there a symbol before it; 2. is POS inside the
      ;; parentheses.  If these are true, then it is the function name we are
      ;; looking for.
      (unless func-beg
        (goto-char pos)
        (while (and (citre--search-backward-in-code "(" pos-limit)
                    (not func-beg))
          (setq left-paren-pos (point))
          (skip-chars-backward "\s\t")
          (setq sym-atpt-bound (bounds-of-thing-at-point 'symbol))
          (when sym-atpt-bound
            (save-excursion
              (goto-char left-paren-pos)
              (forward-list)
              (when (<= left-paren-pos pos (point))
                (setq func-beg (car sym-atpt-bound))))))))
    func-beg))

(defun citre--find-function-name-pos-lisp (&optional pos)
  "When in a function call, return the beginning position of the function name.
When POS is specified, use it as the position inside function
call.

This is for use in Lisp languages."
  (let* ((pos (or pos (point)))
         (pos-limit (max (point-min)
                         (- pos citre-find-function-name-limit)))
         (quoted-flag t)
         (func-beg nil))
    (save-excursion
      (goto-char pos)
      ;; Keep moving backward to the beginning of the form one level up, until
      ;; we've reached the top one.
      (while (and (ignore-errors
                    (progn (up-list -1 'escape-strings 'no-syntax-crossing)
                           t))
                  (> (point) pos-limit))
        ;; `up-list' can also take us to other "beginning of sexps", like the
        ;; beginning of a string.  We need to rule out these situations.
        (when (and
               (eq (char-after) ?\()
               (citre--pos-in-code-p))
          ;; If we found a quoted form (here are some detection of the quote to
          ;; make sure it means "quoted form"), set a flag for it.  The
          ;; function name should be one level upper than the outermost quoted
          ;; form.

          ;; We don't count backquote here since although it makes a valid
          ;; quoted form, it's often used in macros where we do "list
          ;; transformation", and when the macro is called, it functions as
          ;; lisp code.
          (if (and (eq (char-before) ?\')
                   (citre--pos-in-code-p (1- (point)))
                   (not (memq (char-before (1- (point))) '(?\\ ?\?))))
              (setq quoted-flag t)
            ;; If we are at a form that's one level upper than a quoted form,
            ;; record its car as function name.
            (when quoted-flag
              (save-excursion
                (forward-char)
                (skip-chars-forward "\s\t\n")
                (setq func-beg (car (bounds-of-thing-at-point 'symbol)))))
            (setq quoted-flag nil)))))
    ;; Make sure the top form isn't quoted.
    (when (and func-beg (not quoted-flag))
      func-beg)))

(defun citre--find-function-name-pos (&optional pos)
  "When in a function call, return the beginning position of the function name.
When POS is specified, use it as the position inside function
call.

It looks up `citre-find-function-name-pos-function-alist' to find
the appropriate function to call."
  (let ((func nil))
    (cl-dolist (pair citre-find-function-name-pos-function-alist)
      (if (eq (car pair) t)
          (progn
            (setq func (cdr pair))
            (cl-return))
        (when (apply #'derived-mode-p (car pair))
          (setq func (cdr pair))
          (cl-return))))
    (funcall func pos)))

(defun citre--find-function-name ()
  "When in a function call, return the function name."
  (let ((pos (citre--find-function-name-pos)))
    (when pos
      (save-excursion
        (goto-char pos)
        (thing-at-point 'symbol)))))

;; TODO: When signature is nil, use the line in the definition location
;; instead.  Maybe this is appropriate for other interfaces too.
(defun citre-eldoc-function ()
  "When in a function call, return a help string about the function.
The help string consists of the function name and its signature,
and can be used as eldoc message."
  (let* ((func-name (citre--find-function-name))
         (records (citre-get-records func-name 'exact))
         (signature nil))
    (cl-dolist (record records)
      (setq signature (citre-get-field 'signature record))
      (when signature (cl-return)))
    (when func-name
      (concat
       (propertize func-name 'face 'font-lock-function-name-face)
       " "
       (when signature
         (propertize signature 'face 'italic))))))

;;;; Misc commands

(defun citre-show-project-root ()
  "Show the project root of current buffer.
Use this command to see if Citre detects the project root
correctly."
  (interactive)
  (if (citre--project-root)
      (message (citre--project-root))
    (user-error "Buffer is not in a project")))

;;;; Citre mode

;;;###autoload
(define-minor-mode citre-mode
  "Ctags IDE on the True Editor"
  :lighter " citre"
  (cond
   ((not (buffer-file-name))
    (setq citre-mode nil)
    (user-error "Can't enable citre mode: buffer is not visiting a file"))
   (citre-mode
    (unless (cl-member (citre--project-root)
                       citre--project-info-alist
                       :key #'car :test #'equal)
      (setf (alist-get (citre--project-root)
                       citre--project-info-alist nil nil #'equal)
            '(:size nil :tags-recipe nil :tags-use nil)))
    (citre--write-project-size)
    (require 'xref)
    (add-hook 'xref-backend-functions #'citre-xref-backend nil t)
    (add-hook 'completion-at-point-functions
              #'citre-completion-at-point nil t)
    (when (local-variable-p completion-in-region-function)
      (setq citre-completion-in-region-function-orig
            completion-in-region-function))
    (set (make-local-variable 'completion-in-region-function)
         #'citre-completion-in-region)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'citre-eldoc-function))
   (t
    (remove-hook 'xref-backend-functions #'citre-xref-backend t)
    (remove-hook 'completion-at-point-functions #'citre-completion-at-point t)
    (if citre-completion-in-region-function-orig
        (progn
          (setq completion-in-region-function
                citre-completion-in-region-function-orig)
          (setq citre-completion-in-region-function-orig nil))
      (kill-local-variable 'completion-in-region-function))
    (remove-function (local 'eldoc-documentation-function)
                     #'citre-eldoc-function))))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre.el ends here
