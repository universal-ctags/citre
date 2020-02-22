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
  "Code navigation, completion and help message based on ctags."
  :group 'convenience
  :group 'tools
  :prefix "citre-"
  :link '(url-link "https://github.com/AmaiKinono/citre"))

;;;;; Project related options

(defcustom citre-project-denoter-files
  '(".projectile" ".dumbjump" "Makefile" "makefile")
  "List of project denoter files.
If automatic detection for project root fails, put a file with
one of these names in your project root.  The list is in
descending order of priority."
  :type '(repeat string))

(defcustom citre-project-root nil
  "Absolute root directory of current project.
Set this in your .dir-locals.el if automatic detection fails, and
for some reason you can't put a denoter file in the project root.
If you don't set this manually, it will automatically be set when
enabling `citre-mode'."
  :type '(choice (const nil) string))

(make-variable-buffer-local 'citre-project-root)

(defcustom citre-project-size-threshold 100
  "Size threshold (in MiB) between small and large projects.
In a large project, `citre-excluded-patterns-in-large-project'
will also be used in the default ctags command."
  :type 'integer)

;;;;; Ctags command related options

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

(defcustom citre-jump-command
  #'citre-jump-completing-read
  "The command called by `citre-jump'.
Customize this to use your own command.  See README.md to find an
example of user-defined command."
  :type 'function)

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after `citre-jump' and `citre-jump-back'."
  :type :hook)

(defcustom citre-peek-file-content-height 12
  "Number of file content lines displayed in the peeking window."
  :type 'integer)

(defcustom citre-peek-locations-height 3
  "Number of locations displayed in the peeking window."
  :type 'integer)

(defcustom citre-peek-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'citre-peek-next-line)
    (define-key map (kbd "M-p") 'citre-peek-prev-line)
    (define-key map (kbd "M-N") 'citre-peek-next-location)
    (define-key map (kbd "M-P") 'citre-peek-prev-location)
    (define-key map [remap keyboard-quit] ' citre-peek-abort)
    map)
  "Keymap used when peeking."
  :type 'keymap)

(defface citre-peek-border-face
  '((((background light))
     :height 15 :background "#ffcbd3")
    (t
     :height 15 :background "#db8e93"))
  "Face used for borders of the peeking window.
In terminal version of Emacs, the background of this face is used
as the foreground of the dashes.")

(defface citre-peek-current-location-face
  '((((background light))
     :background "#c0c0c0")
    (t
     :background "#666666"))
  "Face used for current location in the peeking window.")

;;;;; Auto completion related options

(defcustom citre-get-completions-by-substring t
  "When searching for completions, whether to match by substring.
Non-nil means to match tags CONTAINING the symbol at point.
Otherwise match tags START WITH the symbol at point.

Notice that when listing the candidates, Emacs itself will
further filtering from the completions we supply, and this
behavior is controled by `completion-styles'.  You need to set
`citre-get-completions-by-substring' to non-nil, AND add
substring to `completion-styles' to do \"fuzzy completion\" (for
Emacs 27, there is also a flex style)."
  :type 'boolean)

(defcustom citre-completion-in-region-function
  #'citre-completion-in-region
  "The function used for `completion-in-region-function'.
See docstring of `citre-completion-in-region' for detail."
  :type :function)

;;;; Internals

;;;;; Misc

;; `define-minor-mode' actually defines this for us.  But since it's used in
;; the code before we define the minor mode, we need to define the variable
;; here to suppress compiler warning.
(defvar citre-mode nil
  "Non-nil if Citre mode is enabled.
Use the command `citre-mode' to change this variable.")

(defun citre--prevent-gc ()
  "Prevent GC before idle.
This sets GC threshold to the largest possible value, and restore
it after idling for 1 second.  Put this function at the entry of
time-consuming tasks.

This is for internal use only.  Functions built on the APIs
should never use this unless it hacks Citre really hard."
  (let ((gc-threshold-orig gc-cons-threshold))
    (setq gc-cons-threshold most-positive-fixnum)
    (run-with-idle-timer
     1 nil (lambda () (setq gc-cons-threshold gc-threshold-orig)))))

;;;;; Dealing with projects

(defvar citre--project-info-alist nil
  "Alist to record project informations.
The key is the absolute path of the project, the value is a plist
consists of size, tags generation recipe and tags use recipe.")

(defun citre--get-project-info (key &optional buffer)
  "Get info of project in current buffer.
When BUFFER is non-nil, use project in BUFFER instead.  KEY
specifies the info type."
  (plist-get
   (alist-get (citre--project-root buffer)
              citre--project-info-alist nil nil #'equal)
   key))

(defun citre--set-project-info (key val &optional buffer)
  "Set info of project in current buffer.
When BUFFER is non-nil, use project in BUFFER instead.  KEY
specifies the info type, VAL is the value."
  (plist-put
   (alist-get (citre--project-root buffer)
              citre--project-info-alist nil nil #'equal)
   key val))

(defun citre--project-root (&optional buffer)
  "Find project root of current file.
Return `citre-project-root' directly if it's set.  Otherwise,
search up directory hierarchy for a file in
`citre-project-denoter-files'.  If this fails, use
`project-current'.  If this also fails, use the directory of
current file.  After project root is found, set
`citre-project-root' in current buffer.

When BUFFER is non-nil, find project root for the file in BUFFER
instead."
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

This uses the \"du\" program. If it doesn't exist, or takes more
than 3 second to run, nothing will be written, and DIR will be
treated as a large project by citre."
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

This is only needed for functions that depends directly on the
project size.  Functions built on the APIs of citre don't need to
care about this."
  (while (not (citre--get-project-info :size buffer))
    (sleep-for 0.05)))

;;;;; Ctags command

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

;;;;; Fetch and parse ctags output

(defun citre-get-lines (symbol match &optional buffer num)
  "Get lines in ctags output that match SYMBOL.
The function returns a list of the lines.  SYMBOL is a string.
MATCH is a symbol, which can be:

- \\='prefix: Match all lines whose tag begins with SYMBOL,
   case insensitively
- \\='substring: Match all lines whose tag contains SYMBOL,
   case insensitively.
- \\='exact: Match all lines whose tag is exactly SYMBOL,
   case sensitively.

if BUFFER is non-nil, use the project in BUFFER instead.  If NUM
is non-nil, it specifies the maximum number of lines."
  (let ((buffer (or buffer (current-buffer))))
    (if (not (buffer-local-value 'citre-mode buffer))
        (user-error "Citre mode not enabled")
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
LINE is the line to be parsed.  This returns a list consists of
the tag, its kind, signature, relative path of the file and line
number."
  (let* ((elts (split-string line "\t" t))
         (kind nil)
         (signature nil)
         (linum nil)
         (found-kind nil)
         (found-signature nil)
         (found-linum nil)
         (found-any nil))
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
    (list (car elts) kind signature (nth 1 elts) linum)))

;;;; APIs

;;;;; Main APIs

(defun citre-get-field (field record)
  "Get FIELD from RECORD.
RECORD is an output from `citre-parse-line'.  FIELD is a symbol
which can be:

- \\='tag: The tag name, i.e. the symbol name.
- \\='kind: The kind.  This tells if the symbol is a variable or
   function, etc.
- \\='signature: The signature of a callable symbol.
- \\='file: The relative path of the file containing the symbol.
  It does not start with a dot.
- \\='path: The absolute path of the file containing the symbol.
- \\='linum: The line number of the symbol in the file.
- \\='line: The line containing the symbol.  Leading and trailing
   whitespaces are trimmed.

`citre-get-field' and `citre-get-records' are the 2 main APIs
that interactive commands should use, and ideally should only
use."
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

(defun citre-get-records (symbol match &optional buffer num)
  "Get parsed tags information of project.
SYMBOL is the symbol to search.  MATCH is how should the tags
match SYMBOL.  See the docstring of `citre-get-lines' for detail.
If BUFFER is non-nil, use project in BUFFER instead.  NUM is the
maximum number of records.

Normally, there's no need to set BUFFER, since the current buffer
will be used.  But there are situations when `citre-get-records'
are called in a buffer which is not what we want.  For example,
when getting records during a minibuffer session, or some
interactive UI that uses its own buffer.  In these situations,
the commands that build on top of `citre-get-records' are
responsible to offer the right BUFFER.  The normal way to do this
is let bound a variable to (current-buffer) at the entry of the
command, before entering the interactive UI, so you can use it
later.

This uses `citre-get-lines' to get ctags output, and
`citre-parse-line' to parse each line in the output.  See their
docstrings to get an idea how this works.  `citre-get-records'
and `citre-get-field' are the 2 main APIs that interactive
commands should use, and ideally should only use."
  (citre--prevent-gc)
  (cl-map 'list #'citre-parse-line
          (citre-get-lines symbol match num buffer)))

;;;;; Helper functions

(defun citre--propertize (str record &rest fields)
  "Propertize STR by FIELDS in RECORD.
Added text properties are prefixed by \"citre-\".  For example,
the \\='kind field becomes the \\='citre-kind property.

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
  "Get text property corresponding to FIELD in STR.
STR should be propertized by `citre--propertize' or
`citre--put-property'."
  (get-text-property 0 (intern (concat "citre-" (symbol-name field))) str))

(defun citre--put-property (str prop val)
  "Set the text property corresponding to PROP in STR.
The value is specified by VAL.  The text property added is
prefixed by \"citre-\".  Propertized STR is returned."
  (put-text-property 0 (length str)
                     (intern (concat "citre-" (symbol-name prop)))
                     val str)
  str)

(defun citre--open-file-and-goto-line (path linum)
  "Open file PATH and go to line LINUM."
  (switch-to-buffer (find-file-noselect path))
  (goto-char (point-min))
  (forward-line (1- linum)))

(defun citre--add-face (str face)
  "Add FACE to STR, and return it.
This is mainly for displaying STR in an overlay.  For example,
FACE only specifies background color, then STR will have that
background color, with all other face attributes preserved.

`default' face is appended to make sure the display in overlay
doesn't affected by its surroundings."
  (let ((len (length str)))
    (add-face-text-property 0 len face nil str)
    (add-face-text-property 0 len 'default 'append str)
    str))

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
  (let ((buffer (current-buffer)))
    (lambda (str pred action)
      (let ((collection
             (cl-map 'list (apply-partially #'citre-get-field 'tag)
                     ;; No need to use 'substring match style here even when
                     ;; using 'substring or 'flex completion styles.  Since
                     ;; Emacs know nothing about the internal of a collection
                     ;; function, it will call this closure with an empty STR
                     ;; to get the whole collection anyway.
                     (citre-get-records str 'prefix buffer))))
        (complete-with-action action collection str pred)))))

;;;; Action: peek definition

;;;;; Helpers

(defun citre--subseq (seq interval)
  "Return the subsequence of SEQ in INTERVAL.
INTERVAL is a cons pair, its car is the starting index, cdr is
the ending index (not included).  Cdr can be smaller than car,
then the result will go from index car, to the end of SEQ, then
back to the start of SEQ, and end before index cdr."
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
the interval goes from car to WRAPNUM (which is not included),
then from 0 to cdr (not included)."
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
  "Return total number of lines of file PATH."
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

(defun citre--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.  ALPHA is a number between 0.0
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
Each element is a string to be displayed, with text property
`citre-path' for the full path, and `citre-linum' for the line
number.")

(defvar-local citre-peek--displayed-locations-interval nil
  "The index of displayed locations in `citre-peek--locations'.
This is a cons pair, its car is the index of first displayed
location, and cdr is of the last one.")

(defvar-local citre-peek--location-index nil
  "The index of current location in `citre-peek--locations'.")

(defvar-local citre-peek--temp-buffer-alist nil
  "A list of file buffers that don't exist before peeking.
These will be killed after `citre-peek-abort'.")

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
  "Return the buffer visiting file FILENAME (a string).
This is like `find-buffer-visiting', but it also searches
`citre-peek--temp-buffer-alist', so it can handle temporary
buffers created when peekin."
  (or (alist-get filename citre-peek--temp-buffer-alist)
      (find-buffer-visiting filename)))

(defun citre-peek--get-content (path linum n)
  "Get file content for peeking.
PATH is the path of the file.  LINUM is the starting line.  N is
the number of lines.

If there's no buffer visiting PATH currently, create a new
temporary buffer for it.  They will be killed by `citre-abort'."
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
  "Move current location N steps forward.
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
  "Scroll N lines forward.
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
  "Return the border to be used in peeking window."
  (if (display-graphic-p)
      (propertize "\n" 'face 'citre-peek-border-face)
    (propertize
     (concat (make-string (1- (window-body-width)) ?-) "\n")
     'face (list :inherit 'default
                 :foreground
                 (face-attribute 'citre-peek-border-face
                                 :background)))))

(defun citre-peek--post-command-function ()
  "Deal with content & display updating when peeking."
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
  ;; Fetch informations to show
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
  ;; Setup environment for peeking
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
  (add-hook 'post-command-hook #'citre-peek--post-command-function))

(defun citre-peek-next-line ()
  "Peek next line."
  (interactive)
  (citre-peek--line-forward 1))

(defun citre-peek-prev-line ()
  "Peek previous line."
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
  (remove-hook 'post-command-hook #'citre-peek--post-command-function))

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
                     (citre-get-field 'file record)
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

(defun citre-jump-completing-read ()
  "Jump to definition of the symbol at point.
When there are more than 1 possible definitions, it will let you
choose one in the minibuffer.

The kind, line number and path information are stored in
\\='citre-kind, \\='citre-linum and \\='citre-path properties of
each candidate, so if you want to build a more informative UI
using some minibuffer completing framework, you can use them
directly."
  (interactive)
  (let ((locations (citre-get-definition-locations))
        (target nil))
    (pcase (length locations)
      (0 (user-error "Can't find definition"))
      (1 (citre--open-file-and-goto-line
          (citre--get-property (car locations) 'path)
          (citre--get-property (car locations) 'linum)))
      (_ (setq target
               (completing-read "location: " locations nil t))
         (citre--open-file-and-goto-line
          (citre--get-property target 'path)
          (citre--get-property target 'linum))))))

;;;;; Commands

(defun citre-jump ()
  "Jump to definition of the symbol at point."
  (interactive)
  (let ((marker (point-marker)))
    (if (overlayp citre-peek--ov)
        (progn
          (let* ((loc (nth citre-peek--location-index
                           citre-peek--locations)))
            (citre-peek-abort)
            (citre--open-file-and-goto-line
             (citre--get-property loc 'path)
             (citre--get-property loc 'linum))))
      (call-interactively citre-jump-command))
    (ring-insert citre--marker-ring marker)
    (run-hooks 'citre-after-jump-hook)))

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

(defvar citre-completion-in-region-function-orig nil
  "This stores the original `completion-in-region-function'.")

(defun citre-get-completions (&optional symbol)
  "Get completions of symbol at point, or SYMBOL if it's non-nil.
The result is a list of strings, each string is the complete tag
name, with text properties containing the kind and signature."
  (let ((symbol (or symbol (thing-at-point 'symbol)))
        (match (if citre-get-completions-by-substring
                   'substring 'prefix))
        (candidate-str-generator
         (lambda (record)
           (citre--propertize
            (citre-get-field 'tag record)
            record 'kind 'signature))))
    (unless symbol
      (user-error "No symbol at point"))
    (cl-map 'list candidate-str-generator
            (citre-get-records symbol match))))

(defun citre-completion-in-region (start end collection &optional predicate)
  "A function that replace default `completion-in-region-function'.
This completes the text between START and END using COLLECTION.
PREDICATE says when to exit.

When there are multiple candidates, this uses standard
`completing-read' interface, while the default one in Emacs pops
a *Completions* buffer to show them.  When combined with some
minibuffer completion framework, it's more user-friendly then the
default one."
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
      (insert (substring-no-properties completion)))))

;;;;; Commands

(defun citre-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
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
          ;; means to try next completion function when current completion
          ;; table fails to match the text at point (see the docstring of
          ;; `completion-at-point-functions').  This is the desired behavior
          ;; but actually it breaks our substring completion.  This is a bug
          ;; of Emacs, see the FIXME comment in the code of
          ;; `completion--capf-wrapper'.  I believe I've fixed it, so let's
          ;; leave this line commented rather than delete it, and see if my
          ;; patch will get itself into Emacs.

          ;; It actually doesn't make much a difference.  Now our completion
          ;; function works well, the only problem is it won't fallback to
          ;; the next one when no tags are matched, which I believe also
          ;; happens in other completion functions.

          ;; :exclusive 'no
          )))

;;;; Misc commands

(defun citre-show-project-root ()
  "Show project root of current file in buffer.
Use this command to see if citre detects the project root
corectly."
  (interactive)
  (if (citre--project-root)
      (message (citre--project-root))
    (user-error "Buffer is not visiting a file")))

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
    (setf (alist-get (citre--project-root)
                     citre--project-info-alist nil nil #'equal)
          '(:size nil :tags-recipe nil :tags-use nil))
    (citre--write-project-size)
    (require 'xref)
    (add-hook 'xref-backend-functions #'citre-xref-backend nil t)
    (add-hook 'completion-at-point-functions
              #'citre-completion-at-point nil t)
    (setq citre-completion-in-region-function-orig
          completion-in-region-function)
    (setq completion-in-region-function #'citre-completion-in-region))
   (t
    (setq citre--project-info-alist
          (cl-delete (citre--project-root)
                     citre--project-info-alist
                     :key #'car :test #'equal))
    (remove-hook 'xref-backend-functions #'citre-xref-backend t)
    (remove-hook 'completion-at-point-functions #'citre-completion-at-point t)
    (setq completion-in-region-function
          citre-completion-in-region-function-orig))))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre.el ends here
