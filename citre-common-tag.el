;;; citre-common-tag.el --- Data structure of source code index items -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 27 Sep 2021
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

;; A tag is a record of an identifier in a source tree.  It could contain
;; information such as what's the name of the identifier, where does it appear,
;; what's the kind of it (function? variable?), etc.  If we compare a source
;; tree to a book, then a tag is an item in its back-of-book index.

;; There are some programs that generate a file or database containing all tags
;; they could find in a source tree.  Ctags creates tags in a plain text file
;; called "tags file".  Gtags creates tags in a set of binary databases, and
;; the "global" program could read it and print them in plain text.  Ctags and
;; gtags both supports multiple languages, but there are also some programs
;; that only support one language, like hasktags, gotags, ripper-tags.  They
;; all create tags files in ctags format.

;; The "tag data structure" defined in this file is an elisp object that models
;; a tag created by such a program.  It is closely modeled after tags in ctags
;; format.  APIs are provided to create/modify a tag and get information from
;; it.

;; A tag, as defined in this file, is a table of field -> value pairs.  The
;; fields are symbols, and a tag can contain arbitrary amount of fields with
;; arbitrary names.  There are no restrictions on the type of values, but since
;; tags are usually get from a tags file (in plain text), or the output of a
;; program (in plain text too), it's suggested to keep values as strings.

;; We do have some specifications on some fields and their values.  They
;; closely follows the Universal Ctags tags file format, see the manpages
;; tags(5) and ctags-client-tools(7), but notice the escape rules in the
;; manpages don't apply: our field values don't contain escape characters,
;; except for some special ones, like the `pattern' field.

;; - `name': The name of the identifier.

;; - `input': The path to the file containing the identifier.  This is
;;   presented in ctags format, but there are no guarantee whether it's
;;   absolute or relative, so use `ext-abspath' instead unless you are
;;   recording a value straight from a tags file.

;; - `pattern': A string that may contain a line number and a search pattern to
;;   locate the tag.  We have APIs to create and parse patterns so you don't
;;   need to care about the actual structure.

;; - `kind': The kind of the tag, like "function", "variable" or
;;   "class"... This is presented in ctags format, but sometimes it uses a
;;   single letter (e.g., "f" for "function"), which is not very readable.  So
;;   use `ext-kind-full' instead unless you are recording a value straight from
;;   a tags file.

;; - `line': The line number containing the identifier.  Should be a string.

;; - `end': If the identifier is in its definition, this is the line number
;;   where the definition ends.  Should be a string.

;; - `extras': Comma separated list of extra properties of the tag, e.g.,
;;   "fileScope,qualified" means it's an identifier that has file scope, and is
;;   qualified (which means the name is prefixed by its namespace).  See "$
;;   ctags --list-extras" to know the properties.

;; - `language': The language around the identifier.  For now Citre doesn't use
;;   this field in a generic way so you could record it as it is from its
;;   source.

;; - `typeref': The type of the identifier.  The value is 2 parts separated by
;;   a colon, like "struct:structName" or "union:unionName".  If a single name
;;   could describe the type, use "typename:name", where "typename" is a
;;   placeholder with no actual meaning.

;; - `scope': The scope which the identifier belongs to.  The value is 2 parts
;;   separated by a colon, like "union:unionName".

;; - `ext-abspath': The absolute path of the file containing the identifier.
;;   When the file is a remote one, this should be a remote file name.

;; - `ext-kind-full': The kind of the tag.  When it's possible to get the
;;   full-length kind (like "function") rather than single-letter kind (like
;;   "f"), this should be the full-length one, otherwise it can be
;;   single-letter.

;; Here's a table of whether a field affects locating & displaying a tag.  See
;; `citre-locate-tag' and `citre-make-tag-str' for details.

;; | field         | locating | displying | remarks |
;; |---------------|----------|-----------|---------|
;; | name          | yes      | yes       |         |
;; | input         |          |           | ^1      |
;; | pattern       | yes      | yes       |         |
;; | kind          |          |           | ^2      |
;; | line          | yes      | yes       |         |
;; | end           |          |           |         |
;; | extras        |          | yes       | ^3      |
;; | language      |          |           |         |
;; | typeref       |          | yes       |         |
;; | scope         |          | yes       |         |
;; | ext-abspath   | yes      | yes       |         |
;; | ext-kind-full |          | yes       |         |

;; ^1: Citre uses `ext-abspath' instead.
;; ^2: Citre uses `ext-kind-full' instead.
;; ^3: Citre only displays whether it's a reference tag (whether the value
;;     contains "reference")

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common-util)
(require 'cl-lib)
(require 'rx)

;; Suppress compiler warning

(eval-when-compile (require 'xref))

;; We use these functions only in xref buffer, and they will be avaliable at
;; that time.
(declare-function xref--item-at-point "xref" ())
(declare-function xref-item-location "xref" (arg &rest args))
(declare-function xref-location-group "xref" (location))
(declare-function xref-location-line "xref" (location))

;;;; User Options

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after jumping to a tag."
  :type 'hook
  :group 'citre)

(defcustom citre-tag-pattern-search-limit 50000
  "The limit of chars to go when searching for a pattern in a tag."
  :type 'integer
  :group 'citre)

(defface citre-tag-annotation-face
  '((((background light))
     :foreground "#666666" :slant italic)
    (t
     :foreground "#c0c0c0" :slant italic))
  "Face used for annotations when presenting a tag.
Annotations include kind, type, etc."
  :group 'citre)

(defface citre-tag-path-face
  '((t :inherit font-lock-function-name-face))
  "Face used for the path when presenting a tag."
  :group 'citre)

(defcustom citre-tag-annotation-separator
  (propertize "/" 'face 'citre-tag-annotation-face)
  "The separator between kind and type in annotation."
  :type 'string
  :group 'citre)

(defcustom citre-tag-annotation-separator-for-scope
  (propertize "@" 'face 'citre-tag-annotation-face)
  "The separator between kind/type and scope in annotation."
  :type 'string
  :group 'citre)

(defcustom citre-tag-reference-mark
  (propertize "<R>" 'face 'citre-tag-annotation-face)
  "Mark added for references in tags."
  :type 'string
  :group 'citre)

(defcustom citre-tag-missing-file-mark
  (propertize "!" 'face 'warning)
  "Mark added before missing files in tags."
  :type 'string
  :group 'citre)

;;;; Internals

;;;;; Lookup tables

;; NOTE: This table is manually created, and the reason is some of the
;; languages use the same extension, e.g., Matlab and ObjectiveC, so we have to
;; manually pick one.  Run `ctags --list-map-extensions' and `ctags
;; --list-map-patterns' to see the built-in map of ctags. When you find
;; languages in the table marked as "not supported by ctags" are now supported,
;; update them.

(defvar citre-tag--extension-lang-table
  #s(hash-table
     test equal
     data
     ("inp" "Abaqus"
      "abc" "Abc"
      "ada" "Ada" "adb" "Ada" "ads" "Ada"
      "ant" "Ant"
      "asc" "Asciidoc" "adoc" "Asciidoc" "asciidoc" "Asciidoc"
      "asm" "Asm" "s" "Asm"
      "asa" "Asp" "asp" "Asp"
      "ac" "Autoconf" "in" "Autoconf"
      "au3" "AutoIt"
      "am" "Automake"
      "awk" "Awk" "gawk" "Awk" "mawk" "Awk"
      "bas" "Basic" "bi" "Basic" "bb" "Basic" "pb" "Basic"
      "bet" "BETA"
      "bib" "BibTeX"
      "clj" "Clojure" "cljs" "Clojure" "cljc" "Clojure"
      "cmake" "CMake" "txt" "CMake"
      "c" "C"
      "h" "C++" "c++" "C++" "h++" "C++"
      "cc" "C++" "hh" "C++" "cp" "C++" "hp" "C++"
      "cpp" "C++" "hpp" "C++" "tpp" "C++" "cxx" "C++" "hxx" "C++" "inl" "C++"
      "css" "CSS"
      "cs" "C#"
      "ctags" "Ctags"
      "cbl" "Cobol" "cob" "Cobol"
      "cu" "CUDA" "cuh" "CUDA"
      "d" "D" "di" "D"
      "diff" "Diff" "patch" "Diff"
      "dtd" "DTD" "mod" "DTD"
      "dts" "DTS" "dtsi" "DTS"
      "bat" "DosBatch" "cmd" "DosBatch"
      "e" "Eiffel"
      "ex" "Elixir" "exs" "Elixir"
      "elm" "Elm"
      "el" "EmacsLisp"
      "erl" "Erlang" "hrl" "Erlang"
      "fal" "Falcon" "ftd" "Falcon"
      "as" "Flex" "mxml" "Flex"
      "f" "Fortran" "for" "Fortran" "ftn" "Fortran"
      "f77" "Fortran" "f90" "Fortran" "f95" "Fortran"
      "f03" "Fortran" "f08" "Fortran" "f15" "Fortran"
      "fy" "Fypp"
      "gdbinit" "Gdbinit" "gdb" "Gdbinit"
      "go" "Go"
      "hx" "Haxe"
      "hs" "Haskell"
      "html" "HTML" "htm" "HTML"
      "ini" "Iniconf" "conf" "Iniconf"
      "inko" "Inko"
      "itcl" "ITcl"
      "java" "Java"
      "properties" "JavaProperties"
      "js" "JavaScript" "jsx" "JavaScript" "mjs" "JavaScript"
      "json" "JSON"
      "jl" "Julia"
      "kt" "Kotlin" "kts" "Kotlin"
      "lds" "LdScript" "ld" "LdScript" "ldi" "LdScript" "scr" "LdScript"
      "cl" "Lisp" "clisp" "Lisp" "lisp" "Lisp" "lsp" "Lisp" "l" "Lisp"
      "lhs" "LiterateHaskell"
      "lua" "Lua"
      "m4" "M4" "spt" "M4"
      "1" "Man" "2" "Man" "3" "Man" "4" "Man" "5" "Man" "6" "Man" "7" "Man"
      "8" "Man" "9" "Man" "3pm" "Man" "3stap" "Man" "7stap" "Man"
      "makefile" "Make" "gnumakefile" "Make" "mak" "Make" "mk" "Make"
      "md" "Markdown" "mkd" "Markdown" "markdown" "Markdown"
      "m" "Matlab"
      "myr" "Myrddin"
      "nsi" "NSIS" "nsh" "NSIS"
      "mm" "ObjectiveC"
      "ml" "OCaml" "mli" "OCaml" "aug" "OCaml"
      "passwd" "Passwd"
      "p" "Pascal" "pas" "Pascal"
      "pl" "Perl" "pm" "Perl" "ph" "Perl" "plx" "Perl" "perl" "Perl"
      "p6" "Perl6" "pm6" "Perl6" "pl6" "Perl6"
      "php" "PHP" "php3" "PHP" "php4" "PHP" "php5" "PHP"
      "php7" "PHP" "phtml" "PHP"
      "pod" "Pod"
      "ps1" "PowerShell" "psm1" "PowerShell"
      "proto" "Protobuf"
      "pp" "PuppetManifest"
      "py" "Python" "pyx" "Python" "pxd" "Python" "pxi" "Python"
      "scons" "Python" "wsgi" "Python"
      "hx" "QemuHX"
      "r" "R" "q" "R"
      "rexx" "REXX" "rx" "REXX"
      "robot" "Robot"
      "spec" "RpmSpec"
      "rst" "ReStructuredText" "rest" "ReStructuredText"
      "rb" "Ruby" "ruby" "Ruby"
      "rs" "Rust"
      "scm" "Scheme" "sm" "Scheme" "sch" "Scheme"
      "scheme" "Scheme" "rkt" "Scheme"
      "scss" "SCSS"
      "sh" "Sh" "bsh" "Sh" "bash" "Sh" "ksh" "Sh" "zsh" "Sh" "ash" "Sh"
      "sl" "SLang"
      "sml" "SML" "sig" "SML"
      "sql" "SQL"
      "service" "SystemdUnit" "socket" "SystemdUnit" "device" "SystemdUnit"
      "mount" "SystemdUnit" "automount" "SystemdUnit" "swap" "SystemdUnit"
      "target" "SystemdUnit" "path" "SystemdUnit" "timer" "SystemdUnit"
      "snapshot" "SystemdUnit" "slice" "SystemdUnit"
      "stp" "SystemTap"
      "stpm" "SystemTap"
      "tcl" "Tcl" "tk" "Tcl" "wish" "Tcl" "exp" "Tcl"
      "tex" "Tex"
      "ttcn" "TTCN" "ttcn3" "TTCN"
      "t2t" "Txt2tags"
      "ts" "TypeScript"
      "vr" "Vera" "vri" "Vera" "vrh" "Vera"
      "v" "Verilog"
      "sv" "SystemVerilog" "svh" "SystemVerilog" "svi" "SystemVerilog"
      "vhdl" "VHDL" "vhd" "VHDL"
      "vimrc" "Vim" "_vimrc" "Vim" "gvimrc" "Vim" "_gvimrc" "Vim"
      "vim" "Vim" "vba" "Vim"
      "rc" "WindRes"
      "y" "YACC"
      "repo" "YumRepo"
      "zep" "Zephir"
      "glade" "Glade"
      "pom" "Maven2"
      "plist" "PlistXML"
      "rng" "RelaxNG"
      "svg" "SVG"
      "xml" "XML"
      "xsl" "XSLT" "xslt" "XSLT"
      "yml" "Yaml"
      "varlink" "Varlink"
      ;; Following extensions are not in the default language map of Universal
      ;; Ctags.
      "eex" "Elixir"
      "vue" "JavaScript"
      "dpr" "Pascal" "int" "Pascal" "dfm" "Pascal"
      "erb" "Ruby" "haml" "Ruby" "rake" "Ruby" "slim" "Ruby"
      "tcsh" "Sh"
      "tsx" "TypeScript"
      ;; Following languages are not officially supported by Universal Ctags.
      "coffee" "CoffeeScript" "litcoffee" "CoffeeScript"
      "cr" "Crystal" "ecr" "Crystal"
      "dart" "Dart"
      "fs" "F#" "fsi" "F#" "fsx" "F#"
      "dsp" "Faust" "lib" "Faust"
      "gradle" "Groovy" "groovy" "Groovy" "jenkinsfile" "Groovy"
      "nim" "Nim"
      "nix" "Nix"
      "org" "Org"
      "scala" "Scala"
      "swift" "Swift"
      "vala" "Vala" "vapi" "Vala"))
  "Hash table of file extensions and the corresponding languages.
File extension (or the file name, if it doesn't have an
extension) are downcased first, then used as keys in this
table.

This is for guessing the language of a tag (the `extra-lang'
field) based on its file name when the `language' field is not
presented.")

;;;;; Extra extension fields

(defvar citre-tag-extra-ext-fields-table
  #s(hash-table
     test eq
     data
     (extra-line
      citre--get-line-from-tag
      extra-matched-str
      citre--get-matched-str-from-tag
      extra-lang
      citre--get-lang-from-tag))
  "Hash table for getting extra extension fields from tags.
It's used by `citre-get-tag-field'. Its keys are valid FIELD
argument values for `citre-get-tag-field', and values are
functions that return the value of the fields.  The arguments of
the functions are:

- TAG: The tag to get field from.

If the required field can't be calculated, the functions should
return nil, rather than signal an error.  The functions should
document which fields it may use to calculate the required field.

Packages that uses citre-tag.el can extend this table.  They
should not modify existing key-value pairs in this table, and the
added keys should be prefixed by a namespace to avoid conflict.")

(defun citre--get-line-from-tag (tag)
  "Get the line number from TAG.
This returns a number or nil.  It tries these in turn:

- Use the `line' field directly.
- Use the `pattern' field if it contains the line number.
- Return nil."
  (or (when-let (line (citre-get-tag-field-primitive 'line tag))
        (string-to-number line))
      (car (citre-split-tag-pattern
            (citre-get-tag-field-primitive 'pattern tag)))))

(defun citre--get-matched-str-from-tag (tag)
  "Get the string encoded by the search pattern in TAG.
Returns nil if the `pattern' field doesn't exist or doesn't
contain a search pattern."
  (when-let* ((pat (citre-get-tag-field-primitive 'pattern tag))
              (search-pat (nth 1 (citre-split-tag-pattern pat))))
    (car (citre-parse-search-pattern search-pat))))

(defun citre--get-lang-from-tag (tag)
  "Get language from TAG.
It tries these in turn:

- Use the `language' field directly.
- Guess the language based on the `input' or `ext-abspath' field.
  See `citre-tag--extension-lang-table'.
- Return the file extension, or the filename if it doesn't have
  an extension.
- Return nil."
  (or (citre-get-tag-field-primitive 'language tag)
      (when-let ((input (or (citre-get-tag-field-primitive 'input tag)
                            (citre-get-tag-field-primitive 'ext-abspath tag)))
                 (extension (citre-file-name-extension input)))
        (or (gethash (downcase extension) citre-tag--extension-lang-table)
            extension))))

;;;; APIs

;;;;; Constructor

(defun citre-make-tag (&rest pairs)
  "Create a tag.
See the commentary section in citre-tag.el to know the
specifications of a tag.

PAIRS should form a sequence of field-value pairs to write into
this tag.  Fields are symbols, values are normally strings.  An
example is:

    (citre-make-tag \\='name \"symbol\" \\='input \"file.el\")

If PAIRS is nil, return an empty tag."
  (let* ((idx 0)
         (len (length pairs))
         field-idx value-idx
         (tag (make-hash-table :test #'eq :size (max 16 len))))
    (while (< (progn (setq field-idx (* 2 idx))
                     (setq value-idx (1+ field-idx)))
              len)
      (puthash (nth field-idx pairs) (nth value-idx pairs) tag)
      (cl-incf idx))
    tag))

;;;;; Setter

(defun citre-set-tag-field (field value tag)
  "Set FIELD in TAG to be VALUE.
FIELD should be a symbol.

This returns the modified tag."
  (puthash field value tag)
  tag)

;;;;; Getter

(defun citre-get-tag-field-primitive (field tag)
  "Get FIELD in TAG.
FIELD should be a symbol.

`citre-get-tag-field' is similar but different, See its
docstring.  Most of the time you should use it instead."
  (gethash field tag))

;;;;; Helpers for creating/modifying a tag

(defun citre-create-tag-search-pattern (str &optional from-beg to-end)
  "Create a search pattern using STR.
FROM-BEG: whether STR begins from the beginning of a line.
TO-END: whether STR ends at the end of a line.

This returns a valid `pattern' field of a tag."
  (setq str (replace-regexp-in-string (rx (or "\\" "/" (seq "$" line-end)))
                                      "\\\\\\&" str))
  (setq str (concat (if from-beg "^" "")
                    str
                    (if to-end "$" "")))
  (setq str (concat "/" str "/;\"")))

(defun citre-make-tag-of-current-location (name)
  "Make a tag of the current line, with the name field being NAME."
  (let* ((line-content (string-trim
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
         (pat (citre-create-tag-search-pattern line-content)))
    (citre-make-tag 'name name
                    'ext-abspath (buffer-file-name)
                    'pattern pat
                    'line (number-to-string (line-number-at-pos)))))

(defun citre-make-tag-of-current-xref-item (name)
  "Make a tag for current item in xref buffer.
The name field is set to NAME."
  (when-let* ((item (xref--item-at-point))
              (location (xref-item-location item))
              (file (xref-location-group location))
              (line (xref-location-line location)))
    (citre-make-tag 'name name
                    'ext-abspath (expand-file-name file)
                    'line (number-to-string line))))

;;;;; Helpers for getting information from a tag

;;;;;; Extended getter

(defun citre-get-tag-field (field tag &optional after-colon)
  "Get FIELD from TAG.
FIELD should be a symbol.  When it is `line' or `end', an integer
is returned, instead of a string.

FIELD could also be:

- `extra-line': The line number of the tag.
- `extra-lang': The language.
- `extra-matched-str': The string encoded by the search pattern in tag.

These are called extra extension fields.  They are not contained
in the tag itself, but calculated in real-time (e.g., when the
tag doesn't have a `line' field, but the `pattern' field records
the line number, `extra-line' gives you that line number).  See
`citre-tag-extra-ext-fields-table' for the details.

AFTER-COLON: if it's non-nil, the field is splitted at the first
colon, and the part after it is returned.  This is for fields
like `scope' or `typeref'.  In a tagline they may look like this:

    typeref:struct:structName

when getting the `typeref' field from this tag, a non-nil
AFTER-COLON gives \"structName\", while nil gives
\"struct:structName\".

If you use this option on a field that doesn't contain a colon,
the whole field is returned."
  (let ((maybe-split (if after-colon
                         #'citre-string-after-1st-colon
                       #'identity))
        value)
    (if-let ((method (gethash field citre-tag-extra-ext-fields-table)))
        (setq value (funcall method tag))
      (setq value (gethash field tag))
      (when value
        (pcase field
          ((or 'line 'end) (when-let ((val (gethash field tag)))
                             (string-to-number val)))
          (_ (funcall maybe-split (gethash field tag))))))))

;;;;;; Pattern field processing

(defun citre-split-tag-pattern (pattern)
  "Split the pattern PATTERN.
PATTERN should be the pattern field in a tag.

This returns a list (LINUM PAT) where:

- LINUM is the line number PATTERN contains (an integer), or nil
  if not presented.
- PAT is the search pattern that PATTERN contains (a string), or
  nil if not presented."
  (let (line pat)
    (pcase pattern
      ;; Line number pattern
      ((guard (string-match (rx (group (+ num)) ";\"" line-end) pattern))
       (setq line (string-to-number (match-string 1 pattern))))
      ;; Search/combined pattern
      ((guard (string-match
               (rx (group (* num)) (opt ";")
                   (group (or "/" "?") (* not-newline) (or "/" "?")) ";\""
                   line-end)
               pattern))
       (let ((num (match-string 1 pattern)))
         (setq line (unless (string-empty-p num) (string-to-number num))))
       (setq pat (match-string 2 pattern)))
      (_ (error "Invalid PATTERN")))
    (list line pat)))

(defun citre-parse-search-pattern (pattern)
  "Parse the search pattern PATTERN.
PATTERN is a string in the form of \"/pat/\" or \"?pat?\".  It
should come from the pattern field of a tagline.

This returns (STR FROM-BEG TO-END), where STR is the (literal)
string that PATTERN matches.  If FROM-BEG is non-nil, the string
should begin from the beginning of a line.  The same for TO-END.

The reason we need this function is the pattern field is actually
not a regexp.  It only adds \"^\" and \"$\", and escape several
chars.  See the code of this function for the detail."
  (let* ((direction (pcase (aref pattern 0)
                      (?/ 'forward)
                      (?? 'backward)))
         ;; Remove the surrounding "/"s or "?"s.
         (pattern (substring pattern 1 -1))
         (from-beg (unless (string-empty-p pattern) (eq ?^ (aref pattern 0))))
         ;; Check if there's an unescaped trailing "$".
         (to-end (string-match (rx (or (not (any "\\")) line-start)
                                   (* "\\\\") "$"
                                   line-end)
                               pattern))
         ;; Remove the beginning "^" and trailing "$"
         (pattern (substring pattern (if from-beg 1 0) (if to-end -1 nil))))
    (when-let ((backslash-idx
                (citre-string-match-all-escaping-backslash pattern)))
      (let ((last 0)
            (i nil)
            (parts nil))
        (while (setq i (pop backslash-idx))
          (push (substring pattern last i) parts)
          (setq last (+ 2 i))
          (let ((char (aref pattern (1+ i))))
            (push (pcase char
                    (?\\ "\\")
                    ((and ?$ (guard (eq i (- (length pattern) 2)))) "$")
                    ((and ?? (guard (eq direction 'backward))) "?")
                    ((and ?/ (guard (eq direction 'forward))) "/")
                    (_ (error "Invalid escape sequence")))
                  parts)))
        (push (substring pattern last) parts)
        (setq pattern (apply #'concat (nreverse parts)))))
    (list pattern from-beg to-end)))

;;;;;; Locate a tag

(defun citre-locate-tag (tag &optional use-linum)
  "Find TAG in current buffer.
Returns the position to goto, or line number if USE-LINUM is
non-nil.  Current buffer should be the buffer visiting the file
containing the tag.

The search is helped by:

- The pattern field.
- The line field, if the pattern is not a combined
  pattern (i.e., not contatining the line number).
- The name of the tag.

This function does its best to find the tag if the file has been
changed, and even when the line including the tag itself has been
changed.  See the code for details.  If the search fails
completely, it will return the beginning position of the file.

This function has no side-effect on the buffer.  Upper components
could wrap this function to provide a desired UI for jumping to
the position of a tag."
  (pcase-let*
      ((name (citre-get-tag-field 'name tag))
       (pat (citre-get-tag-field 'pattern tag))
       (`(,line ,pat) (when pat (citre-split-tag-pattern pat)))
       (line (or (citre-get-tag-field 'line tag) line))
       (`(,str ,from-beg ,to-end)
        (when pat (citre-parse-search-pattern pat)))
       (pat-beg (if from-beg "^" ""))
       (pat-end (if to-end "$" ""))
       (lim citre-tag-pattern-search-limit))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (when line (forward-line (1- line)))
        (or
         (when pat
           (or
            ;; Search for the whole line.
            (citre-find-nearest-regexp
             (concat pat-beg (regexp-quote str) pat-end)
             lim)
            ;; Maybe the indentation or trailing whitespaces has changed, or
            ;; something is added after.  From now on we also use case-fold
            ;; search to deal with projects that uses a case-insensitive
            ;; language and don't have a consistant style on it.
            (citre-find-nearest-regexp
             (concat pat-beg (rx (* space)) (regexp-quote (string-trim str)))
             lim 'case-fold)
            ;; The content is changed.  Try cutting from the end of the tag
            ;; name and search.
            (when-let ((name name)
                       (bound (when (let ((case-fold-search nil))
                                      (string-match (regexp-quote name) str))
                                (match-end 0)))
                       (str (substring str 0 bound)))
              (citre-find-nearest-regexp
               (concat pat-beg (rx (* space)) (regexp-quote (string-trim str)))
               lim 'case-fold))))
         ;; Last try: search for the tag name.
         (when name
           (or
            (citre-find-nearest-regexp (concat (rx symbol-start)
                                               (regexp-quote name)
                                               (rx symbol-end))
                                       lim 'case-fold)
            (citre-find-nearest-regexp (regexp-quote name)
                                       lim 'case-fold))))
        (if use-linum (line-number-at-pos) (point))))))

(defun citre-goto-tag (tag &optional window)
  "Jump to the location of TAG.
WINDOW can be:

- nil: Use current window.
- `other-window': Use other window.
- `other-window-noselect': Use other window but don't select it."
  (let ((path (citre-get-tag-field 'ext-abspath tag)))
    (unless path
      (error "TAG doesn't have the ext-abspath field"))
    (unless (citre-non-dir-file-exists-p path)
      (user-error "File %s doesn't exist" path))
    (let* ((buf (find-file-noselect path))
           (current-buf (current-buffer))
           (current-window (selected-window)))
      (if window
          (pop-to-buffer
           buf
           '(display-buffer-use-some-window . ((inhibit-same-window . t)
                                               (inhibit-switch-frame . t)))
           (when (eq window 'other-window-noselect) 'norecord))
        (pop-to-buffer buf '(display-buffer-same-window)))
      (goto-char (citre-locate-tag tag))
      (run-hooks 'citre-after-jump-hook)
      (when (eq window 'other-window-noselect)
        (select-window current-window)
        (pop-to-buffer current-buf '(display-buffer-same-window) 'norecord)))))

;;;;;; Show a tag

;;;;;;; Internals

(defun citre--reduce-anonymous-value (value)
  "Reduce \"__anon*\" parts in VALUE to \"__anon\".
When Ctags generates anonymous tags, they can be used in typeref
or scope fields.  For example, a symbol defined in an anonymous
union scope could be:

  C_lastcookie ... scope:union:cnode::__anon1146df5f01a

Applying this to the scope field value makes it looks tidier."
  ;; Notice there can be continuous "__anon*"s, meaning nested anonymous
  ;; scopes.
  (replace-regexp-in-string (rx (group (or line-start ":"))
                                (group "__anon" (+ (not (any ":"))))
                                (group (or line-end ":")))
                            "\\1__anon\\3" value))

(defun citre--make-tag-name-str (tag prop)
  "Generate a string to display the name of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let ((name (citre-get-tag-field 'name tag))
        (face
         (pcase (citre-get-tag-field 'ext-kind-full tag)
           ("class" 'font-lock-type-face)
           ((or "const" "constant") 'font-lock-constant-face)
           ("macro" 'font-lock-keyword-face)
           ((or "function" "f") 'font-lock-function-name-face)
           ("method" 'font-lock-function-name-face)
           ("struct" 'font-lock-type-face)
           ((or "typedef" "type") 'font-lock-type-face)
           ((or "variable" "var" "v") 'font-lock-variable-name-face))))
    (when name
      (concat (or (plist-get prop :prefix) "")
              (if face (propertize name 'face face) name)
              (or (plist-get prop :suffix) "")))))

(defun citre--make-tag-annotation-str (tag prop)
  "Generate a string to display the annotation of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let* ((kind (unless (plist-get prop :no-kind)
                 (citre-get-tag-field 'ext-kind-full tag)))
         (type (unless (plist-get prop :no-type)
                 (citre-get-tag-field 'typeref tag)))
         (scope (unless (plist-get prop :no-scope)
                  (citre-get-tag-field 'scope tag)))
         (extras (citre-get-tag-field 'extras tag))
         (reference
          (unless (plist-get prop :no-reference)
            (and extras
                 (citre-csv-contain "reference" extras))))
         (reference (when reference citre-tag-reference-mark))
         (ref-first (plist-get prop :reference-first))
         (face 'citre-tag-annotation-face))
    ;; "typename:" is a placeholder. It doesn't offer useful info, so we can
    ;; drop it.  We don't drop it if it is, say, "struct:" or "union:".
    (when (and type (string-prefix-p "typename:" type))
      (setq type (substring type (length "typename:"))))
    (unless (plist-get prop :full-anonymous-name)
      (when type (setq type (citre--reduce-anonymous-value type)))
      (when scope (setq scope (citre--reduce-anonymous-value scope))))
    (when (or kind type scope reference)
      (concat
       (propertize (or (plist-get prop :prefix) "") 'face face)
       (if ref-first (or reference ""))
       (concat (propertize (or kind "") 'face face)
               (if (and kind type) citre-tag-annotation-separator "")
               (propertize (or type "") 'face face))
       (concat
        (if scope
            citre-tag-annotation-separator-for-scope "")
        (propertize (or scope "") 'face face))
       (if (not ref-first) (or reference ""))
       (propertize (or (plist-get prop :suffix) "") 'face face)))))

(defun citre--make-tag-location-str (tag prop)
  "Generate a string to display the location of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (let* ((abspath (unless (plist-get prop :no-path)
                    (citre-get-tag-field 'ext-abspath tag)))
         (line (unless (plist-get prop :no-line)
                 (citre-get-tag-field 'extra-line tag))))
    (when (or abspath line)
      (concat
       (or (plist-get prop :prefix) "")
       ;; path
       (if abspath
           (concat
            (if (citre-non-dir-file-exists-p abspath) ""
              citre-tag-missing-file-mark)
            (propertize (if-let ((root (plist-get prop :root)))
                            (citre-relative-path abspath root)
                          abspath)
                        'face 'citre-tag-path-face))
         "")
       (if line
           (concat (if abspath "(" "")
                   (propertize (number-to-string line)
                               'face 'warning)
                   (if abspath ")" ""))
         "")
       (or (plist-get prop :suffix) "")))))

(defun citre--make-tag-content-str (tag prop)
  "Return the string recorded in the pattern field of TAG.
PROP controls the format.  See `citre-make-tag-str' for details."
  (if-let ((str (citre-get-tag-field 'extra-matched-str tag)))
      (concat (or (plist-get prop :prefix) "")
              (string-trim str)
              (or (plist-get prop :suffix) ""))
    (when (plist-get prop :ensure)
      (if-let ((path (citre-get-tag-field 'ext-abspath tag)))
          (or (citre-with-file-buffer path
                (goto-char (citre-locate-tag tag))
                (buffer-substring (line-beginning-position)
                                  (line-end-position)))
              "This file doesn't exist.")
        "This file doesn't exist."))))

;;;;;; The API

(cl-defun citre-make-tag-str (tag separator &rest args)
  "Generate a string for TAG for displaying.
ARGS is the components the string should contain, in the order of
presence.  Each element of ARGS is a list of:

  (component :prop val :prop val ...)

Avaliable ones are:

- name: Name of the tag.  It's propertized by font-lock faces
  according to the kind of the tag.

  relevant fields: `name', `ext-kind-full'.

- annotation: Looks like \"kind/type@scope<R>\".  \"<R>\" is a
  mark for reference tags, customizable by
  `citre-definition-reference-mark'.  `:no-kind', `:no-type',
  `:no-scope', `:no-reference' controls the presence of each
  part, `:reference-first' puts the reference mark before other
  parts, `:full-anonymous-name' means don't reduce \"__anon\"
  parts in the type and scope parts, see
  `citre--reduce-anonymous-value'.

  relevant fields: `ext-kind-full', `typeref', `extras'.

- location: Looks like \"path(line)\". `:no-path', `:no-line'
  controls the presence of each part.  When there's only the line
  number, the parentheses around it are omitted.  When `:root' is
  specified, files under it will be displayed relative to it.
  When the path doesn't exist,
  `citre-definition-missing-file-mark' is prefixed to the path.

  relevant fields: `ext-abspath', `extra-line' (which uses `line'
  or `pattern').

- content: The string recorded in the pattern field of TAG.  When
  `:ensure' is non-nil, and the search pattern is not presented,
  get the line content from the file containing the tag.

  relevant fields: `pattern'.  When `:ensure' is non-nil, all
  fields used for locating the tag may be relevant, including
  `ext-abspath', `line', `pattern' and `name'.

All components have `:prefix' and `:suffix' properties to attach
extra prefix and suffix strings to them.  When a component or
some parts of it can't be generated, they are omitted.

SEPARATOR specifies the separator between components.  A space is
used when it's nil."
  (let (parts)
    (dolist (arg args)
      (let ((prop (cdr arg)))
        (push
         (pcase (car arg)
           ('name (citre--make-tag-name-str tag prop))
           ('annotation (citre--make-tag-annotation-str tag prop))
           ('location (citre--make-tag-location-str tag prop))
           ('content (citre--make-tag-content-str tag prop)))
         parts)))
    (string-join (cl-remove nil (nreverse parts) :test #'eq)
                 (or separator " "))))

(provide 'citre-common-tag)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-common-tag.el ends here
