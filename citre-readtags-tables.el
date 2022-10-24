;;; citre-readtags-tables.el --- Lookup tables for citre-readtags -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 16 May 2020
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

;; Lookup tables used by citre-tag.el.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; language -> extension

(defvar citre-readtags--lang-extension-table
  #s(hash-table
     test equal
     data
     ("Abaqus"
      ("inp")
      "Abc"
      ("abc")
      "Ada"
      ("ada" "Ada" "ads" "adb")
      "Ant"
      ("xml" "ant" "build.xml")
      "Asciidoc"
      ("asciidoc" "adoc" "asc")
      "Asm"
      ("S" "s" "ASM" "asm")
      "Asp"
      ("asa" "asp")
      "Autoconf"
      ("ac")
      "AutoIt"
      ("Au3" "aU3" "AU3" "au3")
      "Automake"
      ("am")
      "Awk"
      ("mawk" "gawk" "awk")
      "Basic"
      ("pb" "bb" "bi" "bas")
      "BETA"
      ("bet")
      "BibTeX"
      ("bib")
      "Clojure"
      ("cljc" "cljs" "clj")
      "CMake"
      ("cmake")
      "C"
      ("c")
      "C++"
      ("CXX" "CPP" "H" "C" "inl" "hxx" "hpp" "hp" "hh" "h++" "h" "cxx" "cpp" "cp" "cc" "c++")
      "CSS"
      ("css")
      "C#"
      ("cs")
      "Ctags"
      ("ctags")
      "Cobol"
      ("COB" "CBL" "cob" "cbl")
      "CUDA"
      ("cuh" "cu")
      "D"
      ("di" "d")
      "Diff"
      ("patch" "diff")
      "DTD"
      ("mod" "dtd")
      "DTS"
      ("dtsi" "dts")
      "DosBatch"
      ("cmd" "bat")
      "Eiffel"
      ("e")
      "Elixir"
      ("exs" "ex")
      "Elm"
      ("elm")
      "EmacsLisp"
      ("el")
      "Erlang"
      ("HRL" "hrl" "ERL" "erl")
      "Falcon"
      ("ftd" "fal")
      "Flex"
      ("mxml" "as")
      "Fortran"
      ("F15" "F08" "F03" "F95" "F90" "F77" "FTN" "FOR" "F" "f15" "f08" "f03" "f95" "f90" "f77" "ftn" "for" "f")
      "Fypp"
      ("fy")
      "Gdbinit"
      ("gdb")
      "Go"
      ("go")
      "Haskell"
      ("hs")
      "Haxe"
      ("hx")
      "HTML"
      ("html" "htm")
      "Iniconf"
      ("conf" "ini")
      "Inko"
      ("inko")
      "ITcl"
      ("itcl")
      "Java"
      ("java")
      "JavaProperties"
      ("properties")
      "JavaScript"
      ("mjs" "jsx" "js")
      "JSON"
      ("json")
      "Julia"
      ("jl")
      "Kotlin"
      ("kts" "kt")
      "LdScript"
      ("ldi" "ld" "scr" "lds")
      "Lisp"
      ("lsp" "lisp" "l" "clisp" "cl")
      "LiterateHaskell"
      ("lhs")
      "Lua"
      ("lua")
      "M4"
      ("spt" "m4")
      "Man"
      ("7stap" "3stap" "3pm" "9" "8" "7" "6" "5" "4" "3" "2" "1")
      "Make"
      ("mk" "mak")
      "Markdown"
      ("markdown" "md")
      "MatLab"
      ("m")
      "Myrddin"
      ("myr")
      "NSIS"
      ("nsh" "nsi")
      "ObjectiveC"
      ("h" "m" "mm")
      "OldC++"
      ("H" "C" "inl" "hxx" "hpp" "hp" "hh" "h++" "h" "cxx" "cpp" "cp" "cc" "c++")
      "OldC"
      ("c")
      "OCaml"
      ("aug" "mli" "ml")
      "Pascal"
      ("pas" "p")
      "Perl"
      ("perl" "plx" "ph" "pm" "pl")
      "Perl6"
      ("pl6" "pm" "pm6" "p6")
      "PHP"
      ("phtml" "php7" "php5" "php4" "php3" "php")
      "Pod"
      ("pod")
      "PowerShell"
      ("psm1" "ps1")
      "Protobuf"
      ("proto")
      "PuppetManifest"
      ("pp")
      "Python"
      ("wsgi" "scons" "pxi" "pxd" "pyx" "py")
      "QemuHX"
      ("hx")
      "R"
      ("q" "s" "R" "r")
      "REXX"
      ("rx" "rexx" "cmd")
      "Robot"
      ("robot")
      "RpmSpec"
      ("spec")
      "ReStructuredText"
      ("rst" "reST" "rest")
      "Ruby"
      ("ruby" "rb")
      "Rust"
      ("rs")
      "Scheme"
      ("rkt" "sm" "scm" "scheme" "sch" "SM" "SCM")
      "SCSS"
      ("scss")
      "Sh"
      ("ash" "zsh" "ksh" "bash" "bsh" "SH" "sh")
      "SLang"
      ("sl")
      "SML"
      ("sig" "sml")
      "SQL"
      ("sql")
      "SystemdUnit"
      ("slice" "snapshot" "timer" "path" "target" "swap" "automount" "mount" "device" "socket" "service")
      "SystemTap"
      ("stpm" "stp")
      "Tcl"
      ("exp" "wish" "tk" "tcl")
      "Tex"
      ("tex")
      "TTCN"
      ("ttcn3" "ttcn")
      "Txt2tags"
      ("t2t")
      "TypeScript"
      ("ts")
      "Vera"
      ("vrh" "vri" "vr")
      "Verilog"
      ("v")
      "SystemVerilog"
      ("svi" "svh" "sv")
      "VHDL"
      ("vhd" "vhdl")
      "Vim"
      ("vba" "vim")
      "WindRes"
      ("rc")
      "YACC"
      ("y")
      "YumRepo"
      ("repo")
      "Zephir"
      ("zep")
      "DBusIntrospect"
      ("xml")
      "Glade"
      ("glade")
      "Maven2"
      ("xml" "pom")
      "PlistXML"
      ("plist")
      "RelaxNG"
      ("rng")
      "SVG"
      ("svg")
      "XML"
      ("xml")
      "XSLT"
      ("xslt" "xsl")
      "Yaml"
      ("yml")
      "Varlink"
      ("varlink")))
  "Hash table of languages and their corresponding extensions.
This is for filtering tags in certain languages by their file
name when the `language' field is not presented. See
`citre-readtags-filter-lang'.

Currently this table is not used as we don't want to filter
languages, see the documentation
docs/developer-manual/design-principle.md")

;; Run this snippet to generate `citre-readtags--lang-extension-table'.  The
;; result will be shown in a *Pp Eval Output* buffer, and it can be directly
;; copied into the variable definition.  Make sure to indent them!

;; (let* ((ctags-program (or citre-ctags-program "ctags"))
;;        (output (shell-command-to-string
;;                 (format "%s --quiet --options=NONE --machinable --list-map-extensions"
;;                         ctags-program)))
;;        (output-lines (nthcdr 1 (split-string output "\n" t)))
;;        (output-records (mapcar (lambda (line)
;;                                  (split-string line "\t" t))
;;                                output-lines))
;;        (table (make-hash-table :test #'equal)))
;;   (dolist (record output-records)
;;     (let ((lang (car record))
;;           (ext (nth 1 record)))
;;       (if (gethash lang table)
;;           (push ext (gethash lang table))
;;         (puthash lang (list ext) table))))
;;   (pp-eval-expression table)
;;   (pop-to-buffer "*Pp Eval Output*")
;;   (goto-char (point-min))
;;   (re-search-forward "#s(hash-table.*data")
;;   (replace-match "#s(hash-table\ntest equal\ndata")
;;   (indent-region (point-min) (point-max))
;;   (goto-char (point-min)))

;;;; language -> single-letter kind -> full-length kind

(defvar citre-readtags--kind-name-single-to-full-table
  #s(hash-table
     test equal
     data
     ("Abaqus"
      #s(hash-table
         test equal
         data
         ("a" "assembly" "p" "part" "s" "step"))
      "Abc"
      #s(hash-table
         test equal
         data
         ("s" "section"))
      "Ada"
      #s(hash-table
         test equal
         data
         ("E" "entryspec" "K" "taskspec" "O" "protectspec" "P" "packspec" "R" "subprogspec" "S" "separate" "T" "typespec" "U" "subspec" "V" "varspec" "a" "autovar" "b" "label" "c" "component" "e" "entry" "f" "formal" "i" "identifier" "k" "task" "l" "literal" "n" "constant" "o" "protected" "p" "package" "r" "subprogram" "t" "type" "u" "subtype" "v" "variable" "x" "exception" "y" "anon"))
      "AnsiblePlaybook"
      #s(hash-table
         test equal
         data
         ("p" "play"))
      "Ant"
      #s(hash-table
         test equal
         data
         ("P" "property" "i" "antfile" "p" "project" "t" "target"))
      "Asciidoc"
      #s(hash-table
         test equal
         data
         ("S" "subsection" "T" "l4subsection" "a" "anchor" "c" "chapter" "s" "section" "t" "subsubsection" "u" "l5subsection"))
      "Asm"
      #s(hash-table
         test equal
         data
         ("d" "define" "l" "label" "m" "macro" "s" "section" "t" "type"))
      "Asp"
      #s(hash-table
         test equal
         data
         ("c" "class" "d" "constant" "f" "function" "s" "subroutine" "v" "variable"))
      "AutoIt"
      #s(hash-table
         test equal
         data
         ("S" "script" "f" "func" "g" "global" "l" "local" "r" "region"))
      "Autoconf"
      #s(hash-table
         test equal
         data
         ("c" "condition" "d" "definition" "e" "optenable" "m" "macro" "p" "package" "s" "subst" "t" "template" "w" "optwith"))
      "Automake"
      #s(hash-table
         test equal
         data
         ("D" "data" "L" "library" "M" "man" "P" "program" "S" "script" "T" "ltlibrary" "c" "condition" "d" "directory" "s" "subdir"))
      "Awk"
      #s(hash-table
         test equal
         data
         ("f" "function"))
      "BETA"
      #s(hash-table
         test equal
         data
         ("f" "fragment" "p" "pattern" "s" "slot" "v" "virtual"))
      "Basic"
      #s(hash-table
         test equal
         data
         ("c" "constant" "f" "function" "g" "enum" "l" "label" "t" "type" "v" "variable"))
      "BibTeX"
      #s(hash-table
         test equal
         data
         ("B" "booklet" "I" "incollection" "M" "mastersthesis" "P" "proceedings" "a" "article" "b" "book" "c" "conference" "i" "inbook" "j" "inproceedings" "m" "manual" "n" "misc" "p" "phdthesis" "s" "string" "t" "techreport" "u" "unpublished"))
      "C"
      #s(hash-table
         test equal
         data
         ("D" "macroparam" "L" "label" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member" "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable" "x" "externvar" "z" "parameter"))
      "C#"
      #s(hash-table
         test equal
         data
         ("E" "event" "c" "class" "d" "macro" "e" "enumerator" "f" "field" "g" "enum" "i" "interface" "l" "local" "m" "method" "n" "namespace" "p" "property" "s" "struct" "t" "typedef"))
      "C++"
      #s(hash-table
         test equal
         data
         ("A" "alias" "D" "macroparam" "L" "label" "N" "name" "U" "using" "Z" "tparam" "c" "class" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member" "n" "namespace" "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable" "x" "externvar" "z" "parameter"))
      "CMake"
      #s(hash-table
         test equal
         data
         ("D" "option" "f" "function" "m" "macro" "p" "project" "t" "target" "v" "variable"))
      "CPreProcessor"
      #s(hash-table
         test equal
         data
         ("D" "parameter" "d" "macro" "h" "header"))
      "CSS"
      #s(hash-table
         test equal
         data
         ("c" "class" "i" "id" "s" "selector"))
      "CUDA"
      #s(hash-table
         test equal
         data
         ("D" "macroparam" "L" "label" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member" "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable" "x" "externvar" "z" "parameter"))
      "Clojure"
      #s(hash-table
         test equal
         data
         ("f" "function" "n" "namespace"))
      "Cobol"
      #s(hash-table
         test equal
         data
         ("D" "division" "P" "program" "S" "sourcefile" "d" "data" "f" "fd" "g" "group" "p" "paragraph" "s" "section"))
      "CobolFree"
      #s(hash-table
         test equal
         data
         ("D" "division" "P" "program" "S" "sourcefile" "d" "data" "f" "fd" "g" "group" "p" "paragraph" "s" "section"))
      "CobolVariable"
      #s(hash-table
         test equal
         data
         ("D" "division" "P" "program" "S" "sourcefile" "d" "data" "f" "fd" "g" "group" "p" "paragraph" "s" "section"))
      "Ctags"
      #s(hash-table
         test equal
         data
         ("k" "kind" "l" "langdef"))
      "D"
      #s(hash-table
         test equal
         data
         ("M" "module" "T" "template" "V" "version" "X" "mixin" "a" "alias" "c" "class" "e" "enumerator" "f" "function" "g" "enum" "i" "interface" "l" "local" "m" "member" "n" "namespace" "p" "prototype" "s" "struct" "u" "union" "v" "variable" "x" "externvar"))
      "DBusIntrospect"
      #s(hash-table
         test equal
         data
         ("a" "arg" "i" "interface" "m" "method" "n" "node" "p" "property" "s" "signal"))
      "DTD"
      #s(hash-table
         test equal
         data
         ("E" "entity" "a" "attribute" "e" "element" "n" "notation" "p" "parameterEntity"))
      "DTS"
      #s(hash-table
         test equal
         data
         ("l" "label" "p" "phandler"))
      "Diff"
      #s(hash-table
         test equal
         data
         ("d" "deletedFile" "h" "hunk" "m" "modifiedFile" "n" "newFile"))
      "DosBatch"
      #s(hash-table
         test equal
         data
         ("l" "label" "v" "variable"))
      "Eiffel"
      #s(hash-table
         test equal
         data
         ("c" "class" "f" "feature" "l" "local"))
      "Elixir"
      #s(hash-table
         test equal
         data
         ("a" "macro" "c" "callback" "d" "delegate" "e" "exception" "f" "function" "g" "guard" "i" "implementation" "m" "module" "o" "operator" "p" "protocol" "r" "record" "t" "test" "y" "type"))
      "Elm"
      #s(hash-table
         test equal
         data
         ("a" "alias" "c" "constructor" "f" "function" "m" "module" "n" "namespace" "p" "port" "t" "type"))
      "EmacsLisp"
      #s(hash-table
         test equal
         data
         ("C" "custom" "D" "derivedMode" "G" "group" "H" "face" "M" "minorMode" "T" "theme" "V" "varalias" "a" "alias" "c" "const" "e" "error" "f" "function" "i" "inline" "m" "macro" "s" "subst" "u" "unknown" "v" "variable"))
      "Erlang"
      #s(hash-table
         test equal
         data
         ("d" "macro" "f" "function" "m" "module" "r" "record" "t" "type"))
      "Falcon"
      #s(hash-table
         test equal
         data
         ("c" "class" "f" "function" "i" "namespace" "m" "member" "v" "variable"))
      "Flex"
      #s(hash-table
         test equal
         data
         ("C" "constant" "I" "import" "P" "package" "c" "class" "f" "function" "i" "interface" "l" "localvar" "m" "method" "p" "property" "v" "variable" "x" "mxtag"))
      "Fortran"
      #s(hash-table
         test equal
         data
         ("E" "enum" "L" "local" "M" "method" "N" "enumerator" "P" "prototype" "S" "submodule" "b" "blockData" "c" "common" "e" "entry" "f" "function" "i" "interface" "k" "component" "l" "label" "m" "module" "n" "namelist" "p" "program" "s" "subroutine" "t" "type" "v" "variable"))
      "Fypp"
      #s(hash-table
         test equal
         data
         ("m" "macro"))
      "Gdbinit"
      #s(hash-table
         test equal
         data
         ("D" "document" "d" "definition" "l" "localVariable" "t" "toplevelVariable"))
      "Glade"
      #s(hash-table
         test equal
         data
         ("c" "class" "h" "handler"))
      "Go"
      #s(hash-table
         test equal
         data
         ("M" "anonMember" "P" "packageName" "R" "receiver" "a" "talias" "c" "const" "f" "func" "i" "interface" "m" "member" "n" "methodSpec" "p" "package" "s" "struct" "t" "type" "u" "unknown" "v" "var"))
      "HTML"
      #s(hash-table
         test equal
         data
         ("C" "stylesheet" "I" "id" "J" "script" "a" "anchor" "c" "class" "h" "heading1" "i" "heading2" "j" "heading3"))
      "Haskell"
      #s(hash-table
         test equal
         data
         ("c" "constructor" "f" "function" "m" "module" "t" "type"))
      "Haxe"
      #s(hash-table
         test equal
         data
         ("c" "class" "e" "enum" "i" "interface" "m" "method" "t" "typedef" "v" "variable"))
      "ITcl"
      #s(hash-table
         test equal
         data
         ("C" "common" "c" "class" "m" "method" "p" "procedure" "v" "variable"))
      "Iniconf"
      #s(hash-table
         test equal
         data
         ("k" "key" "s" "section"))
      "Inko"
      #s(hash-table
         test equal
         data
         ("a" "attribute" "c" "constant" "m" "method" "o" "class" "r" "reopen" "t" "trait"))
      "JSON"
      #s(hash-table
         test equal
         data
         ("a" "array" "b" "boolean" "n" "number" "o" "object" "s" "string" "z" "null"))
      "Java"
      #s(hash-table
         test equal
         data
         ("a" "annotation" "c" "class" "e" "enumConstant" "f" "field" "g" "enum" "i" "interface" "l" "local" "m" "method" "p" "package"))
      "JavaProperties"
      #s(hash-table
         test equal
         data
         ("k" "key"))
      "JavaScript"
      #s(hash-table
         test equal
         data
         ("C" "constant" "G" "getter" "S" "setter" "c" "class" "f" "function" "g" "generator" "m" "method" "p" "property" "v" "variable"))
      "Julia"
      #s(hash-table
         test equal
         data
         ("c" "constant" "f" "function" "g" "field" "m" "macro" "n" "module" "s" "struct" "t" "type" "x" "unknown"))
      "Kconfig"
      #s(hash-table
         test equal
         data
         ("C" "choice" "M" "mainMenu" "c" "config" "k" "kconfig" "m" "menu"))
      "Kotlin"
      #s(hash-table
         test equal
         data
         ("C" "constant" "T" "typealias" "c" "class" "i" "interface" "m" "method" "o" "object" "p" "package" "v" "variable"))
      "LdScript"
      #s(hash-table
         test equal
         data
         ("S" "section" "i" "inputSection" "s" "symbol" "v" "version"))
      "Lisp"
      #s(hash-table
         test equal
         data
         ("c" "const" "f" "function" "m" "macro" "u" "unknown" "v" "variable"))
      "LiterateHaskell"
      #s(hash-table
         test equal
         data
         ("c" "constructor" "f" "function" "m" "module" "t" "type"))
      "Lua"
      #s(hash-table
         test equal
         data
         ("f" "function"))
      "M4"
      #s(hash-table
         test equal
         data
         ("I" "macrofile" "d" "macro"))
      "Make"
      #s(hash-table
         test equal
         data
         ("I" "makefile" "m" "macro" "t" "target"))
      "Man"
      #s(hash-table
         test equal
         data
         ("s" "section" "t" "title"))
      "Markdown"
      #s(hash-table
         test equal
         data
         ("S" "subsection" "T" "l4subsection" "c" "chapter" "s" "section" "t" "subsubsection" "u" "l5subsection"))
      "MatLab"
      #s(hash-table
         test equal
         data
         ("c" "class" "f" "function" "v" "variable"))
      "Maven2"
      #s(hash-table
         test equal
         data
         ("a" "artifactId" "g" "groupId" "p" "property" "r" "repositoryId"))
      "Meson"
      #s(hash-table
         test equal
         data
         ("B" "build" "P" "project" "S" "subdir" "V" "variable" "b" "benchmark" "c" "custom" "r" "run" "t" "test"))
      "Moose"
      #s(hash-table
         test equal
         data
         ("a" "attribute" "c" "class" "m" "method" "r" "role" "w" "wrapper"))
      "Myrddin"
      #s(hash-table
         test equal
         data
         ("c" "constant" "f" "function" "p" "pkg" "r" "trait" "t" "type" "v" "var"))
      "NSIS"
      #s(hash-table
         test equal
         data
         ("S" "sectionGroup" "d" "definition" "f" "function" "i" "script" "l" "langstr" "m" "macro" "p" "macroparam" "s" "section" "v" "variable"))
      "OCaml"
      #s(hash-table
         test equal
         data
         ("C" "Constructor" "M" "module" "c" "class" "e" "Exception" "f" "function" "m" "method" "p" "val" "r" "RecordField" "t" "type" "v" "var"))
      "ObjectiveC"
      #s(hash-table
         test equal
         data
         ("C" "category" "E" "field" "I" "implementation" "M" "macro" "P" "protocol" "c" "class" "e" "enum" "f" "function" "i" "interface" "m" "method" "p" "property" "s" "struct" "t" "typedef" "v" "var"))
      "OldC"
      #s(hash-table
         test equal
         data
         ("D" "macroparam" "L" "label" "c" "class" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member" "n" "namespace" "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable" "x" "externvar"))
      "OldC++"
      #s(hash-table
         test equal
         data
         ("D" "macroparam" "L" "label" "c" "class" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member" "n" "namespace" "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable" "x" "externvar"))
      "PHP"
      #s(hash-table
         test equal
         data
         ("a" "alias" "c" "class" "d" "define" "f" "function" "i" "interface" "l" "local" "n" "namespace" "t" "trait" "v" "variable"))
      "Pascal"
      #s(hash-table
         test equal
         data
         ("f" "function" "p" "procedure"))
      "Passwd"
      #s(hash-table
         test equal
         data
         ("u" "username"))
      "Perl"
      #s(hash-table
         test equal
         data
         ("M" "module" "c" "constant" "d" "subroutineDeclaration" "f" "format" "l" "label" "p" "package" "s" "subroutine"))
      "Perl6"
      #s(hash-table
         test equal
         data
         ("b" "submethod" "c" "class" "g" "grammar" "m" "method" "o" "module" "p" "package" "r" "role" "s" "subroutine" "t" "token" "u" "rule"))
      "PlistXML"
      #s(hash-table
         test equal
         data
         ("k" "key"))
      "Pod"
      #s(hash-table
         test equal
         data
         ("S" "subsection" "c" "chapter" "s" "section" "t" "subsubsection"))
      "PowerShell"
      #s(hash-table
         test equal
         data
         ("f" "function" "v" "variable"))
      "Protobuf"
      #s(hash-table
         test equal
         data
         ("D" "protodef" "G" "group" "e" "enumerator" "f" "field" "g" "enum" "m" "message" "o" "oneof" "p" "package" "r" "rpc" "s" "service"))
      "PuppetManifest"
      #s(hash-table
         test equal
         data
         ("c" "class" "d" "definition" "n" "node" "r" "resource" "v" "variable"))
      "Python"
      #s(hash-table
         test equal
         data
         ("I" "namespace" "c" "class" "f" "function" "i" "module" "l" "local" "m" "member" "v" "variable" "x" "unknown" "z" "parameter"))
      "PythonLoggingConfig"
      #s(hash-table
         test equal
         data
         ("L" "loggerSection" "q" "qualname"))
      "QemuHX"
      #s(hash-table
         test equal
         data
         ("i" "infoitem" "q" "qmp"))
      "QtMoc"
      #s(hash-table
         test equal
         data
         ("S" "signal" "p" "property" "s" "slot"))
      "R"
      #s(hash-table
         test equal
         data
         ("f" "function" "g" "globalVar" "l" "library" "s" "source" "v" "functionVar" "z" "parameter"))
      "R6Class"
      #s(hash-table
         test equal
         data
         ("a" "activeBindingFunc" "c" "class" "f" "field" "m" "method"))
      "REXX"
      #s(hash-table
         test equal
         data
         ("s" "subroutine"))
      "RSpec"
      #s(hash-table
         test equal
         data
         ("c" "context" "d" "describe"))
      "ReStructuredText"
      #s(hash-table
         test equal
         data
         ("C" "citation" "S" "subsection" "T" "target" "c" "chapter" "s" "section" "t" "subsubsection"))
      "RelaxNG"
      #s(hash-table
         test equal
         data
         ("a" "attribute" "e" "element" "n" "namedPattern"))
      "Robot"
      #s(hash-table
         test equal
         data
         ("k" "keyword" "t" "testcase" "v" "variable"))
      "RpmSpec"
      #s(hash-table
         test equal
         data
         ("g" "global" "m" "macro" "p" "patch" "t" "tag"))
      "Ruby"
      #s(hash-table
         test equal
         data
         ("A" "accessor" "C" "constant" "L" "library" "S" "singletonMethod" "a" "alias" "c" "class" "f" "method" "m" "module"))
      "Rust"
      #s(hash-table
         test equal
         data
         ("M" "macro" "P" "method" "c" "implementation" "e" "enumerator" "f" "function" "g" "enum" "i" "interface" "m" "field" "n" "module" "s" "struct" "t" "typedef" "v" "variable"))
      "S4Class"
      #s(hash-table
         test equal
         data
         ("c" "class" "g" "generic" "m" "method" "r" "repr"))
      "SCSS"
      #s(hash-table
         test equal
         data
         ("P" "placeholder" "c" "class" "f" "function" "i" "id" "m" "mixin" "v" "variable" "z" "parameter"))
      "SLang"
      #s(hash-table
         test equal
         data
         ("f" "function" "n" "namespace"))
      "SML"
      #s(hash-table
         test equal
         data
         ("c" "functor" "e" "exception" "f" "function" "r" "structure" "s" "signature" "t" "type" "v" "value"))
      "SQL"
      #s(hash-table
         test equal
         data
         ("D" "domain" "E" "field" "L" "label" "P" "package" "R" "service" "T" "trigger" "U" "publication" "V" "view" "c" "cursor" "d" "prototype" "e" "event" "f" "function" "i" "index" "l" "local" "n" "synonym" "p" "procedure" "r" "record" "s" "subtype" "t" "table" "v" "variable" "x" "mltable" "y" "mlconn" "z" "mlprop"))
      "SVG"
      #s(hash-table
         test equal
         data
         ("d" "def"))
      "Scheme"
      #s(hash-table
         test equal
         data
         ("f" "function" "s" "set"))
      "Sh"
      #s(hash-table
         test equal
         data
         ("a" "alias" "f" "function" "h" "heredoc" "s" "script"))
      "SystemTap"
      #s(hash-table
         test equal
         data
         ("f" "function" "m" "macro" "p" "probe" "v" "variable"))
      "SystemVerilog"
      #s(hash-table
         test equal
         data
         ("A" "assert" "C" "class" "E" "enum" "H" "checker" "I" "interface" "K" "package" "L" "clocking" "M" "modport" "N" "nettype" "O" "constraint" "P" "program" "Q" "prototype" "R" "property" "S" "struct" "T" "typedef" "V" "covergroup" "b" "block" "c" "constant" "e" "event" "f" "function" "i" "instance" "l" "ifclass" "m" "module" "n" "net" "p" "port" "q" "sequence" "r" "register" "t" "task" "w" "member"))
      "SystemdUnit"
      #s(hash-table
         test equal
         data
         ("u" "unit"))
      "TTCN"
      #s(hash-table
         test equal
         data
         ("C" "testcase" "G" "group" "M" "module" "P" "modulepar" "T" "timer" "a" "altstep" "c" "const" "d" "template" "e" "enum" "f" "function" "m" "member" "p" "port" "s" "signature" "t" "type" "v" "var"))
      "Tcl"
      #s(hash-table
         test equal
         data
         ("n" "namespace" "p" "procedure" "z" "parameter"))
      "TclOO"
      #s(hash-table
         test equal
         data
         ("c" "class" "m" "method"))
      "TeXBeamer"
      #s(hash-table
         test equal
         data
         ("f" "frametitle" "g" "framesubtitle"))
      "Tex"
      #s(hash-table
         test equal
         data
         ("B" "bibitem" "C" "command" "G" "subparagraph" "N" "counter" "P" "paragraph" "b" "subsubsection" "c" "chapter" "i" "xinput" "l" "label" "p" "part" "s" "section" "u" "subsection"))
      "Txt2tags"
      #s(hash-table
         test equal
         data
         ("s" "section"))
      "TypeScript"
      #s(hash-table
         test equal
         data
         ("C" "constant" "G" "generator" "a" "alias" "c" "class" "e" "enumerator" "f" "function" "g" "enum" "i" "interface" "l" "local" "m" "method" "n" "namespace" "p" "property" "v" "variable" "z" "parameter"))
      "VHDL"
      #s(hash-table
         test equal
         data
         ("A" "alias" "C" "component" "P" "package" "Q" "process" "T" "subtype" "a" "architecture" "c" "constant" "d" "prototype" "e" "entity" "f" "function" "g" "generic" "l" "local" "p" "procedure" "q" "port" "r" "record" "s" "signal" "t" "type" "v" "variable"))
      "Varlink"
      #s(hash-table
         test equal
         data
         ("E" "error" "I" "iparam" "O" "oparam" "d" "edesc" "e" "enumerator" "f" "field" "g" "enum" "i" "interface" "m" "method" "s" "struct"))
      "Vera"
      #s(hash-table
         test equal
         data
         ("D" "macroParameter" "P" "prototype" "T" "typedef" "c" "class" "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header" "i" "interface" "l" "local" "m" "member" "p" "program" "s" "signal" "t" "task" "v" "variable" "x" "externvar"))
      "Verilog"
      #s(hash-table
         test equal
         data
         ("b" "block" "c" "constant" "e" "event" "f" "function" "i" "instance" "m" "module" "n" "net" "p" "port" "r" "register" "t" "task"))
      "Vim"
      #s(hash-table
         test equal
         data
         ("C" "constant" "a" "augroup" "c" "command" "f" "function" "m" "map" "n" "filename" "v" "variable"))
      "WindRes"
      #s(hash-table
         test equal
         data
         ("a" "accelerators" "b" "bitmap" "c" "cursor" "d" "dialog" "f" "font" "i" "icon" "m" "menu" "v" "version"))
      "XML"
      #s(hash-table
         test equal
         data
         ("i" "id" "n" "nsprefix" "r" "root"))
      "XSLT"
      #s(hash-table
         test equal
         data
         ("m" "matchedTemplate" "n" "namedTemplate" "p" "parameter" "s" "stylesheet" "v" "variable"))
      "YACC"
      #s(hash-table
         test equal
         data
         ("l" "label"))
      "Yaml"
      #s(hash-table
         test equal
         data
         ("a" "anchor"))
      "YumRepo"
      #s(hash-table
         test equal
         data
         ("r" "repoid"))
      "Zephir"
      #s(hash-table
         test equal
         data
         ("a" "alias" "c" "class" "d" "define" "f" "function" "i" "interface" "l" "local" "n" "namespace" "t" "trait" "v" "variable"))))
  "Hash table of language -> single-letter kind -> full-length kind.
This is used for guessing the full-length kind (the
`ext-kind-full' field) when it's not presented, and
TAG_KIND_DESCRIPTION pseudo tags are not presented too.")

;; Run this snippet to generate
;; `citre-readtags--kind-name-single-to-full-table'.  The result will be shown
;; in a *Pp Eval Output* buffer, and it can be directly copied into the
;; variable definition.  Make sure to indent them!

;; (let* ((ctags-program (or citre-ctags-program "ctags"))
;;        (output (shell-command-to-string
;;                 (format "%s --quiet --options=NONE --machinable --list-kinds-full"
;;                         ctags-program)))
;;        (output-lines (nthcdr 1 (split-string output "\n" t)))
;;        (output-records (mapcar (lambda (line)
;;                                  (cl-subseq (split-string line "\t" t) 0 3))
;;                                output-lines))
;;        (table (make-hash-table :test #'equal)))
;;   (dolist (record output-records)
;;     (let ((lang (car record))
;;           (kind (nth 1 record))
;;           (kind-full (nth 2 record)))
;;       (unless (gethash lang table)
;;         (puthash lang (make-hash-table :test #'equal) table))
;;       (puthash kind kind-full (gethash lang table))))
;;   (pp-eval-expression table)
;;   (pop-to-buffer "*Pp Eval Output*")
;;   (goto-char (point-min))
;;   (ignore-errors
;;     (while (re-search-forward "#s(hash-table.*data")
;;       (replace-match "\n#s(hash-table\ntest equal\ndata")))
;;   (goto-char (point-min))
;;   (delete-char 1)
;;   (indent-region (point-min) (point-max)))

;;;; full-length kind -> single-letter kind

(defvar citre-readtags--kind-name-full-to-single-table
  #s(hash-table
     test equal
     data
     ("assembly"
      ("a")
      "part"
      ("p")
      "step"
      ("s")
      "section"
      ("s")
      "entryspec"
      ("E")
      "taskspec"
      ("K")
      "protectspec"
      ("O")
      "packspec"
      ("P")
      "subprogspec"
      ("R")
      "separate"
      ("S")
      "typespec"
      ("T")
      "subspec"
      ("U")
      "varspec"
      ("V")
      "autovar"
      ("a")
      "label"
      ("l")
      "component"
      ("C")
      "entry"
      ("e")
      "formal"
      ("f")
      "identifier"
      ("i")
      "task"
      ("t")
      "literal"
      ("l")
      "constant"
      ("C")
      "protected"
      ("o")
      "package"
      ("P")
      "subprogram"
      ("r")
      "type"
      ("t")
      "subtype"
      ("T")
      "variable"
      ("v")
      "exception"
      ("e")
      "anon"
      ("y")
      "play"
      ("p")
      "property"
      ("p")
      "antfile"
      ("i")
      "project"
      ("P")
      "target"
      ("T")
      "subsection"
      ("u")
      "l4subsection"
      ("T")
      "anchor"
      ("a")
      "chapter"
      ("c")
      "subsubsection"
      ("b")
      "l5subsection"
      ("u")
      "define"
      ("d")
      "macro"
      ("d")
      "class"
      ("c")
      "function"
      ("f")
      "subroutine"
      ("s")
      "script"
      ("s")
      "func"
      ("f")
      "global"
      ("g")
      "local"
      ("l")
      "region"
      ("r")
      "condition"
      ("c")
      "definition"
      ("d")
      "optenable"
      ("e")
      "subst"
      ("s")
      "template"
      ("d")
      "optwith"
      ("w")
      "data"
      ("d")
      "library"
      ("L")
      "man"
      ("M")
      "program"
      ("p")
      "ltlibrary"
      ("T")
      "directory"
      ("d")
      "subdir"
      ("S")
      "fragment"
      ("f")
      "pattern"
      ("p")
      "slot"
      ("s")
      "virtual"
      ("v")
      "enum"
      ("g")
      "booklet"
      ("B")
      "incollection"
      ("I")
      "mastersthesis"
      ("M")
      "proceedings"
      ("P")
      "article"
      ("a")
      "book"
      ("b")
      "conference"
      ("c")
      "inbook"
      ("i")
      "inproceedings"
      ("j")
      "manual"
      ("m")
      "misc"
      ("n")
      "phdthesis"
      ("p")
      "string"
      ("s")
      "techreport"
      ("t")
      "unpublished"
      ("u")
      "macroparam"
      ("D")
      "enumerator"
      ("e")
      "header"
      ("h")
      "member"
      ("m")
      "prototype"
      ("P")
      "struct"
      ("s")
      "typedef"
      ("T")
      "union"
      ("u")
      "externvar"
      ("x")
      "parameter"
      ("p")
      "event"
      ("e")
      "field"
      ("f")
      "interface"
      ("i")
      "method"
      ("m")
      "namespace"
      ("n")
      "alias"
      ("a")
      "name"
      ("N")
      "using"
      ("U")
      "tparam"
      ("Z")
      "option"
      ("D")
      "id"
      ("i")
      "selector"
      ("s")
      "division"
      ("D")
      "sourcefile"
      ("S")
      "fd"
      ("f")
      "group"
      ("G")
      "paragraph"
      ("P")
      "kind"
      ("k")
      "langdef"
      ("l")
      "module"
      ("m")
      "version"
      ("v")
      "mixin"
      ("m")
      "arg"
      ("a")
      "node"
      ("n")
      "signal"
      ("s")
      "entity"
      ("e")
      "attribute"
      ("a")
      "element"
      ("e")
      "notation"
      ("n")
      "parameterEntity"
      ("p")
      "phandler"
      ("p")
      "deletedFile"
      ("d")
      "hunk"
      ("h")
      "modifiedFile"
      ("m")
      "newFile"
      ("n")
      "feature"
      ("f")
      "callback"
      ("c")
      "delegate"
      ("d")
      "guard"
      ("g")
      "implementation"
      ("c")
      "operator"
      ("o")
      "protocol"
      ("P")
      "record"
      ("r")
      "test"
      ("t")
      "constructor"
      ("c")
      "port"
      ("p")
      "custom"
      ("c")
      "derivedMode"
      ("D")
      "face"
      ("H")
      "minorMode"
      ("M")
      "theme"
      ("T")
      "varalias"
      ("V")
      "const"
      ("c")
      "error"
      ("E")
      "inline"
      ("i")
      "unknown"
      ("x")
      "import"
      ("I")
      "localvar"
      ("l")
      "mxtag"
      ("x")
      "submodule"
      ("S")
      "blockData"
      ("b")
      "common"
      ("C")
      "namelist"
      ("n")
      "document"
      ("D")
      "localVariable"
      ("l")
      "toplevelVariable"
      ("t")
      "handler"
      ("h")
      "anonMember"
      ("M")
      "packageName"
      ("P")
      "receiver"
      ("R")
      "talias"
      ("a")
      "methodSpec"
      ("n")
      "var"
      ("v")
      "stylesheet"
      ("s")
      "heading1"
      ("h")
      "heading2"
      ("i")
      "heading3"
      ("j")
      "procedure"
      ("p")
      "key"
      ("k")
      "reopen"
      ("r")
      "trait"
      ("t")
      "array"
      ("a")
      "boolean"
      ("b")
      "number"
      ("n")
      "object"
      ("o")
      "null"
      ("z")
      "annotation"
      ("a")
      "enumConstant"
      ("e")
      "getter"
      ("G")
      "setter"
      ("S")
      "generator"
      ("G")
      "choice"
      ("C")
      "mainMenu"
      ("M")
      "config"
      ("c")
      "kconfig"
      ("k")
      "menu"
      ("m")
      "typealias"
      ("T")
      "inputSection"
      ("i")
      "symbol"
      ("s")
      "macrofile"
      ("I")
      "makefile"
      ("I")
      "title"
      ("t")
      "artifactId"
      ("a")
      "groupId"
      ("g")
      "repositoryId"
      ("r")
      "build"
      ("B")
      "benchmark"
      ("b")
      "run"
      ("r")
      "role"
      ("r")
      "wrapper"
      ("w")
      "pkg"
      ("p")
      "sectionGroup"
      ("S")
      "langstr"
      ("l")
      "Constructor"
      ("C")
      "Exception"
      ("e")
      "val"
      ("p")
      "RecordField"
      ("r")
      "category"
      ("C")
      "username"
      ("u")
      "subroutineDeclaration"
      ("d")
      "format"
      ("f")
      "submethod"
      ("b")
      "grammar"
      ("g")
      "token"
      ("t")
      "rule"
      ("u")
      "protodef"
      ("D")
      "message"
      ("m")
      "oneof"
      ("o")
      "rpc"
      ("r")
      "service"
      ("R")
      "resource"
      ("r")
      "loggerSection"
      ("L")
      "qualname"
      ("q")
      "infoitem"
      ("i")
      "qmp"
      ("q")
      "globalVar"
      ("g")
      "source"
      ("s")
      "functionVar"
      ("v")
      "activeBindingFunc"
      ("a")
      "context"
      ("c")
      "describe"
      ("d")
      "citation"
      ("C")
      "namedPattern"
      ("n")
      "keyword"
      ("k")
      "testcase"
      ("C")
      "patch"
      ("p")
      "tag"
      ("t")
      "accessor"
      ("A")
      "singletonMethod"
      ("S")
      "generic"
      ("g")
      "repr"
      ("r")
      "placeholder"
      ("P")
      "functor"
      ("c")
      "structure"
      ("r")
      "signature"
      ("s")
      "value"
      ("v")
      "domain"
      ("D")
      "trigger"
      ("T")
      "publication"
      ("U")
      "view"
      ("V")
      "cursor"
      ("c")
      "index"
      ("i")
      "synonym"
      ("n")
      "table"
      ("t")
      "mltable"
      ("x")
      "mlconn"
      ("y")
      "mlprop"
      ("z")
      "def"
      ("d")
      "set"
      ("s")
      "heredoc"
      ("h")
      "probe"
      ("p")
      "assert"
      ("A")
      "checker"
      ("H")
      "clocking"
      ("L")
      "modport"
      ("M")
      "nettype"
      ("N")
      "constraint"
      ("O")
      "covergroup"
      ("V")
      "block"
      ("b")
      "instance"
      ("i")
      "ifclass"
      ("l")
      "net"
      ("n")
      "sequence"
      ("q")
      "register"
      ("r")
      "unit"
      ("u")
      "modulepar"
      ("P")
      "timer"
      ("T")
      "altstep"
      ("a")
      "frametitle"
      ("f")
      "framesubtitle"
      ("g")
      "bibitem"
      ("B")
      "command"
      ("c")
      "subparagraph"
      ("G")
      "counter"
      ("N")
      "xinput"
      ("i")
      "process"
      ("Q")
      "architecture"
      ("a")
      "iparam"
      ("I")
      "oparam"
      ("O")
      "edesc"
      ("d")
      "macroParameter"
      ("D")
      "augroup"
      ("a")
      "map"
      ("m")
      "filename"
      ("n")
      "accelerators"
      ("a")
      "bitmap"
      ("b")
      "dialog"
      ("d")
      "font"
      ("f")
      "icon"
      ("i")
      "nsprefix"
      ("n")
      "root"
      ("r")
      "matchedTemplate"
      ("m")
      "namedTemplate"
      ("n")
      "repoid"
      ("r")
      "file"
      ("F")))
  "Hash table of full-length kind -> single-letter kind.
This is used for filtering the `kind' field of tags with
single-letter kind by full-length kind.  See
`citre-readtags-filter-kind'.")

;; NOTE: For now there's only one single-letter kind for a full-length kind,
;; which is good, but we still use a "string -> list of strings" structure in
;; the above table, in case that some day, the same (full-length) kind in 2
;; languages have different single-letter versions.

;; Run this snippet to generate
;; `citre-readtags--kind-name-full-to-single-table'.  The result will be shown
;; in a *Pp Eval Output* buffer, and it can be directly copied into the
;; variable definition.  Make sure to indent them!

;; (let* ((ctags-program (or citre-ctags-program "ctags"))
;;        (output (shell-command-to-string
;;                 (format "%s --quiet --options=NONE --machinable --list-kinds-full"
;;                         ctags-program)))
;;        (output-lines (nthcdr 1 (split-string output "\n" t)))
;;        (output-records (mapcar (lambda (line)
;;                                  (cl-subseq (split-string line "\t" t) 0 3))
;;                                output-lines))
;;        (table (make-hash-table :test #'equal)))
;;   (dolist (record output-records)
;;     (let ((kind (nth 1 record))
;;           (kind-full (nth 2 record)))
;;       (if (gethash kind-full table)
;;           (push kind (gethash kind-full table)))
;;       (puthash kind-full (list kind) table)))
;;   ;; "file" kind is not language-specific.  It's used for file tags and is
;;   ;; preserved by ctags.
;;   (puthash "file" '("F") table)
;;   (pp-eval-expression table)
;;   (pop-to-buffer "*Pp Eval Output*")
;;   (goto-char (point-min))
;;   (re-search-forward "#s(hash-table.*data")
;;   (replace-match "#s(hash-table\ntest equal\ndata")
;;   (indent-region (point-min) (point-max))
;;   (goto-char (point-min)))

(provide 'citre-readtags-tables)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-readtags-tables.el ends here
