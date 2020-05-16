;;; citre-readtags-tables.el --- Lookup tables for citre-readtags -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 16 May 2020
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

;;;; For ext-lang field

(defvar citre-readtags--lang-extension-table
  #s(hash-table
     test equal
     data
     ("ada" "Ada" "adb" "Ada" "ads" "Ada"
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
      "html" "HTML" "htm" "HTML"
      "ini" "Iniconf" "conf" "Iniconf"
      "inko" "Inko"
      "itcl" "ITcl"
      "java" "Java"
      "properties" "JavaProperties"
      "js" "JavaScript" "jsx" "JavaScript" "mjs" "JavaScript"
      "json" "JSON"
      "lds" "LdScript" "ld" "LdScript" "ldi" "LdScript" "scr" "LdScript"
      "cl" "Lisp" "clisp" "Lisp" "lisp" "Lisp" "lsp" "Lisp" "l" "Lisp"
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
      "hs" "Haskell" "lhs" "Haskell"
      "jl" "Julia"
      "kt" "Kotlin" "kts" "Kotlin"
      "nim" "Nim"
      "nix" "Nix"
      "org" "Org"
      "scala" "Scala"
      "swift" "Swift"
      "vala" "Vala" "vapi" "Vala"))
  "Hash table of file extensions and the corresponding languages.
File extension (or the file name, if it doesn't have an
extension) are downcased first, then used as the key to lookup in
this table.")

;;;; For ext-kind-full field

(defvar citre-readtags--kind-name-table
  #s(hash-table
   test equal
   data
   ("Ada"
    #s(hash-table
       test equal
       data
       ("E" "entryspec" "K" "taskspec" "O" "protectspec" "P" "packspec"
        "R" "subprogspec" "S" "separate" "T" "typespec" "U" "subspec"
        "V" "varspec" "a" "autovar" "b" "label" "c" "component" "e" "entry"
        "f" "formal" "i" "identifier" "k" "task" "l" "literal" "n" "constant"
        "o" "protected" "p" "package" "r" "subprogram" "t" "type"
        "u" "subtype" "v" "variable" "x" "exception" "y" "anon"))
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
       ("S" "subsection" "T" "l4subsection" "a" "anchor" "c" "chapter"
        "s" "section" "t" "subsubsection" "u" "l5subsection"))
    "Asm"
    #s(hash-table
       test equal
       data
       ("d" "define" "l" "label" "m" "macro" "s" "section" "t" "type"))
    "Asp"
    #s(hash-table
       test equal
       data
       ("c" "class" "d" "constant" "f" "function" "s" "subroutine"
        "v" "variable"))
    "AutoIt"
    #s(hash-table
       test equal
       data
       ("S" "script" "f" "func" "g" "global" "l" "local" "r" "region"))
    "Autoconf"
    #s(hash-table
       test equal
       data
       ("c" "condition" "d" "definition" "e" "optenable" "m" "macro"
        "p" "package" "s" "subst" "t" "template" "w" "optwith"))
    "Automake"
    #s(hash-table
       test equal
       data
       ("D" "data" "L" "library" "M" "man" "P" "program" "S" "script"
        "T" "ltlibrary" "c" "condition" "d" "directory"))
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
       ("c" "constant" "f" "function" "g" "enum" "l" "label" "t" "type"
        "v" "variable"))
    "BibTeX"
    #s(hash-table
       test equal
       data
       ("B" "booklet" "I" "incollection" "M" "mastersthesis"
        "P" "proceedings" "a" "article" "b" "book" "c" "conference"
        "i" "inbook" "j" "inproceedings" "m" "manual" "n" "misc"
        "p" "phdthesis" "s" "string" "t" "techreport" "u" "unpublished"))
    "C"
    #s(hash-table
       test equal
       data
       ("D" "macroparam" "L" "label" "d" "macro" "e" "enumerator"
        "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member"
        "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable"
        "x" "externvar" "z" "parameter"))
    "C#"
    #s(hash-table
       test equal
       data
       ("E" "event" "c" "class" "d" "macro" "e" "enumerator" "f" "field"
        "g" "enum" "i" "interface" "l" "local" "m" "method" "n" "namespace"
        "p" "property" "s" "struct" "t" "typedef"))
    "C++"
    #s(hash-table
       test equal
       data
       ("A" "alias" "D" "macroparam" "L" "label" "N" "name" "U" "using"
        "Z" "tparam" "c" "class" "d" "macro" "e" "enumerator" "f" "function"
        "g" "enum" "h" "header" "l" "local" "m" "member" "n" "namespace"
        "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable"
        "x" "externvar" "z" "parameter"))
    "CMake"
    #s(hash-table
       test equal
       data
       ("D" "option" "f" "function" "m" "macro" "p" "project" "t" "target"
        "v" "variable"))
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
       ("D" "macroparam" "L" "label" "d" "macro" "e" "enumerator"
        "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member"
        "p" "prototype" "s" "struct" "t" "typedef" "u" "union" "v" "variable"
        "x" "externvar" "z" "parameter"))
    "Clojure"
    #s(hash-table
       test equal
       data
       ("f" "function" "n" "namespace"))
    "Cobol"
    #s(hash-table
       test equal
       data
       ("D" "division" "P" "program" "S" "sourcefile" "d" "data" "f" "fd"
        "g" "group" "p" "paragraph" "s" "section"))
    "Ctags"
    #s(hash-table
       test equal
       data
       ("k" "kind" "l" "langdef"))
    "D"
    #s(hash-table
       test equal
       data
       ("M" "module" "T" "template" "V" "version" "X" "mixin" "a" "alias"
        "c" "class" "e" "enumerator" "f" "function" "g" "enum"
        "i" "interface" "l" "local" "m" "member" "n" "namespace"
        "p" "prototype" "s" "struct" "u" "union" "v" "variable"
        "x" "externvar"))
    "DBusIntrospect"
    #s(hash-table
       test equal
       data
       ("a" "arg" "i" "interface" "m" "method" "n" "node" "p" "property"
        "s" "signal"))
    "DTD"
    #s(hash-table
       test equal
       data
       ("E" "entity" "a" "attribute" "e" "element" "n" "notation"
        "p" "parameterEntity"))
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
       ("a" "macro" "c" "callback" "d" "delegate" "e" "exception"
        "f" "function" "g" "guard" "i" "implementation" "m" "module"
        "o" "operator" "p" "protocol" "r" "record" "t" "test" "y" "type"))
    "Elm"
    #s(hash-table
       test equal
       data
       ("a" "alias" "c" "constructor" "f" "function" "m" "module"
        "n" "namespace" "p" "port" "t" "type"))
    "EmacsLisp"
    #s(hash-table
       test equal
       data
       ("C" "custom" "D" "derivedMode" "G" "group" "H" "face" "M" "minorMode"
        "T" "theme" "V" "varalias" "a" "alias" "c" "const" "e" "error"
        "f" "function" "i" "inline" "m" "macro" "s" "subst" "u" "unknown"
        "v" "variable"))
    "Erlang"
    #s(hash-table
       test equal
       data
       ("d" "macro" "f" "function" "m" "module" "r" "record" "t" "type"))
    "Falcon"
    #s(hash-table
       test equal
       data
       ("c" "class" "f" "function" "i" "namespace" "m" "member"
        "v" "variable"))
    "Flex"
    #s(hash-table
       test equal
       data
       ("C" "constant" "I" "import" "P" "package" "c" "class" "f" "function"
        "i" "interface" "l" "localvar" "m" "method" "p" "property"
        "v" "variable" "x" "mxtag"))
    "Fortran"
    #s(hash-table
       test equal
       data
       ("E" "enum" "L" "local" "M" "method" "N" "enumerator" "P" "prototype"
        "S" "submodule" "b" "blockData" "c" "common" "e" "entry"
        "f" "function" "i" "interface" "k" "component" "l" "label"
        "m" "module" "n" "namelist" "p" "program" "s" "subroutine" "t" "type"
        "v" "variable"))
    "Fypp"
    #s(hash-table
       test equal
       data
       ("m" "macro"))
    "Gdbinit"
    #s(hash-table
       test equal
       data
       ("D" "document" "d" "definition" "l" "localVariable"
        "t" "toplevelVariable"))
    "Glade"
    #s(hash-table
       test equal
       data
       ("c" "class" "h" "handler"))
    "Go"
    #s(hash-table
       test equal
       data
       ("M" "anonMember" "P" "packageName" "R" "receiver" "a" "talias"
        "c" "const" "f" "func" "i" "interface" "m" "member" "n" "methodSpec"
        "p" "package" "s" "struct" "t" "type" "u" "unknown" "v" "var"))
    "HTML"
    #s(hash-table
       test equal
       data
       ("C" "stylesheet" "I" "id" "J" "script" "a" "anchor" "c" "class"
        "h" "heading1" "i" "heading2" "j" "heading3"))
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
       ("a" "attribute" "c" "constant" "m" "method" "o" "object" "r" "reopen"
        "t" "trait"))
    "JSON"
    #s(hash-table
       test equal
       data
       ("a" "array" "b" "boolean" "n" "number" "o" "object" "s" "string"
        "z" "null"))
    "Java"
    #s(hash-table
       test equal
       data
       ("a" "annotation" "c" "class" "e" "enumConstant" "f" "field"
        "g" "enum" "i" "interface" "l" "local" "m" "method" "p" "package"))
    "JavaProperties"
    #s(hash-table
       test equal
       data
       ("k" "key"))
    "JavaScript"
    #s(hash-table
       test equal
       data
       ("C" "constant" "G" "getter" "S" "setter" "c" "class" "f" "function"
        "g" "generator" "m" "method" "p" "property" "v" "variable"))
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
       ("S" "subsection" "T" "l4subsection" "c" "chapter" "s" "section"
        "t" "subsubsection" "u" "l5subsection"))
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
    "Moose"
    #s(hash-table
       test equal
       data
       ("a" "attribute" "c" "class" "m" "method" "w" "wrapper"))
    "Myrddin"
    #s(hash-table
       test equal
       data
       ("c" "constant" "f" "function" "p" "pkg" "r" "trait" "t" "type"
        "v" "var"))
    "NSIS"
    #s(hash-table
       test equal
       data
       ("S" "sectionGroup" "d" "definition" "f" "function" "i" "script"
        "l" "langstr" "m" "macro" "p" "macroparam" "s" "section"
        "v" "variable"))
    "OCaml"
    #s(hash-table
       test equal
       data
       ("C" "Constructor" "M" "module" "c" "class" "e" "Exception"
        "f" "function" "m" "method" "p" "val" "r" "RecordField" "t" "type"
        "v" "var"))
    "ObjectiveC"
    #s(hash-table
       test equal
       data
       ("C" "category" "E" "field" "I" "implementation" "M" "macro"
        "P" "protocol" "c" "class" "e" "enum" "f" "function" "i" "interface"
        "m" "method" "p" "property" "s" "struct" "t" "typedef" "v" "var"))
    "OldC"
    #s(hash-table
       test equal
       data
       ("D" "macroparam" "L" "label" "c" "class" "d" "macro" "e" "enumerator"
        "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member"
        "n" "namespace" "p" "prototype" "s" "struct" "t" "typedef"
        "u" "union" "v" "variable" "x" "externvar"))
    "OldC++"
    #s(hash-table
       test equal
       data
       ("D" "macroparam" "L" "label" "c" "class" "d" "macro" "e" "enumerator"
        "f" "function" "g" "enum" "h" "header" "l" "local" "m" "member"
        "n" "namespace" "p" "prototype" "s" "struct" "t" "typedef"
        "u" "union" "v" "variable" "x" "externvar"))
    "PHP"
    #s(hash-table
       test equal
       data
       ("a" "alias" "c" "class" "d" "define" "f" "function" "i" "interface"
        "l" "local" "n" "namespace" "t" "trait" "v" "variable"))
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
       ("M" "module" "c" "constant" "d" "subroutineDeclaration" "f" "format"
        "l" "label" "p" "package" "s" "subroutine"))
    "Perl6"
    #s(hash-table
       test equal
       data
       ("b" "submethod" "c" "class" "g" "grammar" "m" "method" "o" "module"
        "p" "package" "r" "role" "s" "subroutine" "t" "token" "u" "rule"))
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
       ("D" "protodef" "G" "group" "e" "enumerator" "f" "field" "g" "enum"
        "m" "message" "o" "oneof" "p" "package" "r" "rpc" "s" "service"))
    "PuppetManifest"
    #s(hash-table
       test equal
       data
       ("c" "class" "d" "definition" "n" "node" "r" "resource" "v" "variable"))
    "Python"
    #s(hash-table
       test equal
       data
       ("I" "namespace" "c" "class" "f" "function" "i" "module" "l" "local"
        "m" "member" "v" "variable" "x" "unknown" "z" "parameter"))
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
       ("f" "function" "g" "globalVar" "l" "library" "s" "source"
        "v" "functionVar"))
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
       ("C" "citation" "S" "subsection" "T" "target" "c" "chapter"
        "s" "section" "t" "subsubsection"))
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
       ("A" "accessor" "C" "constant" "L" "library" "S" "singletonMethod"
        "a" "alias" "c" "class" "f" "method" "m" "module"))
    "Rust"
    #s(hash-table
       test equal
       data
       ("M" "macro" "P" "method" "c" "implementation" "e" "enumerator"
        "f" "function" "g" "enum" "i" "interface" "m" "field" "n" "module"
        "s" "struct" "t" "typedef" "v" "variable"))
    "SCSS"
    #s(hash-table
       test equal
       data
       ("P" "placeholder" "c" "class" "f" "function" "i" "id" "m" "mixin"
        "v" "variable" "z" "parameter"))
    "SLang"
    #s(hash-table
       test equal
       data
       ("f" "function" "n" "namespace"))
    "SML"
    #s(hash-table
       test equal
       data
       ("c" "functor" "e" "exception" "f" "function" "r" "structure"
        "s" "signature" "t" "type" "v" "value"))
    "SQL"
    #s(hash-table
       test equal
       data
       ("D" "domain" "E" "field" "L" "label" "P" "package" "R" "service"
        "T" "trigger" "U" "publication" "V" "view" "c" "cursor"
        "d" "prototype" "e" "event" "f" "function" "i" "index" "l" "local"
        "n" "synonym" "p" "procedure" "r" "record" "s" "subtype" "t" "table"
        "v" "variable" "x" "mltable" "y" "mlconn" "z" "mlprop"))
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
       ("A" "assert" "C" "class" "E" "enum" "I" "interface" "K" "package"
        "M" "modport" "P" "program" "Q" "prototype" "R" "property"
        "S" "struct" "T" "typedef" "V" "covergroup" "b" "block"
        "c" "constant" "e" "event" "f" "function" "m" "module" "n" "net"
        "p" "port" "r" "register" "t" "task"))
    "SystemdUnit"
    #s(hash-table
       test equal
       data
       ("u" "unit"))
    "TTCN"
    #s(hash-table
       test equal
       data
       ("C" "testcase" "G" "group" "M" "module" "P" "modulepar" "T" "timer"
        "a" "altstep" "c" "const" "d" "template" "e" "enum" "f" "function"
        "m" "member" "p" "port" "s" "signature" "t" "type" "v" "var"))
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
       ("B" "bibitem" "C" "command" "G" "subparagraph" "N" "counter"
        "P" "paragraph" "b" "subsubsection" "c" "chapter" "i" "xinput"
        "l" "label" "p" "part" "s" "section" "u" "subsection"))
    "TypeScript"
    #s(hash-table
       test equal
       data
       ("C" "constant" "G" "generator" "a" "alias" "c" "class"
        "e" "enumerator" "f" "function" "g" "enum" "i" "interface"
        "l" "local" "m" "method" "n" "namespace" "p" "property"
        "v" "variable" "z" "parameter"))
    "VHDL"
    #s(hash-table
       test equal
       data
       ("C" "component" "P" "package" "T" "subtype" "c" "constant"
        "d" "prototype" "e" "entity" "f" "function" "l" "local"
        "p" "procedure" "r" "record" "t" "type"))
    "Varlink"
    #s(hash-table
       test equal
       data
       ("E" "error" "I" "iparam" "O" "oparam" "d" "edesc" "e" "enumerator"
        "f" "field" "g" "enum" "i" "interface" "m" "method" "s" "struct"))
    "Vera"
    #s(hash-table
       test equal
       data
       ("D" "macroParameter" "P" "prototype" "T" "typedef" "c" "class"
        "d" "macro" "e" "enumerator" "f" "function" "g" "enum" "h" "header"
        "i" "interface" "l" "local" "m" "member" "p" "program" "s" "signal"
        "t" "task" "v" "variable" "x" "externvar"))
    "Verilog"
    #s(hash-table
       test equal
       data
       ("b" "block" "c" "constant" "e" "event" "f" "function" "m" "module"
        "n" "net" "p" "port" "r" "register" "t" "task"))
    "Vim"
    #s(hash-table
       test equal
       data
       ("a" "augroup" "c" "command" "f" "function" "m" "map" "n" "filename"
        "v" "variable"))
    "WindRes"
    #s(hash-table
       test equal
       data
       ("a" "accelerators" "b" "bitmap" "c" "cursor" "d" "dialog" "f" "font"
        "i" "icon" "m" "menu" "v" "version"))
    "XML"
    #s(hash-table
       test equal
       data
       ("i" "id" "n" "nsprefix" "r" "root"))
    "XSLT"
    #s(hash-table
       test equal
       data
       ("m" "matchedTemplate" "n" "namedTemplate" "p" "parameter"
        "s" "stylesheet" "v" "variable"))
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
       ("a" "alias" "c" "class" "d" "define" "f" "function" "i" "interface"
        "l" "local" "n" "namespace" "t" "trait" "v" "variable"))))
  "Hash table of language -> single-letter kind -> full-length kind.
This is used for guessing the full-length kind when it's not
presented, and TAG_KIND_DESCRIPTION pseudo tags are not presented
too.")

(declare-function citre-readtags--build-shell-command "citre-readtags")

(defun citre-readtags--generate-kind-name-table
    (&optional ctags-program)
  "Generate a kind name table from the help info of ctags.
This is used for updating `citre-readtags--kind-name-table'.  The
result will be shown in a *Pp Eval Output* buffer, and it can be
directly copied into the variable definition.

  CTAGS-PROGRAM is the name/path of the ctags program, nil means
\"ctags\"."
  (let* ((ctags-program (or ctags-program "ctags"))
         (output (shell-command-to-string
                  (citre-readtags--build-shell-command ctags-program
                                                       "--list-kinds-full")))
         (output-lines (nthcdr 1 (split-string output "\n" t)))
         (output-records (mapcar (lambda (line)
                                   (cl-subseq (split-string line " " t) 0 3))
                                 output-lines))
         (table (make-hash-table :test #'equal)))
    (dolist (record output-records)
      (let ((lang (car record))
            (kind (nth 1 record))
            (kind-full (nth 2 record)))
        (unless (gethash lang table)
          (puthash lang (make-hash-table :test #'equal) table))
        (puthash kind kind-full (gethash lang table))))
    (pp-eval-expression table)
    (pop-to-buffer "*Pp Eval Output*")
    (setq fill-column (- fill-column 2))
    (goto-char (point-min))
    (ignore-errors
      (while (re-search-forward "#s(hash-table.*data")
        (replace-match "\n#s(hash-table\ntest equal\ndata")))
    (goto-char (point-min))
    (delete-char 1)
    (indent-region (point-min) (point-max))
    (ignore-errors
      (while (re-search-forward "(\\(\"[a-zA-Z0-9]+\" *\\)+)")
        (save-excursion
          (backward-sexp)
          (forward-char)
          (while (ignore-errors (progn (forward-sexp) (forward-sexp) t))
            (when (> (current-column) fill-column)
              (save-excursion
                (backward-sexp) (backward-sexp)
                (insert-char ?\n)
                (funcall indent-line-function)))))))
    (whitespace-cleanup)
    (goto-char (point-min))))

(provide 'citre-readtags-tables)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre-readtags-tables.el ends here
