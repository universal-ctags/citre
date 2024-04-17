# Project Structure

## Backend interface

Citre tools and backends depend on a common interface, defined in
`citre-backend-interface.el`. A backend is a cl-struct that contains a bunch of
functions, for finding completions/definitions/references, etc. Read the
definition of `citre--backend` to know how this works and how to create your
own backend.

## Tags: the central data structure

A symbol in a certain location in the code base is represented by a "tag",
created using `citre-make-tag`. It contains information like the file, line
number, is it a function/variable, its type, etc. Citre backends produces tags,
and Citre tool uses tags, by jumping to them or show them to the user.

See the commentary section of `citre-common-tags.el` to know about the tag data
structure. It closely resembles a tag line in a tags file produced by Universal
Ctags.

## Tools

All Citre tools are defined in `citre.el`. The UI code of `citre-jump` and
`citre-peek` are extracted into `citre-ui-jump.el` and `citre-ui-peek.el`.

## Global backend, xref adapter and eglot backends

The global backend is defined in `citre-global.el`.

Xref adapter is defined in `citre-xref-adapter.el`. It also contains the eglot
backend as an example.

## Tags backend

The tags backend is defined in `citre-tags.el`. This is a sophisticated one.

First, it requires `citre-ctags.el`, which defines functions for finding the
tags file for a buffer, and offers tools to generate a tags file.

But of course, the more important functionality of `citre-tags.el` is getting
tags (for completions, definitions, imenu). The process is:

- Top level APIs like `citre-tags-get-definitions` is called.
- The language support framework is consulted, which tells us in a specific
  language, how to get the symbol at point, how to filter/sort the tags. Citre
  also has generic functions for these as a fallback.
- `citre-tags-get-tags` is called with the symbol and filters/sorters. It
  further finds the readtags program and the tags file for current buffer, then
  calls `citre-readtags-get-tags`, which returns the tags.

To work on the tags backend, you need to know a bit about the readtags program
and tags file format. These information are provided below.

### Readtags

Readtags is a program for filtering, sorting, and printing tags from tags
files. `citre-readtags.el` is an interface of it, which parses readtags output
to the "tags" data structure.

The following is a brief introduction to the key concepts of readtags. Run `$
readtags -h` and see the manpage readtags(1) to learn more.

The basic filtering and printing is done using *actions*. `-l` action lists all
regular tags:

``` console
$ readtags -l
```

NAME action lists regular tags matching NAME(s):

``` console
# Lists all tags start with "citre".
# -p means performing prefix match.
# -e and -n means print the whole line from tags files.
$ readtags -enp - citre
```

*Post processors* can further process the lines given by actions. *Filter* is
one of the post processors. It uses scheme-style expressions to specify the
condition of sorting:

``` console
# Lists all tags start with "zero", and has the kind "macro".
$ readtags -Q '(eq? "macro" $kind)' -nep - zero
```

*Sorter* is another post processor, for sorting the printed tags. For
example, sort by the name of the tags:

``` console
$ readtags -S '(<> $name &name)' -ne -l
```

You can learn more about filters and sorters from `$readtags -H filter`,
`$readtags -H sorter` and readtags(1).

### Language support framework

To tweak the behavior of Citre in a specific major mode (thus, a language), you
need to define functions/variables that tells Citre:

- How to get the symbol at point.

  The symbol is a string, and additional info could be stored in its text
  properties for the following function to use.

- How to compose filter/sorter expressions for auto-complete that symbol.

- How to compose filter/sorter expressions for finding definitions of that
  symbol.

These are plugged into `citre-tags-language-support-alist` for them to work.
See its docstring for a detailed guide. See the `citre-lang-*.el*` files for
reference implementations.

### More about tags files

It's assumed that you've read [More About Tags
Backend](../user-manual/more-about-tags-backend.md).

#### Extension fields

Tags files can offer *ambiguous information*. For example, when generating the
tags file with `$ ctags -R`, the tags file records paths relative to the
current working directory (using the `input` field). When jumping to
definitions, how do we know which files they actually point to? This shows the
ambiguity of information offered by tags files.

For these "ambiguous fields", Citre offers ascertained versions of them called
*extension fields*. For example, `ext-abspath` offers the full path to the file
containing the tag, no matter the `input` field actually uses absolute or
relative paths.

See the docstring of `citre-readtags-get-tags` to know about all extension
fields.

#### Additional info

To ascertain such ambiguous fields, we need *additional information*. For
example, the `TAG_PROC_CWD` pseudo tag tells us the current working directory
when the tags file is generated. The `ext-abspath` field actually makes use of
it.

The API `citre-readtags-tags-file-info` is for grabbing these additional
information. See `citre-readtags--tags-file-info-alist` to know about the
avaliable additional info.

#### Extra extension fields

`citre-get-tag-field` can accept another kind of extension fields,
called *extra extension fields* since I couldn't come up with a better
name. The difference between it and the extension fields are:

- Extension fields offer "ascertained version" of ambiguous fields, and
  are for this purpose only.

  Upper components should not define new extension fields.

- Extra extension fields is just another way to calculate some info based on
  the records. So, they are not recorded by the tags, but generated "on the
  fly" when calling `citre-get-tag-field`.

  Upper components could define more extra extension fields (using
  `citre-tag-extra-ext-fields-table`) when certain procedure is frequently
  used to do this kind of calculation.

  Also, extra extension fields are not valid field arguments for
  `citre-readtags-get-tags`.
