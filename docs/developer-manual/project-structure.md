# Project Structure

**WARNING:** The whole structure of Citre is under refactoring and this
document contains outdated information.

Citre is constructed by 3 layers:

1. *Core layer*: `citre-core.el`, `citre-core-tables.el`.

  This is an interface to the readtags program, offering APIs for upper
  components to query a tags file.

2. *Utils layer*: `citre-util.el`.

  This is the basis of all the user tools. It defines 2 common queries to a
  tags file:

  - What are the completions for an incomplete symbol (for auto-completion).
  - What are the definitions for a symbol (for finding definitions).

  It offers a general implementation for them, and a pluggable language support
  framework, so that how these queries are handled can be customized for a
  specific language. `citre-lang-*.el*` are the language support plugins.

  The utils layer also offers common functionalities like recognizing project,
  finding a tags file, generate a string for showing a tag, etc.

3. *Tools layer*: `citre-basic-tools.el`, `citre-peek.el`.

  These are the tools that the user directly uses.

This documentation explains how the core layer and language support framework
works.

## Core APIs

The core layer defines the APIs that upper components should use to get
information from tags files. Please have a look of the APIs before you go on.
You should:

- Open `citre-core.el`.
- Read the comments at the start of the `;;; Code` section to know how to see
  the outline of the source file.
- Open an outline view of the file. Read the `;;;; APIs` section.

The basic process of querying a tags file is:

- Build filter/sorter expressions to specify which tags are you interested in,
  and how you want to sort them.
- Get tags from a tags file, using these expressions.
- Get the needed fields from the tags.
- Optionally, you may want to find the location of a tag (to jump to there).

The core layer offers APIs for each of the above steps.

### Extension fields

Tags files can offer *ambiguous information*. For example, when generating the
tags file with `$ ctags -R`, the tags file records paths relative to the
current working directory (using the `input` field). When jumping to
definitions, how do we know which files they actually point to? This shows the
ambiguity of information offered by tags files.

For these "ambiguous fields", Citre offers ascertained versions of them called
*extension fields*. For example, `ext-abspath` offers the full path to the file
containing the tag, no matter the `input` field actually uses absolute or
relative paths.

See the docstring of `citre-core-get-tags` to know about all extension fields.

### Additional info

To ascertain such ambiguous fields, we need *additional information*. For
example, the `TAG_PROC_CWD` pseudo tag tells us the current working directory
when the tags file is generated. The `ext-abspath` field actually makes use of
it.

Upper components may want to use these additional info too. The API
`citre-core-tags-file-info ` is for this purpose. See
`citre-core--tags-file-info-alist` to know about the avaliable additional info.

### The main API

The one main API is `citre-core-get-tags`. It asks for:

- The action, case-sensitivity, filter/sorter expressions, etc. These
  are feed as actions and options to the readtags program.
- The fields required. You could ask it to get certain fields, or
  exclude certain fields, or throw an error when certain fields don't
  exist. Extension fields can also be used. This is for upper components
  to get the fields it needed. See the docstring for details.

What it does is:

- Get the additional information needed by the extension fields
  required.
- Run `citre-core--get-lines` to get the taglines needed.
- Run `citre-core--parse-line`, with the additional information, on the
  result of `citre-core--get-lines`, to get the *tags*.

The *"tags"* are just hash tables, and can be utilized by
`citre-get-tag-field`, to extract all kinds of information offered by them.

A wrapper `citre-tags-get-tags` is defined in `citre-util.el`. It's a handier
version of `citre-core-get-tags`. Tools offered by Citre should use
`citre-tags-get-tags` instead.

### Extra extension fields

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
  `citre-core-extra-ext-fields-table`) when certain procedure is frequently
  used to do this kind of calculation.

  Also, extra extension fields are not valid field arguments for
  `citre-core-get-records`.

### Concepts of readtags

Citre is build on readtags. Readtags is a program for filtering, sorting, and
printing tags from tags files.

The following is a brief introduction to the key concepts of readtags, for
those who are curious about how the low-level part of Citre works. Run `$
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

## Add support for a language

To tweak the behavior of Citre in a specific major mode (thus, a language), you
need to define functions/variables that tells Citre:

- How to get the symbol at point.

  The symbol is a string, and additional info could be stored in its text
  properties for the following function to use.

- How to compose filter/sorter expressions for auto-complete that symbol.

- How to compose filter/sorter expressions for finding definitions of that
  symbol.

These are plugged into `citre-language-support-alist` for them to work. See its
docstring for a detailed guide. See the `citre-lang-*.el*` files for reference
implementations.
