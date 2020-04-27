# The core layer

The core layer defines the way that upper components should use to get
information from tags files.

The basis of this layer is a readtags abstraction layer. Readtags is a
program for filtering, sorting, and printing tags from tags files. On
top of this, there are some wrappers around the APIs offered by the
readtags abstraction layer, which transforms human-friendly inputs to
readtags API calls.

## Concepts of readtags

The following is a brief introduction to the key concepts of readtags.
Run `$ readtags -h` to learn more.

The basic filtering and printing is done using **"actions"**. `-l`
action lists all regular tags:

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

**Post processors** can further process the lines given by actions.
**Filter** is one of the post processors. It uses scheme-style
expressions to specify the condition of sorting:

``` console
# Lists all tags start with "zero", and has the kind "macro".
$ readtags -Q '(eq? "macro" $kind)' -nep - zero
```

Learn more about filters from `$ readtags -H filter`.

**Sorter** is another post processor, for sorting the printed tags. It's
not utilized by Citre for now.

## The ambiguity of information offered by tags files

Tags file can offer *ambiguous information*. For example, when
generating the tags file with `$ ctags -R`, the file fields use relative
path to the current working directory. When jumping to definitions, how
do we know where do they actually point to? This shows the ambiguity of
information offered by tags files.

To ascertain such ambiguous information, we need *additional
information*. For example, the `TAG_PROC_CWD` pseudo tag tells us
exactly the needed current working directory. Combining it with the file
fields, we know the accurate paths.

## Readtags abstraction layer internals

`citre--readtags-get-lines` is the function that runs readtags. It's
arguments directly corresponds to the options and actions of the
readtags command. Read its docstring to know the details.

`citre--readtags-parse-line` operates on the result of
`citre--readtags-get-lines`. It transforms a line into a "record", which
is the data structure Citre uses to store information of a tag, and can
be utilized by `citre-get-field`.

Citre ensures the records store unambiguous information. Due to this
reason, `citre--readtags-parse-line` has a `tagsfile-info` argument,
where you should offer necessary additional information.

Let's have a look at how Citre handles those "additional information".
For each tags file, the additional information of it is stored in
`citre--tags-file-info-alist`. The function `citre--tags-file-info` is
the way to operate on it. It either returns the information required
from `citre--tags-file-info-alist`, or if it's not presented, gather the
information needed from pseudo tags, user input, etc., then write them
to `citre--tags-file-info-alist` and returns them.

For now, the only piece of additional information is the current working
directory mentioned above, and it is ensured by `citre--tags-file-info`
to only present when relative paths are indeed used in the tags file. In
the future, as upper tools want to do more and more fancy things, we may
use an extensible data structure for additional information, and let
`citre--tags-file-info` writes and returns only the required pieces in
it.

## Readtags APIs and their wrappers

The core API is `citre-readtags-get-records`. It's arguments directly
corresponds to the options and actions of the readtags program. What it
does is:

- Get the additional information needed with `citre--tags-file-info`.
- Run `citre--readtags-get-lines` to get the lines needed.
- Run `citre--readtags-parse-line`, with the additional information, on
  the result of `citre--readtags-get-lines`, to produce records.

The records generated can be utilized by `citre-get-field`, to extract
all kinds of information offered by them.

Another API is `citre-readtags-get-pseudo-tag`, which gets the value of
a specific pseudo tag in a tags file. This is mainly used by
`citre--tags-file-info`, but upper components may want to use it too.

`citre-readtags-get-records` has two problems:

- The arguments are natural for the readtags program, but not human
  friendly.
- Additional information may be needed for the filter expression. For
  example, I want to filter tags from `/home/me/citre/citre.el"`, but
  how do I know if the tags use relative or absolute paths? So I have to
  call `citre--tags-file-info` beforehand to get this piece of
  information, then I can build the right filter expression.

`citre-get-records` is a wrapper around `citre-readtags-get-records`, to
overcome these inconvenience. It transforms human-friendly arguments to
`citre-readtags-get-records` calls, and whenever the need emerges in the
future, it will also take care of getting the additional information
needed. In general, it's recommended for upper components to use
`citre-get-records` instead of `citre-readtags-get-records`.

## Appendix: How is the readtags command built?

`citre--build-shell-commands` is used to build the readtags command.
Each of its argument can be a string, symbol, or list of
strings/symbols/similar lists.

This function uses `%s` to format strings, and `%S` to format anything
else. Then, it runs `citre--disable-single-quote-as-terminator` on each
of the results, and wrap them with single quotes, then concatenate them
with spaces in between.

Most of this is straight forward: you use strings for each part of the
command, then `citre--build-shell-commands` concatenates them for you.
The interesting parts are the symbols and lists, let's call them "source
expressions", which is used specifically for building post processor
expressions, and let's call them "target expressions".

The rule is simple: For strings in target expressions, you use strings
in source expressions; for anything else in target expressions, you use
symbols in source expressions. This is also said in the docstring of
`citre--readtags-get-lines`. See this:

``` elisp
(setq source-expr
      `(eq? $name ,"citre"))

(insert (format "%S" source-expr))
=> inserts (eq\? $name "citre")
```

Double quotes are added around strings ("citre"), which is what we need
for target expressions. The `?` in `eq?` is escaped, but readtags know
this escape sequence, and will explain it as `eq?`.

The problem happens when `%S` produces escape sequences that readtags
doesn't know. From what we know for now, this happens only for `#t`,
`#f`, and `()`. For example:

```elisp
(insert (format "%S" '\#t))
=> inserts \#t
```

`\#` is not a valid escape sequence for readtags, and that causes
problems.

Readtags defines some aliases specifically for this need. Use the symbol
`true` for `#t`, `false` for `#f`, and `nil`/`()` for `()`.
