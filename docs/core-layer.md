# The core layer

The core layer defines the way that upper components should use to get
information from tags files.

The basis of this layer is a readtags abstraction layer, implemented by
`citre-readtags.el`. Readtags is a program for filtering, sorting, and
printing tags from tags files. On top of this, `citre.el` implements
some wrappers around the APIs offered by `citre-readtags.el`, which
transforms human-friendly inputs to the API calls.

## Concepts of readtags

The following is a brief introduction to the key concepts of readtags.
Run `$ readtags -h` and see the manpage readtags(1) to learn more.

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

**Sorter** is another post processor, for sorting the printed tags. For
example, sort by the name of the tags:

``` console
$ readtags -S '(<> $name &name)' -ne -l
```

You can learn more about filters and sorters from `$readtags -H filter`,
`$readtags -H sorter` and readtags(1).

## The ambiguity of information offered by tags files

Tags file can offer *ambiguous information*. For example, when
generating the tags file with `$ ctags -R`, the input fields use
relative path to the current working directory. When jumping to
definitions, how do we know where do they actually point to? This shows
the ambiguity of information offered by tags files.

To ascertain such ambiguous information, we need *additional
information*. For example, the `TAG_PROC_CWD` pseudo tag tells us
exactly the needed current working directory. Combining it with the file
fields, we know the accurate paths.

Citre-readtags can gather the needed additional info from the tags file
itself, and uses `citre-readtags--tags-file-info-alist` to store them.
For now we have 2 fields of additional info:

- `path`: Whether the tags file uses relative paths, and the current
  working directory when generating the tags file.
- `kind`: Whether the tags file uses single-letter kinds, and how to
  convert single-letter kinds to full-length kinds

You can learn more from the docstring of
`citre-readtags--tags-file-info-alist`. We offer 2 functions for
accessing these additional info:

- `citre-readtags--get-tags-file-info`: Return the additional info of a
  tags file. It caches the results in
  `citre-readtags--tags-file-info-alist`, and only update them when the
  tags file itself is updated.
- `citre-readtags--tags-file-info`: Get a certain field from the return
  value of `citre-readtags--get-tags-file-info`, or from a value in
  `citre-readtags--tags-file-info-alist`.

Based on this, Citre-readtags offers "extension fields". For example,
through the `ext-abspath` field, you can always get absolute paths, no
matter the input fields actually use absolute or relative paths. See the
docstring of `citre-readtags-get-records` to know about all extension
fields. Keep the concept of extension fields in mind since we'll use it
frequently from now.

## Readtags abstraction layer internals

`citre-readtags--get-lines` is the function that runs readtags, and get
its output. It's arguments directly correspond to the options and
actions of the readtags command. Read its docstring to know the details.

`citre-readtags--parse-line` operates on the result of
`citre-readtags--get-lines`. It transforms a line into a "record", which
is the data structure Citre uses to store information of a tag, and can
be utilized by `citre-get-field`. If additional info is offered, it can
also write extension fields to the records.

## Readtags APIs and their wrappers

The core API is `citre-readtags-get-records`. It asks for:

- The action, case-sensitivity, filter/sorter expressions, etc. These
  are feed as actions and options to the readtags program.
- The fields required. You could ask it to get certain fields, or
  exclude certain fields, or throw an error when certain fields don't
  exist. Extension fields can also be used. This is for upper components
  to get the fields it needed. See the docstring for details.

What it does is:

- Get the additional information needed by the extension fields
  required.
- Run `citre--readtags-get-lines` to get the lines needed.
- Run `citre--readtags-parse-line`, with the additional information, on
  the result of `citre--readtags-get-lines`, to produce records.

The records generated can be utilized by `citre-readtags-get-field`, to
extract all kinds of information offered by them.

`citre-readtags-get-field` can accept another kind of extension fields,
called "extra extension fields" since I couldn't come up with a better
name. The difference between it and the extension fields are:

- Extension fields offer "ascertained version" of ambiguous fields, and
  are for this purpose only. Upper components should not extend them.
- Extra extension fields is just another way to calculate some info
  based on the records. Upper components could extend them (using
  `citre-readtags-extra-ext-fields-table`) when certain procedure is
  frequently used to do this kind of calculation. Also, they are not
  valid field arguments for `citre-readtags-get-records`.

Another API is `citre-readtags-get-pseudo-tags`, which gets the value of
a specific pseudo tag in a tags file. This is also used internally for
getting additional info of a tags file. Upper tools could use this to
write their own doctor/diagnostic tool, to inspect the tags file and see
if it meets their requirements. For this purpose,
`citre-readtags-get-records` could also be used. It has a `lines`
argument, making it possible to get one or more sample records to
analyse.

Handy helpers are offered to build filter and sorter expressions. See
`citre-readtags-build-filter`, `citre-readtags-filter-match-input`,
`citre-readtags-filter-match-kind` and `citre-readtags-build-sorter`.

(TBW: talk about the wrappers in `citre.el` when the upper components
are fixed.)

## Appendix: How is the readtags command built?

`citre-readtags--build-shell-command` is used to build the readtags
command. Each of its argument can be a string, symbol, or list of
strings/symbols/similar lists.

This function uses `%s` to format strings, and `%S` to format anything
else. Then, it runs `citre-readtags--escape-single-quote` on each of the
results, and wrap them with single quotes, then concatenate them with
spaces in between.

Most of this is straight forward: you use strings for each part of the
command, then `citre-readtags--build-shell-command` concatenates them
for you. The interesting parts are the symbols and lists, let's call
them "source expressions", which is used specifically for building
sorters and filters in the readtags command, and let's call them "target
expressions".

The rule is simple: For strings in target expressions, you use strings
in source expressions; for anything else in target expressions, you use
symbols in source expressions. This is also said in the docstring of
`citre-readtags-get-records`. See this:

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
doesn't know. From what we know for now, this happens for `#`, `(` and
`)`. For example:

```elisp
(insert (format "%S" '\#t))
=> inserts \#t
```

`\#` is not a valid escape sequence for readtags, and that causes
problems.

Readtags defines some aliases specifically for this need. Use the symbol
`true` for `#t`, `false` for `#f`, `nil`/`()` for `()`, and
`string->regexp` for `#/PATTERN/`. This is also said in the docstring of
`citre-readtags-get-records`.
