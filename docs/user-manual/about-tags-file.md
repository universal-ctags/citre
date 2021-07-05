# About Tags File

## Tags file concepts

The tags file is a list of taglines. A tagline looks like:

```
citre-peek	citre-peek.el	/^(defun citre-peek (&optional buf point)$/;"	\
kind:function	line:1121...
```

It is some *fields* separated by tab characters. Each field records some info
about the tag entry, like:

- `name`: the identifier of the tag entry (`citre-peek` in this tagline).
- `input`: the file containing the tag (`citre-peek.el` in this tagline).
- `line`: the line number of the location of the tag (`line:1121` in this
  tagline).

The `kind` field is of particular interest. It records the kind of the tag
entry. For a class definition, it will be `kind:class`; For a function
definition, it will be `kind:function`, etc.

*Extra tags* are tags that are not normal definitions in source files. Some of
them are:

- `fileScope`: These are tags that have file scope, e.g., local variables.
- `inputFile`: These are tags of the file names that are tagged by ctags.
- `reference`: These are reference (the opposite of "definition") tags.

Citre works by:

1. Use readtags to extract needed taglines from a tags file.
2. Parse these taglines and extract info from their fields.
3. Present these info in an appropriate way.

## Create informative tags file

Using command line arguments, we can tweak which kinds, fields and extra tag
types to include in a tags file. For example, we can enable all of them:

```console
$ ctags --languages=c,c++,... --kinds-all='*' --fields='*' --fields-all='*' \
> --extras='*' --extras-all='*' -R
```

But this may be too much. The recommended command line in
[README](../../README.md) is:

```console
$ ctags --languages=c,c++,... --kinds-all='*' --fields='*' --extras='*' -R
```

This ensures all the info Citre could utilize is presented in the tags file.

If your project is big and you want to reduce the tags file size, here's how
you can create one without information that Citre doesn't make use of (yet):

```console
$ ctags --languages=c,c++,... --kinds-all='*' --fields=+KESflnt --extras=+fr -R
```

You can further reduce the size by not generating the search pattern, but then
you can't jump to a tag accurately when the file containing it is edited:

```console
$ ctags --languages=c,c++,... -n --kinds-all='*' --fields=+KESfnt \
> --extras=+fr -R
```

To learn about the meaning of the flags, run:

```console
$ ctags --list-kinds-full
$ ctags --list-fields
$ ctags --list-extras
```

You can also tweak the flags to suit the need of your project, but the
recommended ones should be enough for most projects.

## Create a command to run it

Once we use our customized command line to generate tags files, Citre can't
help you update them. Luckily we could easily create a Emacs command to help us
do that.

TBW

## How does Citre seek for a tags file for the current buffer?

TBW
