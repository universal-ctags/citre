# More About Tags Backend

## Where to save a tags file, and how Citre finds it

When Citre seeks a tags file for current file, it goes up directory hierarchy
from current file. In each directory it passes, it tries the following methods
to see if a tags file is assgined to it:

1. By `citre-tags-file`, which can be set as a directory/file local variable.

2. By `citre-tags-file-global-cache-dir`. This uses a global cache dir
   (`~/.cache/tags/` by default) so it won't pollute your project directory.

    For example, for `/project/root/some/dir/`, if
    `~/.cache/tags/project!root!some!dir.tags` exists, it's used as the tags
    file.

3. By `citre-tags-file-names`. If any file in it exists in the directory, it's
   used as the tags file for this directory.

   By default `citre-tags-files` contains `.tags` and `tags`, so for
   `/project/root/some/dir/`, if `/project/root/some/dir/.tags` exists, it's
   used.

If any one succeeded, the rest will not be tried.

When you create a tags file, Citre lets you to choose a location between "the
directory in which you use the tags file" (which is found by method 3 above)
and "the global cache dir" (which is method 2). To use method 1, manually
create the tags file and set `citre-tags-file` as a local variable.

## More on creating a tags file

When creating a tags file, Citre asks you to specify a directory in which you
want to use the tags file. It has the following effects:

- It's the working directory when running ctags. You can imagine that Citre
  `cd` to this directory, then run Ctags. Files in this directory is recorded
  using relative path in the tags file, which saves some space.
- When editing the options, you could use relative paths as paths to scan.
- When visiting the files in this directory, Citre will use that tags file.

Other than `citre-update-this-tags-file`, which is most commonly used, there
are more commands generating tags files:

- `citre-update-tags-file`: Select a tags file to update
- `citre-create-tags-file`: Create a new tags file.

You can also pass the name of a tagsfile as an argument to
`citre-update-tags-file`. Based on this, you could write scripts to update tags
files created using Citre.

## Tags file concepts

The command line can be tweaked to create tags file that suits your project
better. In order to do this, it's necessary to know some basic concepts of tags
file.

A tags file is a list of taglines. A tagline looks like:

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

But this may be too much. The default command line used by Citre is:

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

## Extend a parser

One of the unique strengths of Ctags is it's hackable, meaning you could extend
a parser using regular expressions to tag the symbols you want Ctags to tag.

A Citre user once asked me this question: in PHP, sometimes a function is
defined in an array:

`test.php`:

```php
return array(
  "test_func"=>function(string $str="test_func"){
    return $str;
  }
);
```

It can be called from other files:

```php
$test=require("./test.php");
echo $test["test_func"]("hello");
```

How to tag the function `test_func`?

The answer:

```console
$ cat options.ctags
--langdef=PHPext{base=PHP}
--kinddef-PHPext=a,arrayfunc,functions defined in arrays
--regex-PHPext=/"(.*)"=>function/\1/a/

$ ctags --options=./options.ctags --fields='*' -f - test.php
test_func	test.php	/^  "test_func"=>function(string $str="test_func"){$/;"	\
kind:arrayfunc  line:2  language:PHPext      roles:def       extras:subparser
```

Let's see how it works.

- `--langdef=PHPext{base=PHP}`

  We define a new language called PHPext, and use it as a subparser of PHP.

  When extending a parser, it's always encouraged to define a new language, so
  the kind you defined for it won't conflict with the base language.

- `--kinddef-PHPext=a,arrayfunc,functions defined in arrays`

  Define a kind for PHPext, with abbreviated name `a`, full name `arrayfunc`,
  which means "functions defined in arrays".

- `--regex-PHPext=/"(.*)"=>function/\1/a/`

  The grammar of this is `--regex-<LANG>=<PATTERN>/<NAME>/[<KIND>/]LONGFLAGS`.
  Let's see the fields in it:

  - `"(.*)"=>function`: The pattern to match arrayfunc. The function name is
    captured by a group.

  - `\1`: The tag name is the function name captured by the group.

  - `a`: The kind of the tag is `a`/`arrayfunc`.

Another real world example is in the Universal Ctags source tree,
`ctags/Tmain/nested-subparsers.d/event.ctags`:

```
--langdef=Event{base=C}
--kinddef-Event=e,event,events
--regex-Event=/DEFINE_EVENT\((.*)\);/\1/e/)
```

It introduces `event` kind. The point is when using `DEFINE_EVENT`, the
programmer thinks about defining an `event`, rather than what the macro is
eventually expanded into.

In DSL-heavy projects, Ctags plus some regular expressions could beat
intelligent tools like language servers.

See [Extending ctags with Regex parser
*(optlib)*](https://docs.ctags.io/en/latest/optlib.html) to learn more.
