# About Tags File

## Where to save a tags file, and how Citre finds it

When Citre seeks a tags file for current file, it goes up directory hierarchy
from current file. In each directory it passes, it tries the following methods
to see if a tags file is assgined to it:

1. By `citre-tags-file-alist`. We'll not go through it here, but it's a
   powerful customizable option to assign any tags file to any directory. Read
   its docstring for details.

2. By `citre-tags-file-per-project-cache-dir`. When
   `citre-project-root-function` could detect your project root, this directory
   could store all the tags files in the project. By default it's `./.tags/`

   For example, We are seeking a tags file for the directory
   `/project/root/some/dir/`, then if `/project/root/.tags/some!dir.tags`
   exists, it's used as the tags file.

3. By `citre-tags-file-global-cache-dir`. This is similar to 2 but a global
   cache dir (`~/.cache/tags/` by default) is used so it won't pollute your
   project directory.

    For example, for `/project/root/some/dir/`, if
    `~/.cache/tags/project!root!some!dir.tags` exists, it's used as the tags
    file.

4. By `citre-tags-files`. If any file in it exists in the directory, it's used
   as the tags file for this directory.

   By default `citre-tags-files` contains `.tags` and `tags`, so for
   `/project/root/some/dir/`, if `/project/root/some/dir/.tags` exists, it's
   used.

These methods are in the precende order, so if a tags file is found by method
2, then 3 and 4 won't be tried.

When you create a tags file, Citre gives you 4 places to save, each corresponds
to a method above:

1. In the directory to use it: method 4
2. In the global cache dir: method 3
3. In the per-project cache dir: method 2
4. Anywhere: method 1

## More on creating a tags file

After you've decided where to save the tags file, Citre asks you to specify a
root dir. It has 2 uses:

- Ctags command runs in it, i.e., it's the current working directory when
  running ctags.
- In the command editing buffer, The path of added dir/files relative to the
  root dir are used, if they are inside the root dir.

2 more commands dealing with tags files are not mentioned in README:

- `citre-update-tags-file`: Select a tags file to update
- `citre-create-tags-file`: Create a new tags file.

You can also pass the name of a tagsfile as an argument to
`citre-update-tags-file`, and it will update it. Based on this, you could write
scripts to update tags files created using Citre.

## User options

These are some more options for tags file creating/updating:

- `citre-edit-cmd-buf-default-cmd`: Default command shown in the command
  editing buffer. You can customize this to use a command you prefer for most
  projects.
- `citre-edit-cmd-buf-map`: Keymap used in the command editing buffer.
- `citre-default-create-tags-file-location`: If you want to always save the
  tags file to global cache dir, or in the directory that uses it, etc., use
  this option.

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

In DSL-heavy projects, Ctags plus some regular expressions could beat
intelligent tools like language servers.

See [Extending ctags with Regex parser
*(optlib)*](https://docs.ctags.io/en/latest/optlib.html) to learn more.
