# Use Global Backend

## Prerequisite

You need to install GNU Global. In most distributions its package name is
"global", and should contain "gtags" and "global" program.

The built-in parser of GNU Global supports C, Yacc, Java, PHP4 and assembly. By
using the Pygments plugin parser, Global supports finding definitions and
references of 150+ languages. To use the Pygments plugin parser, you need:

- Python (>=2.6. 3.x are also supported)
- Pygments. Check if it's installed properly by `$ python -m pygments -h`. This
  should print the help message of Pygments.
- Ctags. The GNU Global documentation says Exuberant Ctags is required, but
  Universal Ctags could do the work (and should be better).

For latest information, see `plugin-factory/PLUGIN_HOWTO.pygments` in the GNU
Global source tree.

### Configuration

By default, gtags creates database in the working directory. If you want to
save them using a global cache directory, do this:

```sh
# Set this to save gtags database to GTAGSOBJDIRPREFIX/<project-root>.  This
# requires --objdir option in gtags command line, see citre-gtags-args.
export GTAGSOBJDIRPREFIX=~/.cache/gtags/
```

Notice that `GTAGSOBJDIRPREFIX` *must exist* for `gtags` to use it. So you need
to run:

```console
$ mkdir -p ~/.cache/gtags/
```

This may not work on Windows, see the docstring of `citre-gtags-args`.

If you want to use the Pygments plugin parser, you need the following config:

```sh
# Make sure you use the path to the default config file on your machine.  This
# file contains the definition for Pygments plugin parser.
export GTAGSCONF=/usr/share/gtags/gtags.conf
export GTAGSLABEL=pygments
```

Put these in your preferred shell initialization file, like `~/.profile` or
`~/.bashrc`.

### Test

Let's move to an empty directory. Create `test.c` with the following content:

```c
#include <stdio.h>

static void f() {
  printf("Hello, world!\n");
}

int main() {
  f();
  return 0;
}
```

Let's generate the database with `--explain` option. If you are using Pygments
plugin parser, you should see output similar to:

```console
$ gtags --explain
 ...
 - File 'test.c' is handled as follows:
        ...
        parser:   |parser|
        library:  |/usr/lib/gtags/pygments-parser.so|
 ...
```

Let's see if we could find the definition of and references to `f`:

```console
$ global -x f
f                   3 test.c           static void f() {
$ global -xr f
f                   8 test.c             f();
```

If all works as expected, you now have an working GNU Global installation that
handles references.

## Tagging the source tree

Open any file or directory in your project. Type `M-x
citre-global-update-database`. If no gtags database is avaliable, it will guide
you to create one using gtags, otherwise it will update the existing one, and
it's done incrementally.

You could also use `citre-global-create-database`, or the `gtags` program
directly to create the database.

After generating a global database, you'll be able to use all Citre tools
through the global backend.

## Use global backend for Imenu

The default configuation of Citre may use tags backend for Imenu even if you
haven't created a tags file: If ctags program is found on the machine, then
when using Imenu in small files, Citre first scans the file using ctags, then
read the tags from the generated tags file. This should work better for most
situations.

If you have created a gtags database and want to use global backend, then one
of the following conditions needs to be satisfied:

- `global` comes before `tags` in `citre-tags-in-buffer-backends`
- `ctags` program is not installed
- `citre-tags-imenu-create-tags-file-threshold` is set to `nil`
- Current file is bigger than `citre-tags-imenu-create-tags-file-threshold`

Please note that since a gtags database doesn't contain kind information, all
the Imenu items are in the "<unclassified>" category, rather than in classified
as "variable", "function", etc..

## Tips for using global/gtags program

Though Citre handles this for you, you may still want to know how to use
global/gtags program in the command line, to understand how everything works,
and for debugging purpose.

The `gtags` program is used to tag the project in the working directory. Citre
uses the following options:

- `--compact`: to use compact format for the tags file.
- `--objdir`: to use the `GTAGSOBJDIRPREFIX` variable.

Once the tags file is created, you can use

```console
$ global --update
```

to update the tags file incrementally. This works in any directory under the
project root.

To find the reference of a symbol, Citre internally runs:

```console
$ global --color=never --encode-path=' :' --result=grep --literal --reference \
> --symbol --nearness=<current-file-or-dir> -- <symbol-name>
```

This also works in any directory under the project root. The meanings of some
of the arguments are:

- `--encode-path=' :'`: Escape spaces and colons in the path. This is for
  making the output machine-readable.
- `--result=grep`: Use grep output format.
- `--literal`: <symbol-name> is a literal string, not a regexp pattern.
- `--reference`: Find the references to <symbol-name>
- `--symbol`: Find the references even if <symbol-name> is not defined.
- `--nearness`: Sort the result by nearness. This means references in the
  current file appears at top, those in the current directory, parent
  directory, grandparent directory... follow them in order.

The following command prints the path containing the tags files:

```console
$ global --print-dbpath
```

The following command prints the project root path:

```console
$ global --print-dbpath --rootdir
```

## More tips on configuring GNU Global

You can copy the default config to your home directory and customize it, see
the manpage gtags.conf(5). You may want to customize the rule for ignoring
files when tagging, then you need to modify this part:

```conf
common:\
	:skip=HTML/,HTML.pub/,tags,...
```

If you don't have ctags in the standard path (`/usr/bin/ctags`), you need to
modify this part:

```conf
pygments-parser|Pygments plug-in parser:\
	:tc=common:\
	:ctagscom=/path/to/ctags:\
```

The default config also contains a label "native-pygments". This uses GNU
Global built-in parser for its supported languages, and use Pygments plugin
parser for other languages. To use this, set `GTAGSLABEL` to `native-pygments`.
