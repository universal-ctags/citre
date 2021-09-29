# How To Use `citre-global`

`citre-global` is a GNU Global plugin for Citre. With `citre-global`, you can
find the references of a symbol using xref or UI similar to `citre-jump` and
`citre-peek`.

## Prerequisite

You need to install GNU Global. In most distributions its package name is
"global", and should contain "gtags" and "global" program.

The built-in parser of GNU Global supports C, Yacc, Java, PHP4 and assembly. By
using the Pygments plugin parser, Global supports finding references of 150+
languages. To use the Pygments plugin parser, you need:

- Python (>=2.6. 3.x are also supported)
- Pygments. Check if it's installed properly by `$ python -m pygments -h`. This
  should print the help message of Pygments.
- Ctags. The documentation requires Exuberant Ctags, but Universal Ctags could
  do the work (and should be better).

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

This may not work on Windows, see `citre-gtags-args`.

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

## Configure `citre-global`

Requiring `citre-global` and you are good to go:

```elisp
(require 'citre-global)
```

Here's an `use-package` config example:

```elisp
(use-package citre-global
  :ensure nil
  :defer t
  :init
  (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek-references)
  (global-set-key (kbd "C-x c U") 'citre-global-update-database)
  (with-eval-after-load 'citre-peek
    (define-key citre-peek-keymap (kbd "M-l r")
      'citre-peek-through-references)))
```

This lazy-loads `citre-global`, meaning it will load `citre-global` only after
you use any command in it. But since `xref-find-references` is not a command
defined in `citre-global`, calling it won't load `citre-global`. One way to
deal with this is:

```elisp
(use-package citre-global
  :ensure nil
  :defer t
  :init
  (add-hook 'citre-mode-hook
            (defun require-citre-global ()
              (require 'citre-global)
              (remove-hook 'citre-mode-hook #'require-citre-global)))
  ;; ...the rest of the init block...
  )
```

This still lazy-loads `citre-global`, plus when `citre-mode` is called (to
enable the Citre xref backend), `citre-global` is loaded.

## Tagging the source tree

Open any file or directory in your project. Type `M-x
citre-global-update-database`. If no gtags database is avaliable, it will guide
you to create one using gtags, otherwise it will update the existing one, and
it's done incrementally.

You could also use `citre-global-create-database` to create the database.

## Use `citre-global`

`citre-global` offers `citre-jump-to-reference`, which reuses the `citre-jump`
UI.

`citre-global` also defines method to find references for the Citre xref
backend. So with `citre-mode` being enabled, you can use
`xref-find-references`.

`citre-global` offers commands that reuse the `citre-peek` UI:

- `citre-peek-references`, equivalent to `citre-peek`;
- `citre-ace-peek-references`, equivalent to `citre-ace-peek`;
- `citre-peek-through-references`, equivalent to `citre-peek-through`.

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
