# Citre

Citre means Ctags IDE on the true editor. It aims to be the state-of-art coding
solution based on Universal Ctags, that make benefits from all the advanced
features of Universal Ctags that's long overlooked in the Emacs world.

Citre Map is a component of Citre. It gives you a map in the maze of code
reading, that's aimed to support long code reading sessions of several hours or
days.

Currently this README only talks about Citre Map.

## Installation

Clone this repository to a directory, add that directory to your `load-path`,
then put this in your `init.el`:

``` emacs-lisp
(require 'citre)
```

Citre offers you a minor mode called `citre-mode`. Enable it to have auto
completion, jump to definition, and peek definition functionalities.

Most of the functionalities of Citre Map don't rely on `citre-mode` to be
enabled, but when you add a symbol to a code map, enabling `citre-mode` is
needed to find definitions of it.

To use Citre Map, put this in your `init.el`:

``` emacs-lisp
(require 'citre-map)
```

## Concepts

### Code map

Code map is a tool offerd by Citre Map. It's literally a map of a project, you
can go to places using it. It consists of 3 levels of lists:

```
 File list            Symbol list                        Definition list
+--------------+     +----------------------------+     +-------------------------+
| citre.el     |     | citre--code-map-alist ---------> | Line 96 in citre-map.el |
| citre-map.el ----> | citre--print-value-to-file |     | ...                     |
| ...          |     | ...                        |     |                         |
+--------------+     +----------------------------+     +-------------------------+
```

You add items to a code map by adding a symbol to it, then the file it belongs
to goes into file list, and its definition locations go into definition list.

You can browse the code map, add/delete/hide things in it, and save/read a code
map in the disk.

## Usage

### Setup your project

When enabling citre-mode, it automatically detects the root of your project by
the VCS it uses, or denoter files like "makefile" in its root. Use
`citre-show-project-root` to know if Citre did it right. If not, set
`citre-project-root` in your `.dir-locals.el` manually.

If you have a `tags` file in the project root, Citre will use it. Set
`citre-tags-file` to use a different tags file name. Notice that certain fields
in the `tags` file are needed to ensure Citre works properly. Here's a minimal
command to generate such a `tags` file:

``` console
$ cd /path/to/project/root
$ ctags --fields=Kzn -R
```

### Add a symbol to the code map

Enable `citre-mode`, put your cursor on a symbol, then use
`citre-see-symbol-in-code-map` to add it to the code map.

### Browse the code map

After you add a symbol, Citre takes you to the code map, and shows the
definition list of that symbol. You can also use `citre-see-file-in-code-map`,
then it shows the symbol list of current file. `citre-see-code-map` will open
the code map and restore the status when you leave it last time.

Type `f`/`RET` (`citre-code-map-forward`) or `b` (`citre-code-map-backward`) to
go forward or backward in the map. When in a definition list, type `f`/`RET` to
jump to the definition location.

Type `n`/`p` to go to the next/previous line.

### Modify the code map

You can type `m` (`citre-code-map-mark`) to mark/unmark any item in any list.
You can do this in batch by selecting the items (i.e. put them in an active
region) then type `m`. It will mark all the selected items, or unmark them if
they are already marked.

In a definition list, type `h` (`citre-code-map-hide`) to hide current item.
When there's an active region, it hides all items in the region; Or when there
are marked items, it hides them. Type `S` (`citre-code-map-show-all`) to show
the hidden items.

Here are the commands that will come in future:

- `citre-code-map-remove`: Remove items in a symbol list or file list.
- `citre-code-map-keep`: Keep marked items, and hide/remove the rest.
- `citre-code-map-refresh`: read definitions of a symbol, or of all symbols in a
  file. Use this when the code and tags file are updated. Notice that all the
  hidden items will be shown, since Citre can't tell which are the "updated
  version" of old tags.

### Save and load the code map

In the code map, use standard bindings of `save-buffer` (`C-x C-s` by default)
or `find file` (`C-x C-f` by default) to save or load the code map. You can
also call `citre-save-code-map` in any file in the project, or call
`citre-load-code-map` to do this.

Once you've saved the code map, or loaded it from the disk, then when exiting
Emacs, Citre will detect if the code map is modified since then, and ask if you
want to save it.
