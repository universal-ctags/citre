# Citre

Citre means Ctags IDE on the true editor. It aims to be the state-of-art coding
solution based on Universal Ctags, that make benefits from all the advanced
features of Universal Ctags that's long overlooked in the Emacs world.

Citre Map is a component of Citre. It gives you a map in the maze of code
reading, which aims to support long code reading sessions of several hours or
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

You can browse the code map, add/delete/hide things in it, and save/load a code
map in the disk.

## Usage

### Setup your project

When enabling citre-mode, it automatically detects the root of your project by
the VCS it uses, or denoter files like ".citre" or "makefile" in its root. Use
`citre-show-project-root` to know if Citre did it right. If not, put a file
with one of the names in `citre-project-denoter-files` in your project root, or
set `citre-project-root` in your `.dir-locals.el` manually.

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

Type `o` (`citre-code-map-open-file`) to open current file in a file list.

### Modify the code map

#### Mark/select

You can type `m` (`citre-code-map-mark`) to mark/unmark any item in any list.
You can do this in batch by selecting the items (i.e. put them in an active
region) then type `m`. It will mark all the selected items, or unmark them if
they are already marked.

Type `M` (`citre-code-map-unmark-all`) to unmark all items.

In a code map, when there are an active region, the items in it are considered
to be selected; or when there are no active region, but some marks, the marked
items are considered to be selected. We'll use "selected items" to refer to
such items later.

#### Hide/show items in definition lists

In a definition list, type `h` (`citre-code-map-hide`) to hide selected items,
or current item if there are no selected items.

Type `S` (`citre-code-map-show-all`) to show the hidden items. They will be
automatically marked so that you can unmark some of them, or mark more items,
and hide all of them again. If you don't need this, type `M` to unmark all.

Type `k` (`citre-code-map-keep`) to keep the selected items and hide all other
items.

#### Delete items in file/symbol lists

In a file or symbol list, type `d` (`citre-code-map-delete`) to delete selected
items, or current items if there are no selected items. This operation can't be
undone so Citre will ask if you really want to delete them.

Type `k` to keep the selected items and delete all others. Still, Citre will
ask for your confirmation.

### Save and load the code map

In the code map, use standard key bindings of `save-buffer` (`C-x C-s` by
default) or `find file` (`C-x C-f` by default) to save or load the code map.
You can also call `citre-save-code-map` in any file in the project, or call
`citre-load-code-map` to do this.

Once you've saved the code map, or loaded it from the disk, then when exiting
Emacs, Citre will detect if the code map is modified since then, and ask if you
want to save it.

### Update the code map

The lines in definition lists are fetched from the files in real time. Changes
in the file can mess it up, then you can't jump to the right definition
locations.

If this happens, you should update the tags file first, then type `U`
(`citre-code-map-update`) in the code map to rescan definitions of all symbols
and update the code map.

Remember that the concept of code map really assumes the code is not changing
(think about it, there's actually no way to tell whether a new definition
location is the "updated version" of an old one), so all hidden definitions
will be unhide.

Currently, updating the code map can't handle situations where files in the
file list is missing/renamed (don't worry too much, all symbols under it would
still be preserved). If a symbol is renamed, Citre also won't know that. In the
future, commands that deal with such situations will be offered, but it's
generally not suggested to use code map for a very long code reading session
during which the code changes.
