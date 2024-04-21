# Getting started on tags backend

### Prerequisites

#### readtags

Citre uses readtags program to read from tags files.

Citre requires readtags program provided by [Universal
Ctags](https://github.com/universal-ctags/ctags). The minimal version is:

- commit `31d13e85`, or
- weekly release p5.9.20200124.0

It's recommended to get the latest version, as Citre actively takes advantage
of its latest features.

- For GNU/Linux users: If you install ctags from your software repository, run
  `$ ctags --version` to see if you are using Universal Ctags with the required
  version.

- For macOS users: Follow the instructions
  [here](https://github.com/universal-ctags/homebrew-universal-ctags) to
  install the latest version.

- For Windows users: Download the binary
  [here](https://github.com/universal-ctags/ctags-win32). Ctags in cygwin (or
  msys repo of msys2) won't work since it doesn't come with readtags. Ctags in
  the mingw64 repo works.

If you don't have readtags executable in your PATH, customize
`citre-readtags-program` to the path of it.

#### ctags

You need a ctags program to generate a tags file for your project. The ctags
program provided by Universal Ctags is still recommended, as it supports a huge
amount of languages, produces abundant information, and is well maintained. If
you use Citre commands to generate the tags file, Universal Ctags is required.

You can also use other program that outputs a tags file, like
[hasktags](https://hackage.haskell.org/package/hasktags),
[gotags](https://github.com/jstemmer/gotags) and
[ripper-tags](https://github.com/tmm1/ripper-tags). Citre doesn't have commands
for them so you'll have to use the command line.

Note that Emacs comes with a ctags program when installing from source, which
may override the Universal Ctags program.

### Create a tags file

After creating a tags file, you'll be able to use Citre tools through the tags
bckend. But note that tags backend doesn't support finding references for now.

#### The customizable way

Open a file in your project, type `M-x citre-update-this-tags-file`. If it
can't find a tags file for the current file, it'll guide you to generate one.
I'll take you through the process here.

You are asked to:

1. Pick a directory in which you want to use the tags file. This means when you
   visit a file in that directory, this tags file is used for it. Usually it's
   the "project root", so let's choose `/path/to/project/`.

1. Pick a place to save the tags file, the option `1` saves it in the project
   root, while option `2` saves it in a global cache directory, which is
   `~/.cache/tags/` by default, in case you don't want to pollute your project.
   Let's choose `1` this time.

3. Pick a tags file name between `tags` and `.tags`. It'll be saved to
   `/path/to/project/`.

4. An option file `/path/to/project/.ctags.d/0.ctags` is generated and opened.
   If you chose to save the tags file in the global cache dir, this option file
   will also be there. It is used for generating and updating the tags file.
   Please read the help message in the opened file.

5. Type `M-x citre-ctags-cmd-buf-add-langs` (keybinding is `C-c l` by default)
   in the `--languages=` line to add languages to be scanned. You could also
   change the options in other ways to your need.

6. Type `M-x citre-ctags-cmd-buf-commit` (keybinding is `C-c C-c` by default)
   to save the options, close the buffer, and generate the tags file.

Once you've created such a file, run `M-x citre-update-this-tags-file` again to
update it. `/path/to/project/.ctags.d/0.ctags` is a standard path recognised by
ctags program, so you can also update it by running `ctags` in a shell the
project root.

You can edit the option file later by `citre-edit-tags-file-recipe`.

#### The simpler way

`citre-default-create-tags-file-location` lets you choose a default location.
For example, if you always want to use the global cache dir, set it to
`'global-cache`, so you don't have to choose one every time.

If you don't want to edit the command line manually, set
`citre-edit-ctags-options-manually` to `nil`. Then, instead of giving you a
buffer to edit the command, Citre lets you choose the languages to scan, and
generates options that should work for most projects.

In any situations, you could further edit the tags file updating recipe by
`citre-edit-tags-file-recipe` later.

#### The command line way

You don't have to create a tags file using Citre. You can just `cd` to the
project root directory and run:

``` console
$ ctags --languages=c,c++,... --kinds-all='*' --fields='*' --extras='*' -R
```

This creates a `tags` file in the project root, and Citre could find it.

Tags file created this way can't be updated by Citre. If you use are not using
Universal Ctags, you have to use the command line.

You may want to add "generating tags file" as a target/task in your project's
build system to avoid typing the command every time.

By using the command line, you could store the tags file wherever you want. To
use a certain tags file in a directory, customize `citre-ctags-program` as a
directory-local variable. For example, You've tagged `/usr/include/` and store
the tags file to `~/tags/sys-headers.tags`. Add this to your `init.el`

``` elisp
(dir-locals-set-class-variables
 'sys-header
 '((nil . ((citre-tags-file . "~/tags/sys-headers.tags")))))

(dir-locals-set-directory-class
 "/usr/include/" 'sys-header)
```

to use this tags file when visiting files in `/usr/include/`.

### Notes about the TAGS format

Emacs users are more familiar with the TAGS format. TAGS format is generated by
`etags` or `$ ctags -e`, and Citre doesn't support it.

Citre supports the tags format, which is the default format used by Ctags.
Simply puts it, tags format is much more informative than TAGS format, making
Citre a much more powerful tool. See [Source Code Indexing Tools
Comparison](source-code-indexing-tools-comparison.md) for details.

*Note for Windows and macOS users:* Windows and macOS uses case-insensitive
file system by default, so this may happen:

- You create a tags file named `tags`.
- Some plugins like `projectile` and `company` (when using the `company-etags`
  backend) tries find and load a `TAGS` file, which is the default file used by
  Emacs etags.
- Since `tags` and `TAGS` are the same to the file system, they try to load the
  `tags` file, which can't be recognised by `etags.el`.
- You'll see a "TAGS is not valid tags table" error.

To avoid this problem, you could configure those plugins to not use a tags
file, or simply avoid creating a tags file named `tags` on Windows and macOS.
