<p align="center">
  <img src="logo.svg" alt="logo">
</p>

<p align="center"><i>Ctags IDE on the True Editor!</i></p>

## What is it?

Citre is an advanced Ctags (or actually, readtags) frontend for Emacs. It
offers:

- `completion-at-point`, xref and imenu integration.
- `citre-jump`: A `completing-read` UI for jumping to definition.
- `citre-peek`: A powerful code reading tool that lets you go down the rabbit
  hole without leaving current buffer.

Let's see them in action!

- `completion-at-point`, with the UI of
  [company](https://company-mode.github.io/) and
  [Selectrum](https://github.com/raxod502/selectrum):

  ![capf](capf.jpg)

  Notice the rich annotations. Candidates are annotated by `(kind/type)`, so
  you know "it's a struct member with `pid_t` type", etc. This is because Ctags
  "tags" format records much more abundant info than the etags "TAGS" format.

  Also, notice that candidates with the "member" kind are put above the others
  because we are in a C source file, and the current symbol is after a dot.
  Citre guesses that you want a struct member.

- `citre-jump`, with `completing-read` UI provided by
  [Selectrum](https://github.com/raxod502/selectrum):

  ![citre-jump](citre-jump.jpg)

- `citre-peek`. It opens a "peek window" to show the definition of a symbol:

  ![citre-peek](citre-peek.jpg)

  And there's more. Notice the code reading history at the bottom of the peek
  window. Do you hate having to switch between a lot of buffers while reading
  code? With `citre-peek`, you can peek a symbol in the peek window. This
  allows a tree-like code reading history, that you can browse and edit,
  without leaving current buffer!

All above screenshots were taken in a huge project (the Linux kernel), and
Citre is still fast, because readtags performes binary search on the tags file.

## Quick start

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
  `$ ctags --version` to see if you are using Universal Ctags. The version is a
  little hard to inspect since Universal Ctags doesn't have a formal version
  number yet. If it's compiled before Jan 21 2021, it will probably not work.
  You can [build it
  yourself](https://github.com/universal-ctags/ctags#how-to-build-and-install),
  or try the [snap package](https://github.com/universal-ctags/ctags-snap).

- For macOS users: Follow the instructions
  [here](https://github.com/universal-ctags/homebrew-universal-ctags) to
  install the latest version.

- For Windows users: Download the binary
  [here](https://github.com/universal-ctags/ctags-win32). Ctags in cygwin (or
  msys repo of msys2) won't work since it doesn't come with readtags. Ctags in
  the mingw64 repo of msys2 is Universal Ctags, but by the time of writing, it
  doesn't meet the version requirement.

If you don't have readtags executable in your PATH, customize
`citre-readtags-program` to the path of it.

#### ctags

If you use Citre's built-in tools to create tags file, you need a ctags
program.

The ctags program provided by Universal Ctags is still recommended. If you
don't have it in your PATH, customize `citre-ctags-program` to the path of it.

You can also use other program that outputs a tags file, like
[hasktags](https://hackage.haskell.org/package/hasktags) or
[gotags](https://github.com/jstemmer/gotags). You don't need to customize
`citre-ctags-program` when using these tools.

### Installation

You can install `citre` from [MELPA](https://melpa.org). Below are instructions
to manually install Citre.

1. Clone this repository:

   ```console
   $ git clone https://github.com/universal-ctags/citre.git /path/to/citre
   ```

2. Add the path to your `load-path` in your Emacs configuration:

   ```elisp
   (add-to-list 'load-path "/path/to/citre")
   ```

3. Require `citre` and `citre-config` in your configuration:

   ```elisp
   (require 'citre)
   (require 'citre-config)
   ```

   Or, you can read [citre-config.el](citre-config.el), and write your own
   config.

### Create tags file

Open a file in your project, type `M-x citre-update-this-tags-file`. If it
can't find a tags file for the current file, it'll guide you to generate one.
I'll take you through the simplest situation here. To know more, read [this
user manual](docs/user-manual/about-tags-file.md).

You are asked to:

1. Pick a place to save the tags file, type `1`.
2. Pick a directory in which to use the tags file. Let's genearte a tags file
   for the whole project and use it in the whole project, so choose
   `/path/to/project/`.
3. Pick a tags file name. It'll be saved inside `/path/to/project/`. Pick one
   you like.
4. Pick a root dir to run ctags. Pick the project root.
5. You are taken to a command editing buffer, and a help message is shown in
   the buffer. Read it.

If you use Universal Ctags, you may write a command like:

```
ctags
-o
%TAGSFILE%
--languages=C,C++
--kinds-all=*
--fields=*
--extras=*
-R
./
/external/lib/used/in/the/project/
```

You can also use other ctags program, for example, gotags:

```
~/go/bin/gotags
-R=true
-f=%TAGSFILE%
./
/external/lib/used/in/the/project/
```

You get the idea.

Once you've created such a file, run `M-x citre-update-this-tags-file` again to
update it. The recipe for updating a tags file is stored in the tags file
itself, so no more configuration file or buffer-local variables are needed!

You can edit the updating recipe later by `citre-edit-tags-file-recipe`.

See [this user manual](docs/user-manual/about-tags-file.md) to know more about
tags file format, how to tweak the command line, how to specify which dir uses
which tags file, etc.

*Note:* Emacs users are more familiar with the TAGS format. TAGS format is
generated by `etags` or `$ ctags -e`, and Citre doesn't support it.

Citre supports the tags format, which is the default format used by Ctags.
Simply puts it, tags format is much more informative than TAGS format, making
Citre a much more powerful tool. See [this user
manual](docs/user-manual/compare-with-other-tools.md) for details.

*Note for Windows and macOS users:* Windows and macOS uses case-insensitive
file system by default, so this may happen:

- You create a tags file named `tags`.
- Some plugins like `projectile` and `company` (when using the `company-etags`
  backend) tries find and load a `TAGS` file, which is the default file used by
  Emacs etags.
- Since `tags` and `TAGS` are the same to the file system, they tries to load
  the `tags` file, which can't be recognised by `etags.el`.
- You'll see a "TAGS is not valid tags table" error.

To avoid this problem, you could configure those plugins to not use a tags
file, or simply avoid creating a tags file named `tags` on Windows and macOS.

### Use Citre

Use `citre-mode` to enable `completion-at-point`, xref and imenu integration.
If you also use `company`, make sure `company-capf` is in `company-backends`.

By default, `citre-mode` is automatically enabled when you open a file, and a
tags file can be found for it. If you don't use `citre-config`, you can put
this in your configuration:

``` elisp
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)
```

`citre-jump` and `citre-peek` works without `citre-mode`. Type `M-x citre-jump`
on a symbol to jump to its definition, `M-x citre-jump-back` to go back in the
jump history. About `citre-peek`, See [this user
manual](docs/user-manual/citre-peek.md) to know how to use it.

Here's a example configuration using
[`use-package`](https://github.com/jwiegley/use-package). Be sure to read it
and tweak it to your own need.

``` elisp
(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set this if readtags is not in your path.
   citre-readtags-program "/path/to/readtags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root))
```

See [this user manual](docs/user-manual/toc.md) to know more customizable
options.

## Manuals

- [User Manual](docs/user-manual/toc.md)
- [Developer Manual](docs/developer-manual/toc.md)

[This chapter](docs/developer-manual/design-principle.md) in the developer
manual talks about the strengths/weaknesses of ctags, and the design principle
of Citre. Non-developers are also encouraged to read it to know more about
these tools.

## FAQ

- Q: What are the advantages of Citre & Ctags over etags, gtags, language
  servers...

  A: See [this documentation](docs/user-manual/compare-with-other-tools.md).

- Q: How to use Citre over TRAMP?

  A: Make sure you've installed readtags on the remote machine, and everything
  will just work. Tags file generating/updating also works if you have ctags
  program on the remote machine.

- Q: Why doesn't Citre support automatically update tags file?

  A: Citre uses both line number and a search pattern to locate a tag. When the
  file containing the tag is edited, Citre could still locate the tag using the
  search pattern. Citre even tries to locate the tag when the line containing
  the tag itself is edited.

  So, jumping to definition is still useable when the file is edited. There's
  no need to frequently update the tags file.

  You may ask "what if I add new definitions, or modify/delete existing ones?"
  The truth is, if your codebase is reasonably large that you have to index
  them by Ctags, then small edits won't cause much trouble. You can just
  update the tags file when needed.

- Q: How many languages does Citre support?

  A: Citre supports all languages that Ctags support. The latest [Universal
  Ctags](https://github.com/universal-ctags/ctags) support 134(!) languages:

  ```console
  $ ctags --list-languages | wc -l
  134
  ```

  Besides, you could [define your own parser using
  regex](http://docs.ctags.io/en/latest/man/ctags-optlib.7.html) to support
  more languages.

- Q: But seems for now Citre only has support code for C...

  A: No matter what's the language, as long as you have a tags file for it,
  then Citre works out of the box. Language-specific support is for extra minor
  goodies, see the "Commentary" section in each language-support code file.

## Current status

Citre is in its alpha stage. The authors are still exploring the designing and
usage of the tools provided by Citre, so we may introduce some breaking
changes.

That said, I've put much effort polishing the tools, and have been using it
daily for a long time. Citre is useable, and offers great tools, so just try
it!
