<p align="center">
  <img src="./img/logo.svg" alt="logo">
</p>

<p align="center"><i>Ctags IDE on the True Editor!</i></p>

## What is it?

(If you are already familiar with Citre, see [changelog](CHANGELOG.md) for the
news.)

Citre is an advanced Ctags (or actually, readtags) frontend for Emacs. It
offers:

- `completion-at-point`, xref and imenu integration.
- `citre-jump`: A `completing-read` UI for jumping to definition.
- `citre-peek`: A powerful code reading tool that lets you go down the rabbit
  hole without leaving current buffer.

Let's see them in action!

- `completion-at-point`, with the UI of
  [company](https://company-mode.github.io/) and
  [Vertico](https://github.com/minad/vertico):

  ![capf](./img/capf.jpg)

  Notice the rich annotations. Candidates are annotated by `(kind/type@scope)`,
  so you know "it's a member of struct `thread`, with `pid_t` type", etc. This
  is because Ctags "tags" format records much more abundant info than the etags
  "TAGS" format.

  Also, notice that candidates with the "member" kind are put above the others
  because we are in a C source file, and the current symbol is after a dot.
  Citre guesses that you want a struct member.

- `citre-jump`, with `completing-read` UI provided by
  [Selectrum](https://github.com/raxod502/selectrum):

  ![citre-jump](./img/citre-jump.jpg)

- `citre-peek`. It opens a "peek window" to show the definition of a symbol:

  ![citre-peek](./img/citre-peek.jpg)

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
[hasktags](https://hackage.haskell.org/package/hasktags),
[gotags](https://github.com/jstemmer/gotags) and
[ripper-tags](https://github.com/tmm1/ripper-tags). You don't need to customize
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

#### The customizable way

Open a file in your project, type `M-x citre-update-this-tags-file`. If it
can't find a tags file for the current file, it'll guide you to generate one.
I'll take you through the simplest situation here. To know more, read [this
user manual](docs/user-manual/about-tags-file.md).

You are asked to:

1. Pick a place to save the tags file, type `1`.

2. Pick a directory in which to use the tags file. This means when you visit a
   file in that directory, this tags file is used for it.

   Let's genearte a tags file for the whole project and use it in the whole
   project, so choose `/path/to/project/`.

3. Pick a tags file name. It'll be saved inside `/path/to/project/`. Pick one
   you like.

4. Pick a root dir to run ctags. This is the working directory when running
   Ctags. Let's pick the project root.

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

#### The simpler way

Citre searches the tags file in different locations, like in the directory that
uses it, in global/project cache dir, etc. See [this
documentation](docs/user-manual/about-tags-file.md) to know the details.

`citre-default-create-tags-file-location` lets you choose a default location.
For example, if you always want to use the global cache dir, set it to
`'global-cache`.

Most of the time, people just create a tags file for the whole project and use
it in the whole project. If you want Citre to do this by default, rather than
ask you a lot of questions, set `citre-use-project-root-when-creating-tags` to
`t`. This uses `citre-project-root-function` to detect the project root.

If you don't want to edit the command line manually, set
`citre-prompt-language-for-ctags-command` to `t`. Then, instead of giving you a
buffer to edit the command, Citre lets you choose the languages to scan, and
generates a command that should work for most projects. This requires the ctags
program from Universal Ctags.

In any situations, you could further edit the tags file updating recipe by
`citre-edit-tags-file-recipe` later.

#### The command line way

You don't have to create a tags file using Citre. You can just `cd` to the
project root directory and run:

``` console
$ ctags --languages=c,c++,... --kinds-all='*' --fields='*' --extras='*' -R
```

This creates a `tags` file in the project root, and Citre could find it. To
know how Citre finds a tags file for the current buffer, see [this
documentation](docs/user-manual/about-tags-file.md)

Tags file created this way can't be updated by Citre.

#### Notes

See [this documentation](docs/user-manual/about-tags-file.md) to know more
about tags file format, how to tweak the command line, how to specify which dir
uses which tags file, etc.

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

By default, when you open a file, and a tags file can be found for it,
`citre-mode` is automatically enabled. If you don't use `citre-config`, you can
put this in your configuration:

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
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   citre-readtags-program "/path/to/readtags"
   citre-ctags-program "/path/to/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)))
```

See [this documentation](docs/user-manual/toc.md) to know more customizable
options.

## Documentations

- [User Manual](docs/user-manual/toc.md)
- [Developer Manual](docs/developer-manual/toc.md)
- [Wiki](https://github.com/universal-ctags/citre/wiki)

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

- Q: What to do if Citre didn't grab the right symbol for me, e.g., I want to
  find the definition of `foo.bar`, but can only get `foo` or `bar`?

  A: You can select `foo.bar` first (by an active region), then find its
  definitions.

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

Below are the status of tools provided by Citre:

| Tool              | Description                          | Status | Note |
|-------------------|--------------------------------------|--------|------|
| Ctags             | Create/update tags files             | alpha  | [^1] |
| capf, xref, imenu | Integration with built-in mechanisms | stable |      |
| `citre-jump`      | Jump to the definition               | stable |      |
| `citre-peek`      | Deep code reading in a peek window   | beta   | [^2] |

[^1]: Universal Ctags is exploring concepts like [incremental
      updating](https://github.com/universal-ctags/ctags/issues/2697),
      [multi-pass parsing](https://github.com/universal-ctags/ctags/pull/2741),
      and more. Citre may follow the changes happen in Universal Ctags.

[^2]: I plan to implement a feature that lets you further filter the
      definitions in a peek window.

"alpha" means the tool is likely to go through breaking changes. "beta" means
new features and improvements may happen. "stable" means the tool is basically
finished.

Below are new tools I have in mind, and may come in the future:

- `citre-diagnostics`: Check if you have the right version of readtags, ctags;
  show the project root, the tags file being used, and things like that.

- A tool that lets you interactively filter a tags file (find a tag whose name
  contains "foo", the path contains "bar", with the kind "function", etc). Then
  you can use the results as completion (insert in current bufffer), visit
  their definition, or convert them into a `citre-peek` or xref session. I
  consider this to be the "ultimate weapon" of Citre.

## Donation

If Citre makes you happy, please consider buying me (@AmaiKinono) a beer to
make me happy ;)

- by Alipay

  <img src="./img/alipay.jpg" alt="Alipay" width="180" />

- by Wechat Pay

  <img src="./img/wechat-pay.png" alt="Wechat Pay" width="180" />

- by [Buy me a coffee](https://www.buymeacoffee.com/amaikinono)
