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
amount of languages, produces abundant information, and is well maintained.

You can also use other program that outputs a tags file, like
[hasktags](https://hackage.haskell.org/package/hasktags),
[gotags](https://github.com/jstemmer/gotags) and
[ripper-tags](https://github.com/tmm1/ripper-tags).

Note that Emacs comes with a ctags program when installing from source, which
may override the Universal Ctags.

### Create a tags file

After creating a tags file, you'll be able to use all the Citre tools through
the tags bckend. But note that tags backend doesn't support finding references
for now.

#### The customizable way

Open a file in your project, type `M-x citre-update-this-tags-file`. If it
can't find a tags file for the current file, it'll guide you to generate one.
I'll take you through the process here.

You are asked to:

1. Pick a place to save the tags file, type `1`, which will save the tags file
   in the project.

2. Pick a directory in which to use the tags file. This means when you visit a
   file in that directory, this tags file is used for it.

   Let's genearte a tags file for the whole project and use it in the whole
   project, so choose `/path/to/project/`.

3. Pick a tags file name between `tags` and `.tags`. It'll be saved to
   `/path/to/project/`.

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

When Citre seeks the tags file for current buffer, it will look into different
locations, like in the "project root", in global/project cache dir, etc.

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
program from Universal Ctags. If you don't have it in your PATH, customize
`citre-ctags-program` to the path of it.

In any situations, you could further edit the tags file updating recipe by
`citre-edit-tags-file-recipe` later.

#### The command line way

You don't have to create a tags file using Citre. You can just `cd` to the
project root directory and run:

``` console
$ ctags --languages=c,c++,... --kinds-all='*' --fields='*' --extras='*' -R
```

This creates a `tags` file in the project root, and Citre could find it.

Tags file created this way can't be updated by Citre.

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
- Since `tags` and `TAGS` are the same to the file system, they tries to load
  the `tags` file, which can't be recognised by `etags.el`.
- You'll see a "TAGS is not valid tags table" error.

To avoid this problem, you could configure those plugins to not use a tags
file, or simply avoid creating a tags file named `tags` on Windows and macOS.
