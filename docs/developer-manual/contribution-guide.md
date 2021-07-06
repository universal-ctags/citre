# Contribution Guide

## Tests

Run `$ make` at the root of Citre project to run all the tests.

We've setup CI to run the tests automatically on various versions of Emacs. The
CI is triggered on pull requests, or on pushes to the master/develop branch.

## Add a test case

Please refer to existing tests under `tests/`.

If you want to add a test case that doesn't use a tags file, see the test
`core-parse-pattern`.

If you want to add a test case that needs tags file(s), see the test
`core-main-apis` (use 1 tags file) and `core-locate-tag` (use multiple tags
file). For such test cases, you need to:

- Create a `src` directory and put your code files in it.
- Create a `ctags.d` directory.
- Create `*.ctags` file(s) under it. They contain the arguments for ctags to
  generate the tags files.
- Run `$ make test-tags` at the root of Citre project to generate the tags.
- Create a `test.el` to write your tests. `expand-test-file` is a commonly used
  function in these tests, see `tests/common.el` for its docstring.

I (@AmaiKinono) should regularly update u-ctags on my machine and run `$ make
test-tags` to update all the tags files. If a new version of u-ctags generates
a tags file differently and breaks a test case, I should fix it.

## Style

- Please indent all source files.

- No lines in source file and documentation should be longer than 79
  characters, unless:

  - It's the first line in an elisp file
  - It's the first line in a docstring
  - It contains a web link

Code or shell commands in the docs may contain long lines. In these cases,
break the line by a `\`.

Don't worry to much as we also have style tests when you run `$ make` and on
the CI.

## Misc

The develop branch is there only for running tests on all versions of Emacs.
Once it's passed, we rebase the master branch to it. So the master branch is
really *not* a "stable branch".

## Working with files

Currently file-related code is still a bit entangled in Citre, though I've put
much effort clear it up.

Let's start with basic concepts:

- absolute/canonical path: Canonical path is like absolute path, but the `~` is
  expanded, symbolic links are resolved, etc.

- local/remote path: We all know local paths. In Emacs, a filename can point to
  a remote file, like:

  ```
  /ssh:user@host:/path/to/file
  ```

These terms are orthogonal, e.g., you can have "remote canonical path" or
"local absolute path".

Here are some guidelines when working with filenames.

### When to use canonical/absolute path

When the filename is associated with additional info, it's better to use
canonical path to store them.

For example, in Citre, tags files come with additional info like its root
directory, etc. These are stored in a hash table, where the keys are canonical
paths of the tags files, and values are the additiona info.

That said, when exposing APIs to upper components, it's often be better to ask
for absolute path, and canonicalize it by `expand-file-name`, so upper
components can path canonical or non-canonical paths. Ideally, all APIs in
`citre-core.el` and `citre-util.el` should be designed like this.

### When to use local/remote path

Whenever you use Elisp to access a file, use local path for local files, and
remote path for remote files. It's easy to understand.

Troubles happens in 2 situations:

- When we run a readtags/ctags command line, we use local path even on remote
  machines, because the command is run on the remote machine itself. We need to
  pay attention to this when starting a process.

- When we use paths in a tags file, remember they are all local. If the tags
  file itself is on a remote machine, we need to convert them to remote path so
  we can visit them. Currently, the `dir` additional info field of tags files,
  the `ext-abspath` extension field, and `citre-core-filter-input` takes care
  of these problems.

### Others

Some functions, like `file-name-absolute-p`, works differently on different
platforms. Trouble happens when you visit a Unix remote machine on Windows, and
you use local path on that remote machine (e.g. it's come from the tags file),
these functions treat them as Windows paths. When this is a problem, we use the
`os` additional info field of the tags file to know the OS of the remote
machine, and do things manually.

The default `file-exists-p` doesn't differ directories and files. If you know
you want a directory or a file, use `citre-non-dir-file-exists-p` and
`citre-dir-exists-p` instead.
