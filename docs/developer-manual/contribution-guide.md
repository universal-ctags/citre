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
