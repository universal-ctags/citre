# Contribution Guide

## Tests

Run `$ make` at the root of Citre project to run all the tests.

We've setup CI to run the tests automatically on various versions of Emacs. The
CI is triggered on pull requests, or on pushes to the master/develop branch.

## Style

- Please indent all source files.

- No lines in source file and documentation should be longer than 79
  characters, unless:

  - It's the first line in an elisp file
  - It's the first line in a docstring
  - It contains a web link

Don't worry to much as we also have style tests when you run `$ make` and on
the CI.

## Misc

The develop branch is there only for running tests on all versions of Emacs.
Once it's passed, we rebase the master branch to it. So the master branch is
really *not* a "stable branch".
