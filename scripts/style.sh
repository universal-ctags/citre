#!/bin/sh

# Copyright (C) 2020 Hao WANG
# License: GPL v3, or (at your option) any later version

EMACS=${EMACS:=emacs}

PASSED_FACE="\e[1;32m"
ERROR_FACE="\e[1;31m"
NORMAL_FACE="\e[0m"

info()
{
    printf '[style] %s\n' "$@"
}

pass()
{
    printf $PASSED_FACE
    printf '[style] %s' "$@"
    printf "$NORMAL_FACE\n"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[style] %s' "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}

# TODO: checkdoc, check indent.
for f in *.el tests/common.el tests/*/test.el; do
    info "$f"
    # Allow long lines that are the first lines of the file, contains web
    # links, or are the first lines of docstrings.
    grep -n '.\{80,\}' $f \
        | grep -v "\(^1:\)\|\(http://\)\|\(https://\)\|\(^[0-9]\+:  \"\)" \
        && error "Long line found in $f."
    (if ! $EMACS -Q --batch \
          --eval "(setq inhibit-message t)" \
          # The if-let series of macros are defined in subr-x, and has their
          # own indent declarations.
          --eval "(require 'subr-x)" \
          --eval "(find-file \"$f\")" \
          --eval "(indent-region (point-min) (point-max))" \
          --eval "(when (buffer-modified-p) (kill-emacs 1))"; then
         error "Wrong indentation in $f."
     fi
     exit 0
    ) || exit 1
done

for f in scripts/*.sh; do
    info "$f"
    grep -n '.\{80,\}' $f | grep -v "\(http://\)\|\(https://\)" \
        && error "Lone line found in $f";
done

# TODO: Check long lines for docs.

pass "Style check passed :)"
exit 0
