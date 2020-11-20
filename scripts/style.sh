#!/bin/sh

# Copyright (C) 2020 Hao WANG
# License: GPL v3, or (at your option) any later version

EMACS=${EMACS:=emacs}

PASSED_FACE="\e[1;32m"
ERROR_FACE="\e[1;31m"
NORMAL_FACE="\e[0m"

info()
{
    printf '%s\n' "$@"
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

## Check long lines

for f in *.el tests/common.el tests/*/test.el; do
    info "[style, longline] $f"
    # Allow long lines that are the first lines of the file, contains web
    # links, or are the first lines of docstrings.
    grep -n '.\{80,\}' $f \
        | grep -v "\(^1:\)\|\(http://\)\|\(https://\)\|\(^[0-9]\+:  \"\)" \
        && error "Long line found in $f."
done

for f in scripts/*.sh; do
    info "[style, longline] $f"
    grep -n '.\{80,\}' $f | grep -v "\(http://\)\|\(https://\)" \
        && error "Lone line found in $f";
done

# TODO: Check long lines for docs.

## Check indentation

for f in *.el tests/common.el tests/*/test.el; do
    info "[style, indent] $f"
    # Some macros may have indent declarations. We (eval-buffer) to apply them.
    (if ! $EMACS -Q --batch -L . \
          --eval "(setq inhibit-message t)" \
          --eval "(find-file \"$f\")" \
          --eval "(eval-buffer)" \
          --eval "(indent-region (point-min) (point-max))" \
          --eval "(when (buffer-modified-p) (kill-emacs 1))"; then
         error "Wrong indentation in $f."
     fi
     exit 0
    ) || exit 1
done

## Checkdoc

for f in *.el; do
    info "[style, checkdoc] $f"
    (if $EMACS -Q --batch \
               --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
               --eval "(checkdoc-file \"$f\")" 2>&1 \
            | grep .; then
         error "Checkdoc failed in $f."
     fi
     exit 0
    ) || exit 1
done

pass "Style check passed :)"
exit 0
