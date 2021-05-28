#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

CTAGS_PROG=${CTAGS_PROG:=ctags}

ERROR_FACE="\033[1;31m"
NORMAL_FACE="\033[0m"

info()
{
    printf '[test tags] %s\n' "$@"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[test tags] %s' "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}

for t in tests/*/; do
    (cd $t
     for f in ctags.d/*.ctags; do
         # For test cases where there's no ctags.d directory, $f is literally
         # "ctags.d/*.ctags", which doesn't exist. So we need to test it.
         if test -f $f; then
             info $t$f
             # We don't want TAG_PROC_CWD since it will be different on
             # different machines. Citre can guess the cwd (see
             # `citre-core--get-dir`) and it works for our directory structure.
             if ! $CTAGS_PROG --quiet --options=NONE --options=$f \
                  --pseudo-tags=-{TAG_PROGRAM_VERSION}{TAG_PROC_CWD}; then
                 error "Can't generate tags file for $d."
             fi
         fi
     done
     exit 0
    ) || exit 1
done
