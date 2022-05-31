#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

## Executables

EMACS=${EMACS:=emacs}
CTAGS_PROG=${CTAGS_PROG:=ctags}
READTAGS_PROG=${READTAGS_PROG:=readtags}
GTAGS_PROG=${GTAGS_PROG:=gtags}
GLOBAL_PROG=${GLOBAL_PROG:=global}

## Helper functions

export PASSED_FACE="\033[1;32m"
export WARNING_FACE="\033[1;33m"
export ERROR_FACE="\033[1;31m"
export NORMAL_FACE="\033[0m"


info()
{
    printf '[%s] %s\n' "$ITEM" "$@"
}

pass()
{
    printf $PASSED_FACE
    printf '[%s] %s' "$ITEM" "$@"
    printf "$NORMAL_FACE\n"
}

warning()
{
    printf $WARNING_FACE 1>&2
    printf '[%s] %s' "$ITEM" "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[%s] %s' "$ITEM" "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}
