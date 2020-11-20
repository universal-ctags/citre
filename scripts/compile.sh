#!/bin/sh

# Copyright (C) 2020 Hao WANG
# License: GPL v3, or (at your option) any later version

EMACS=${EMACS:=emacs}
SRC_FILES="citre-readtags-tables.el citre-readtags.el citre.el"

PASSED_FACE="\e[1;32m"
ERROR_FACE="\e[1;31m"
NORMAL_FACE="\e[0m"

info()
{
    printf '[compile] %s\n' "$@"
}

pass()
{
    printf $PASSED_FACE
    printf '[compile] %s' "$@"
    printf "$NORMAL_FACE\n"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[compile] %s' "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}

for f in $SRC_FILES; do
    [ ! -e $f ] && error "Cannot find $f."
    info "$f"
    ($EMACS -Q --batch --eval "(setq byte-compilation-error-on-warn t)" \
            -L . -f batch-byte-compile $f 2>&1 \
         | grep . && error "Failed compiling $f." || exit 0
    ) || exit 1
done

pass "Byte compilation succeeded :)"
exit 0
