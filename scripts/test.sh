#!/bin/sh

# Copyright (C) 2020 Masatake YAMATO
# License: GPL v3, or (at your option) any later version

# NOTE: READTAGS is also useable. It's handled in tests/common.el.
EMACS=${EMACS:=emacs}
PRELOAD="citre-core-tables.el citre-core.el citre-util.el \
citre-basic-tools.el citre-peek.el citre-lang-fileref.el citre.el"

PASSED_FACE="\033[1;32m"
ERROR_FACE="\033[1;31m"
NORMAL_FACE="\033[0m"

d=$(pwd)
preload_options=

info()
{
    printf '[test] %s\n' "$@"
}

pass()
{
    printf $PASSED_FACE
    printf '[test] %s' "$@"
    printf "$NORMAL_FACE\n"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[test] %s' "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}

for p in $PRELOAD; do
    [ ! -e  $p ] && error "Cannot find $p."
    # If p doesn't start with "/" (i.e. is relative), convert to absolute path.
    if [ "$p" = "${p#/}" ]; then
        p=${d}/$p
    fi
    preload_options="${preload_options} -l $p "
done

for t in tests/*/test.el; do
    d=$(dirname $t)
    info "$d"
    (cd $d
     if ! $EMACS -Q --batch -l ert $preload_options -l ../common.el \
          -l ./test.el -f ert-run-tests-batch-and-exit; then
         error "A test in $d failed."
     fi
     exit 0
    ) || exit 1
done

pass "All tests passed :)"
exit 0
