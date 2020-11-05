#!/bin/sh

# Copyright (C) 2020 Masatake YAMATO
# License: GPL v3, or (at your option) any later version

# NOTE: READTAGS is also useable. It's handled in tests/common.el.
EMACS=${EMACS:=emacs}
PRELOAD="citre-readtags-tables.el citre-readtags.el citre.el"

ERROR_FACE="\e[1;31m"
PASSED_FACE="\e[1;32m"
NORMAL_FACE='\e[0m'

d=$(pwd)
preload_options=

pass()
{
    printf $PASSED_FACE
    printf '%s' "$@"
    printf "$NORMAL_FACE\n"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '%s' "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}

for p in $PRELOAD; do
    [ ! -e  $p ] && error "Cannot find $p."
    # Convert to complete file path
    if [ ${p:0:1} != '/' ]; then
        p=${d}/$p
    fi
    preload_options="${preload_options} -l $p "
done

for t in tests/*/test.el; do
    d=$(dirname $t)
    (cd $d
     if ! $EMACS -Q --batch -l ert $preload_options -l ../common.el \ \
          -l ./test.el -f ert-run-tests-batch-and-exit; then
         error "A test in $d failed."
     fi
     exit 0
    ) || exit 1
done

pass "All tests passed :)"
exit 0
