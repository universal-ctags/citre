#!/bin/sh

# Copyright (C) 2020 Masatake YAMATO
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"
ITEM="test"

PRELOAD="citre-common-util.el citre-common-tag.el citre-readtags-tables.el
citre-readtags.el citre-ui-jump.el citre-ui-peek.el citre-util.el
citre-ctags.el citre-basic-tools.el citre-tags.el citre-global.el
citre-lang-c.el citre-lang-fileref.el citre.el citre-config.el"

d=$(pwd)
preload_options=

if [ -z "$1" ]; then
    test_files="tests/*/test.el"
else
    test_files="tests/$1-*/test.el"
fi

for p in $PRELOAD; do
    [ ! -e  $p ] && error "Cannot find $p."
    # If p doesn't start with "/" (i.e. is relative), convert to absolute path.
    if [ "$p" = "${p#/}" ]; then
        p=${d}/$p
    fi
    preload_options="${preload_options} -l $p "
done

for t in $test_files; do
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
