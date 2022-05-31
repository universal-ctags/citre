#!/bin/sh

# Copyright (C) 2020 Masatake YAMATO
# License: GPL v3, or (at your option) any later version

# Check dependencies for tags backend
check_tags_deps()
{
    if [ ! -x "$(command -v ${READTAGS_PROG})" ]; then
        warning "Warning: readtags program not found. Test skipped."
        return 1
    else
        return 0
    fi
}

# Check dependencies for global backend
check_global_deps()
{
    if [ ! -x "$(command -v ${GTAGS_PROG})" ]; then
        warning "Warning: gtags program not found. Test skipped."
        return 1
    elif [ ! -x "$(command -v ${GLOBAL_PROG})" ]; then
        warning "Warning: global program not found. Test skipped."
        return 1
    else
        return 0
    fi
}

. "./scripts/common.sh"
ITEM="test"

# Run test. $1 is an element in test_files
run_test()
{
    d=$(dirname $1)
    info "$d"
    (cd $d
     if ! $EMACS -Q --batch -l ert $preload_options -l ../common.el \
          -l ./test.el -f ert-run-tests-batch-and-exit; then
         error "A test in $d failed."
     fi
     exit 0
    ) || exit 1
}
PRELOAD="citre.el citre-config.el"

d=$(pwd)
preload_options=

if [ -z "$1" ]; then
    test_files="tests/*/test.el"
else
    test_files="tests/$1*/test.el"
fi

preload_options="${preload_options} -L ${d}"

for p in $PRELOAD; do
    [ ! -e  $p ] && error "Cannot find $p."
    preload_options="${preload_options} -l $p "
done

for t in $test_files; do
    if [ ! $t = ${t#tests/tags} ]; then
        if $(check_tags_deps); then
            run_test $t
        elif [ $FAIL_ON_WARNING -ne 0 ]; then
            exit 1
        fi
    elif [ ! $t = ${t#tests/global} ]; then
        if $(check_global_deps); then
            run_test $t
        elif [ $FAIL_ON_WARNING -ne 0 ]; then
            exit 1
        fi
    else
        run_test $t
    fi
done

pass "All tests passed :)"
exit 0
