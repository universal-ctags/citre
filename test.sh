#!/bin/sh

EMACS=${EMACS:=emacs}
PRELOAD=citre.el

d=$(pwd)
preload_options=

error()
{
    echo "$@" 1>&2
    exit 1
}

for p in $PRELOAD; do
    [ ! -e  $p ] && error "cannot find $p"
    # Convert to complete file path
    if [ ${p:0:1} != '/' ]; then
       p=${d}/$p
    fi
    preload_options="${preload_options}-l $p "
done

for t in tests/*/test.el; do
    d=$(dirname $t)
    (cd $d
     if ! $EMACS -batch -l ert $preload_options -l ../common.el -l ./test.el \
	  -f ert-run-tests-batch-and-exit; then
	 error "testing is failed in $d"
     fi
     )
done

echo "OK"
exit 0
