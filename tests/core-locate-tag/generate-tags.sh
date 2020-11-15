#/bin/sh

CTAGS_PROG=${CTAGS_PROG:=ctags}

for f in .ctags.d/*.ctags; do
    $CTAGS_PROG --options=NONE --options=$f
done
