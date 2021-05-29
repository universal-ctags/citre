#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"
ITEM="test tags"

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
