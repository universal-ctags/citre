#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"
ITEM="version"

if [ ! -v NEW_VERSION ]; then
    error "NEW_VERSION variable is not set."
fi

for f in *.el; do
    info "$f"
    sed -ri "s/(^;; Version: ).*$/\1$NEW_VERSION/" $f
done
