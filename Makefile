# Copyright (C) 2020 Hao Wang
# License: GPL v3, or (at your option) any later version

SHELL = /bin/sh

.PHONY: check
check: test

.PHONY: test
test:
	$(SHELL) scripts/test.sh
