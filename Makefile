# Copyright (C) 2020 Hao Wang
# License: GPL v3, or (at your option) any later version

SHELL = /bin/sh

.PHONY: check
check: style compile test

.PHONY: style
style:
	$(SHELL) scripts/style.sh

.PHONY: compile
compile:
	$(SHELL) scripts/compile.sh

.PHONY: test
test:
	$(SHELL) scripts/test.sh

.PHONY: clean
clean:
	rm ./*.elc
