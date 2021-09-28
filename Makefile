# Copyright (C) 2020 Hao Wang
# License: GPL v3, or (at your option) any later version

SHELL = /bin/sh

.PHONY: check
check: test compile style

.PHONY: test
test:
	$(SHELL) scripts/test.sh

.PHONY: test-tag
test-tag:
	$(SHELL) scripts/test.sh tag

.PHONY: test-ctags
test-ctags:
	$(SHELL) scripts/test.sh ctags

.PHONY: test-global
test-global:
	$(SHELL) scripts/test.sh global

.PHONY: compile
compile: clean
	$(SHELL) scripts/compile.sh

.PHONY: style
style:
	$(SHELL) scripts/style.sh

.PHONY: update-test-tags
test-tags:
	$(SHELL) scripts/update-test-tags.sh

.PHONY: clean
clean:
	rm -f ./*.elc

.PHONY: version
version:
	$(SHELL) scripts/version.sh
