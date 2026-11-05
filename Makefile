# -*- Makefile -*-

# Development is driven by Eldev: <https://github.com/emacs-eldev/eldev>
# Install it with `curl -fsSL https://raw.githubusercontent.com/emacs-eldev/eldev/master/webinstall/eldev | sh'
# or via your package manager.

ELDEV ?= eldev

.PHONY: all prepare lint compile test clean

all: lint compile test

prepare:
	$(ELDEV) prepare

lint:
	$(ELDEV) lint

compile:
	$(ELDEV) compile --warnings-as-errors

test:
	$(ELDEV) test

clean:
	$(ELDEV) clean
