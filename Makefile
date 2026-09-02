EMACS ?= emacs
EASK ?= eask

.PHONY: compile test test-local check clean

compile:
	$(EASK) compile

test:
	$(EASK) run script test

test-local:
	$(EASK) run script test-local

check:
	$(EASK) lint checkdoc

clean:
	$(EASK) clean all
