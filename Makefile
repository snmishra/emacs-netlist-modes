# -*- makefile-gmake-*-
all: spectre-mode spice-mode eldo-mode aplac-mode
spice-mode eldo-mode aplac-mode:
	emacs -Q -batch -f batch-byte-compile-if-not-done $@/*.el
spectre-mode:
	emacs -Q -batch -L spectre-mode \
	-f batch-byte-compile-if-not-done $@/*.el
clean:
	$(RM) */*.elc

.PHONY: spectre-mode spice-mode eldo-mode aplac-mode
