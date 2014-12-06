# -*- makefile-gmake-*-
MODES = spectre-mode spice-mode eldo-mode aplac-mode
NONSPECTRE = $(subst spectre-mode,,$(MODES))

prefix = /usr/local
bindir = $(prefix)/bin
datadir = $(prefix)/share
sitelispdir = $(datadir)/emacs/site-lisp
sitestartdir = $(sitelispdir)/site-start.d
netlistmodeir = $(sitelispdir)/emacs-netlist-modes



all: $(MODES) netlist-loaddefs.el

$(NONSPECTRE):
	emacs -Q -batch -f batch-byte-compile-if-not-done $@/*.el
spectre-mode:
	emacs -Q -batch -L spectre-mode \
	-f batch-byte-compile-if-not-done $@/*.el

netlist-loaddefs.el:
	emacs -Q -batch --eval '(setq generated-autoload-file (concat command-line-default-directory "netlist-loaddefs.el"))' -f batch-update-autoloads $(MODES)

install: all
	mkdir -p $(netlistmodeir)
	cp -rp $(MODES) coloreldo $(netlistmodeir)
	test -d $(sitestartdir) || mkdir -p $(sitestartdir)
	cp -p netlist-loaddefs.el $(sitestartdir)
	cp -p coloreldo/coloreldo.pl coloreldo/celdo $(bindir)

clean:
	$(RM) */*.elc

.PHONY: $(MODES)
