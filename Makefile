export EMACS ?= emacs
CASK=$(shell which cask)

DEBUG=

ifdef AA_DEBUG
DEBUG:=${DEBUG} --eval "(custom-set-default (quote aa-defs-debug) t)" --eval "(setq debug-on-error t)"
endif

EMACSBATCH=$(EMACS) -Q --batch -L ./lisp -l cl-lib $(DEBUG)

RM=rm -f
PKG_DESCS_MK=.pkg-descs.mk

ifneq ($(CASK),)
CASK_DIR := $(shell $(CASK) package-directory)

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	cask install
	touch $(CASK_DIR)
endif

ifneq ($(CASK),)
.DEFAULT_GOAL := compile
TESTSSRC := $(shell ls test/*.el)
TESTSLOAD := $(patsubst %.el,-l %,$(TESTSSRC))
TESTSELC := $(TESTSSRC:.el=.elc)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)
	cask emacs -batch -L . -L test \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(TESTSSRC); \
	  (ret=$$? ; rm -f $(TESTSELC) && exit $$ret)
	rm -f aa-autoloads.el

.PHONY: test
test: compile
	cask emacs --batch -L . -L test $(TESTSLOAD) -f ert-run-tests-batch
endif

pkgs := $(wildcard packages/*)
autoloads := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-autoloads.el)
descs := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)

.PHONY: clean clean/%
clean:
	$(RM) $(PKG_DESCS_MK)
	for pkg in $(notdir $(pkgs)) ; do make clean/$${pkg} ; done

clean/%:
	$(RM) $(filter packages/$*/%, $(autoloads)) $(filter packages/$*/%, $(descs))
	find $(filter packages/$*, $(pkgs)) -name '*.elc' -print0 | xargs -0 $(RM)

.PHONY: tidy/%
tidy/%:
	@$(EMACSBATCH) -f aa-admin-batch-tidy "$*"

.PHONY: fetch/%
fetch/%:
	@$(EMACSBATCH) -f aa-admin-batch-fetch "$*"

.PHONY: build/%
build/%:
	@$(EMACSBATCH) -f aa-admin-batch-build "$*"

.PHONY: hulk-smash
hulk-smash:
	@$(EMACSBATCH) -f aa-admin-purge

README.rst: README.in.rst lisp/aa.el Makefile
	grep ';;' lisp/aa.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;\s\?//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	mkdir dist
	cp -p $$(git ls-files lisp/aa*.el) dist

.PHONY: install
install: dist
	$(EMACSBATCH) --eval "(package-initialize 'no-activate)" \
	  --eval "(add-to-list 'package-archives '(\"shmelpa\" . \"https://shmelpa.commandlinesystems.com/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote aa) package-alist)))" \
	  --eval "(with-current-buffer (dired \"dist\") \
	            (package-install-from-buffer))"
