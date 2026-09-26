EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q -batch -L . -L test -l test/test-helper.el -l test/test-denote-mcp.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q -batch -L . -L test -l test/test-helper.el --eval '(byte-compile-file "denote-mcp.el")'

clean:
	rm -f *.elc test/*.elc
