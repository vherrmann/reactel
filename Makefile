EMACS ?= emacs
# Directory containing dash.el (needed by examples and their tests).
DASH_DIR ?= $(firstword $(wildcard /usr/share/emacs/site-lisp/elpa-src/dash-*) $(wildcard $(HOME)/.emacs.d/elpa/dash-*))
LOAD = -L . -L examples $(if $(DASH_DIR),-L $(DASH_DIR))
.PHONY: test test-strict test-compiled lint compile indent bench
test:
	$(EMACS) -Q --batch $(LOAD) -l ert -l test/reactel-test.el -l test/lint-test.el -l test/strict-test.el -l test/compile-test.el -l test/indent-test.el -l test/backend-test.el -l test/buffer-test.el -l test/examples-test.el -f ert-run-tests-batch-and-exit
compile:
	@rm -f *.elc examples/*.elc
	@$(EMACS) -Q --batch $(LOAD) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile reactel-lint.el reactel-compile.el reactel.el reactel-node.el reactel-c.el reactel-buffer.el reactel-markdown.el examples/*.el; status=$$?; rm -f *.elc examples/*.elc; exit $$status

# The library tests again with the runtime checks of the Rules of React on.
# (`purity' is left out: several tests count renders, which a double render
# would double; test/strict-test.el covers it.)
# The whole suite with every component compiled (automatic memoization on),
# alone and together with the strict runtime checks.
test-compiled:
	$(EMACS) -Q --batch $(LOAD) -l ert --eval "(setq reactel-compile t)" -l test/reactel-test.el -l test/lint-test.el -l test/strict-test.el -l test/compile-test.el -l test/indent-test.el -l test/backend-test.el -l test/buffer-test.el -l test/examples-test.el -f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch $(LOAD) -l ert --eval "(setq reactel-compile t reactel-strict '(mutation render-update buffer))" -l test/reactel-test.el -l test/backend-test.el -l test/buffer-test.el -l test/examples-test.el -f ert-run-tests-batch-and-exit

test-strict:
	$(EMACS) -Q --batch $(LOAD) -l ert -l test/reactel-test.el -l test/backend-test.el -l test/buffer-test.el -l test/examples-test.el --eval "(setq reactel-strict '(mutation render-update buffer))" -f ert-run-tests-batch-and-exit

# Static check of the Rules of Hooks over the project's own sources.
lint:
	$(EMACS) -Q --batch $(LOAD) -l reactel -l dash -f reactel-lint-batch reactel.el reactel-compile.el reactel-node.el reactel-c.el reactel-buffer.el $(wildcard examples/*.el)

# Reindent all sources with Emacs's rules (components indent their children by two columns).
indent:
	$(EMACS) -Q --batch $(LOAD) -l scripts/reindent.el

# Repaint benchmark of the buffer backend, byte-compiled (scripts/bench.el).
bench:
	@rm -f *.elc
	@$(EMACS) -Q --batch $(LOAD) -f batch-byte-compile reactel.el reactel-node.el reactel-c.el reactel-buffer.el 2>/dev/null; \
	$(EMACS) -Q --batch $(LOAD) -l scripts/bench.el -f reactel-bench-run; status=$$?; rm -f *.elc; exit $$status
