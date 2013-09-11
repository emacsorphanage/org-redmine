CASK ?= cask
export EMACS ?= emacs

.PHONY: test

test: elpa
	${CASK} exec ${EMACS} -Q --batch \
			--load org-redmine.el \
			--load test/org-redmine-test.el \
			-f batch-expectations

elpa: Cask
	${CASK} install
	touch $@
