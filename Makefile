TARGET = org-redmine.elc
SRC    = org-redmine.el

TEST_DIR = test
TEST_SRC = $(wildcard $(TEST_DIR)/*.el)
TEST_OBJ = $(TEST_SRC:.el=.elc)

EMACS=emacs

.PHONY: clean test

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -f batch-byte-compile $<

all: $(TARGET)

test: 
	cd $(TEST_DIR) && sh run.sh

clean:
	$(RM) -f *.elc $(TEST_DIR)/*.elc
