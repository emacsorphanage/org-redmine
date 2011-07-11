#!/bin/sh
EMACS=emacs

ORGMODE_DIR=~/.emacs.d/lib/org-mode/
ANYTHING_DIR=~/.emacs.d/lib/auto-install-el

OPTIONS="-L .. -L . -L $ORGMODE_DIR -L $ANYTHING_DIR"
OUTPUT=aaa

$EMACS -q --no-site-file --batch $OPTIONS \
    -l org-redmine \
    -l el-expectations \
    -f batch-expectations $OUTPUT org-redmine-test.el
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
