#!/bin/sh
EMACS=emacs

ORGMODE_DIR=~/.emacs.d/lib/org-mode/
ANYTHING_DIR=~/.emacs.d/lib/auto-install-el
ELEXPECTATIONS_DIR=~/.emacs.d/lib/auto-install-el
ELMOCK_DIR=~/.emacs.d/lib/auto-install-el

OPTIONS="-L .. -L . -L $ORGMODE_DIR -L $ANYTHING_DIR -L $ELEXPECTATIONS_DIR -L $ELMOCK_DIR"
OUTPUT=aaa

$EMACS -q --no-site-file --batch $OPTIONS \
    -l org-redmine \
    -l el-expectations \
    -f batch-expectations $OUTPUT org-redmine-test.el
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
