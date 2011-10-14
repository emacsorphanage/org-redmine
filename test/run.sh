#!/bin/sh
EMACS=emacs

ORGMODE_DIR=~/.emacs.d/el-get/org-mode/
ANYTHING_DIR=~/.emacs.d/el-get/anything/
ELEXPECTATIONS_DIR=~/.emacs.d/el-get/el-expectations/
ELMOCK_DIR=~/.emacs.d/el-get/el-mock/

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
