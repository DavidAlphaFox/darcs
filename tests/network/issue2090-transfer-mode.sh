#!/usr/bin/env bash
## Test for darcs transfer-mode
##
## Copyright (C) 2012 Eric Kow
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

echo 'Comment this line out and run the script by hand'; exit 200

. $(dirname $0)/../lib
. $(dirname $0)/sshlib

# Clean up after previous remote runs
${SSH} ${REMOTE} "\
rm -rf ${REMOTE_DIR}; \
mkdir ${REMOTE_DIR}; \
"

# Set up a repo to test
darcs init --repo R
cd R
touch f g
darcs add f g
darcs record f g -a --ignore-times -m 'add some files' -A moi
darcs clone . $REMOTE:$REMOTE_DIR/R
cd ..

darcs clone $REMOTE:$REMOTE_DIR/R S --debug > log 2>&1
COUNT=$(grep -c 'darcs transfer-mode' log)
# with issue2090, this was 6!
test $COUNT -eq 1
cleanup
