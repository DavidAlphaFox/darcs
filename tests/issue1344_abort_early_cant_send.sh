#!/usr/bin/env bash
## Test for issue1344 - abort early darcs send if sendmail is not
## available
##
## Copyright (C) 2010 Gabriel Kerneis
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

. ./lib

# The mail sending code on Windows uses the MAPI API unconditionally
# so this test fails.
# If it's possible to discover in advance whether mail sending would work,
# the code and this test could be improved to do that.
abort_windows

# Skip this test if sendmail is available
if which sendmail ; then
  echo "Sendmail found (in path), skipping test."
  exit 200
fi
if [ -f "/usr/sbin/sendmail" -o -f "/sbin/sendmail" -o \
  -f "/usr/lib/sendmail" ]; then
  echo "Sendmail found, skipping test."
  exit 200
fi

DARCS_EDITOR=echo
export DARCS_EDITOR

mkdir temp1 temp2

cd temp2
darcs init

# setup test
cd ../temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x

# If --sendmail-command is provided, no warning
darcs send --author=me -a --to=random@random --sendmail-command='true' ../temp2
# If --dry-run is provided, no warning
darcs send --author=me -a --to=random@random --dry-run ../temp2
# If -o or -O is provided, no warning
darcs send --author=me -a --to=random@random -O ../temp2
darcs send --author=me -a --to=random@random -o test.patch ../temp2
# Otherwise, fail early
(darcs send --author=me -a --to=random@random ../temp2 || true) | grep "No working sendmail"

cd ..
