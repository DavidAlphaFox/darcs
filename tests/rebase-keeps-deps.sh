#!/usr/bin/env bash
##
## Check that explicit dependencies are preserved during rebase
##
## Copyright (C) 2011 Ganesh Sittampalam
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

. lib                  # Load some portability helpers.

rm -rf t1
mkdir t1
cd t1
darcs init

echo 'A' > A
darcs add A
darcs rec -am"A" --ignore-times

echo 'B' > B
darcs add B
darcs rec -am"B" --ignore-times

echo 'C' > C
darcs add C
echo 'yyy' | darcs rec -am"C" --ignore-times --ask-deps

echo 'yydy' | darcs rebase suspend

echo 'yyy' | darcs rebase unsuspend
darcs unpull --patch 'A' --no-deps | grep "No patches selected"
darcs unpull --patch 'B' --no-deps | grep "No patches selected"

# repeat the test with a tag
darcs unpull --patch 'C' -a
darcs tag T

# two stages because suspend stops at clean tags
# might need to change back to one stage if this changes
echo 'yy' | darcs rebase suspend
echo 'yd' | darcs rebase suspend

echo 'yyy' | darcs rebase unsuspend
# this offers 'T', seems to be a bug in darcs unpull --no-deps
# darcs unpull --patch 'A' --no-deps | grep "No patches selected"
darcs unpull --match 'not name T' | grep "No patches selected"
darcs unpull --patch 'B' --no-deps | grep "No patches selected"