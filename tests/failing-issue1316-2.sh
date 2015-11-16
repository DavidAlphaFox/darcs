#!/usr/bin/env bash
## Test for issue1316 - junk left in pending
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

# this test is a slightly more complicated variant on failing-issue1316
# it also uses darcs commands to test rather than grepping pending.

. lib                           # Load some portability helpers.
darcs init      --repo R        # Create our test repos.

cd R

touch X
darcs rec -lam 'X'

darcs mv X Y
darcs rec -am 'Y'

rm Y
echo 'y' | darcs amend-rec -a

# the bug is that addfile Y shows up in pending
# if pending is empty as it should be, then since
# whatsnew doesn't have -l, it shouldn't report anything
# even after we touch Y.
touch Y
not darcs whatsnew

