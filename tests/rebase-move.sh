#!/usr/bin/env bash
##
## Check that rebase merges moves without conflicts
##
## Copyright (C) 2010 Ganesh Sittampalam
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
echo 'wibble' > wibble
darcs add wibble
darcs rec -am"wibble" --ignore-times
echo 'wobble' > wibble
darcs rec -am"wobble" --ignore-times

echo 'yd' | darcs rebase suspend
darcs mv wibble wobble
echo 'y' | darcs amend -a --patch 'wibble'
# there shouldn't be any conflicts
echo 'yy' | darcs rebase unsuspend # | not grep "We have conflicts"
not darcs wh



