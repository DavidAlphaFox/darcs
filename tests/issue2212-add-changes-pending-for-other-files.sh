#!/usr/bin/env bash
## Test for issue2212 - darcs updates pending when add is called for a distinct
## file.
##
## Copyright (C) 2012 Owen Stephens
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

. lib

rm -rf R
darcs init --repo R

cd R

touch a b

darcs add a
darcs rec -am 'Add a'

rm a
darcs add b

# Darcs shouldn't have updated anything to do with (despite the fact that it
# has indeed been deleted.)
cat _darcs/patches/pending | not grep 'rmfile \./a'

# The same should be true for other commands that have to recompute pending
# from the working directory
darcs revert -a
touch b
darcs add b
rm a
darcs revert b -a
cat _darcs/patches/pending | not grep 'rmfile \./a'
