#!/usr/bin/env bash
## Test for issue2234 - darcs doesn't ignore tags when rolling back, so tagged
## patches can't be selected. 
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

darcs init --repo R

cd R

echo -e "line1\nline2" > f1
cp f1 f2

darcs rec -alm 'Add files'

# Make a couple of changes to two files
sed -i -e 's/1/a/' f1
sed -i -e 's/2/b/' f2

darcs rec -am 'change 1'

# Make another change to one file
sed -i -e 's/1/A/' f2
darcs rec -am 'change 2'

darcs tag footag

# we'd like to cut down the presented patches, by giving a filename should ask
# us about the changes in the first patch
darcs rollback -p '1' f1 | not grep -i 'no patches selected'
