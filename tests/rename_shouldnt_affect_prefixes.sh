#!/usr/bin/env bash
## Renaming a -> b should not affect any filenames with prefix b, when looking
## for the original name of the files in changes --xml, or when annotating.
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

echo -e 'a\nb\nc' > a
cp a bb
darcs rec -alm 'Add a bb'

darcs move a b
darcs rec -am 'Move a -> b'

darcs changes --xml b bb > changes.xml

grep "original_name='./a'" < changes.xml
# Ensure we've not used a prefix of the filename for the move.
not grep "original_name='./ab'" < changes.xml

# Ensure that we are able to annotate bb (if the rename has affected bb
# internally, we'll not be able to annotate the file)
darcs annotate bb | not grep unknown
