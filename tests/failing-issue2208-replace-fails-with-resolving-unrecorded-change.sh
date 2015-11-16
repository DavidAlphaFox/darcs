#!/usr/bin/env bash
## Test for issue2208 - darcs shouldn't fail to replace if unrecorded changes
## would make the replace succeed
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

echo -e 'foo\nbar' > testing

darcs rec -alm 'Add testing file'

echo -e 'baz\nbar' > testing

# Darcs will complain here, since we've not recorded the fact that we've
# removed the occurrence of foo
darcs replace bar foo testing | not grep Skipping

# We can get around this by recording, then replacing and amending the patch...
darcs rec -am "I don't want to have to record this!"

darcs replace bar foo testing

echo y | darcs amend -a

# Check the workaround succeeded.
darcs changes --last 1 -v | grep 'replace.*bar.*foo'
