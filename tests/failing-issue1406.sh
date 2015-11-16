#!/usr/bin/env bash
## Test for issue1406 - failed test on amend unrecords the
##                      original patch
##
## Copyright (C) 2009 Adam Vogt, Kamil Dworakowski
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
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S
mkdir R/d/ R/e/                 # Change the working tree.
echo 'Example content.' >R/d/f
darcs record    --repo R -lam 'Add d/f and e.'
darcs mv        --repo R d/f e/
darcs record    --repo R -am 'Move d/f to e/f.'
darcs push      --repo R S -a   # Try to push patches between repos.
darcs push      --repo S R
rm -rf R/ S/                    # Clean up after ourselves.
#!/bin/sh
. ./lib

rm -rf temp1
darcs init --repodir temp1

cd temp1

echo "test exit 1" > _darcs/prefs/prefs
echo "a" > a
darcs record --look-for-adds --no-test --all --name=p1
echo "b" >> a
echo "y" | not darcs amend-record --all --patch=p1

# There should be one patch in the repo
test 1 -eq `darcs changes -a --count` || exit 1

# Another check: there should be nothing new after a is restored
echo "a" > a
not darcs whatsnew -l

cd ..
rm -rf test1

