#!/usr/bin/env bash
## Test for issue1514: send --minimize
##
## Copyright (C) 2014 G. Hoffmann
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

. lib                           # Load some portability helpers.
darcs init      --repo SCREENED       # Create our test repos.

cd SCREENED
touch a
darcs rec -lam a
touch b
darcs rec -lam b
cd ..

darcs clone SCREENED LOCAL
darcs clone SCREENED REVIEWED

cd LOCAL
touch c
darcs rec -lam c
darcs push --all # so SCREENED has c

# now create a patch we'll send as bundle with full context
touch d
darcs rec -lam d
darcs send --all -o bundle --no-minimize

# check it cannot be applied to REVIEWED (which lacks patch c)
not darcs apply bundle --all --repodir=../REVIEWED

# now create bundle with minimal context (default option)
darcs send --all -o bundle.min

# this one lacks c in its context so it can be applied to REVIEWED
darcs apply bundle.min --all --repodir=../REVIEWED

cd ..

