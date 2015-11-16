#!/usr/bin/env bash
## Test for issue2311 - 
##          posthook for 'get' should run in created repo
##
## Copyright (C) 2013 Sebastian Fischer
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

rm -rf repos
mkdir repos
cd repos
mkdir origin
cd origin
darcs init
cd ..

# The output of the posthook `pwd` should include "branch"
# if "branch" is passed as repo name
darcs get origin branch --posthook=pwd > out
head -n 1 out | grep branch

# The output of the posthook `pwd` should include "origin_0"
# if no name for the created repo is passed
darcs get origin --posthook=pwd > out
head -n 1 out | grep origin_0

cd ..
rm -rf repos
