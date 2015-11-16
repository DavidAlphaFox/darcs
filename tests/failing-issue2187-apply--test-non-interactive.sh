#!/usr/bin/env bash
## Test for issue2187 - "darcs apply --test <patch-set" crashes if 
## the test fails.
##
## Copyright (C) 2012 Ilya Perminov
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
darcs get R S

# Create a patch bundle
cd R
echo 'Example content.' >file1
darcs add file1
darcs rec -a --name patch1
darcs send --dont-edit-description --output=./patch1 -a ../S

# Setup a test that fails, apply the patch set, check for an unhandled exception.
cd ../S 
darcs setpref test false
not darcs apply --test <../R/patch1 > log 2>&1
not fgrep -q "illegal operation" log
