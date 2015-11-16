#!/usr/bin/env bash
## Test for patch index automation - <SYNOPSIS: Patch index should
## always be in sync with repo after execution of any command.>
##
## Copyright (C) 2012  BSRK Aditya
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

#pragma repo-format darcs-2

. lib
grep darcs-2 $HOME/.darcs/defaults || exit 200

rm -rf R S
darcs init --repo R
darcs init --repo S

cd R
touch f
darcs record -lam 'p1'

cd ../S
touch f
darcs record -lam 'p2'
darcs send -ao p2.dpatch ../R

cd ../R
darcs apply -a ../S/p2.dpatch 
darcs changes --verbose
darcs changes --verbose | grep -q 'duplicate'
darcs changes f --verbose | not grep -q 'duplicate'
