#!/usr/bin/env bash
## Test for issue2012 - darcs send -o shall not print a "will be sent
## to" line
##
## Copyright (C) 2010 Gabriel Kerneis
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

. ./lib

DARCS_EDITOR=echo
export DARCS_EDITOR

mkdir temp1 temp2

cd temp2
darcs init
echo "default@email" > _darcs/prefs/email
cd ..

cd temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x

darcs send -a -o test.patch ../temp2 2>&1 | not grep  "Patch bundle will be sent to: default@email"
darcs send -a -O ../temp2 | not grep  "Patch bundle will be sent to: default@email"

cd ..
