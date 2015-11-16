#!/usr/bin/env bash
## Test for issue2013 - darcs send --context --to shall not ask email address
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

mkdir temp2

cd temp2
darcs init
echo foo > a
darcs record -alm add_a -A x

# setup test
cd ..
darcs get temp2 temp1

cd temp1
darcs changes --context > context
touch foo bar
darcs add foo bar
darcs record -alm add_foo_bar -A x

# Test that --to works with send --context
darcs send --mail --author=me -a --to=random@random --sendmail-command='grep "^To: random@random$" %<' --context context

# Test that a default preference will NOT be used when no --to value is specified
echo "default@email" > ../temp2/_darcs/prefs/email
not darcs send --mail --author=me -a --sendmail-command='grep "^To: default@email$" %<' --context context

cd ..
