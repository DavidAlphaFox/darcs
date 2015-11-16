#!/usr/bin/env bash
## Test that we can repair incorrect adds
##
## Copyright (C) 2012 Ganesh Sittampalam
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

rm -rf bad
mkdir bad
cd bad
darcs init --no-patch-index

echo foo > file
darcs add file
mkdir dir
darcs add dir

darcs rec -a -m 'initial'

darcs changes --verbose --patches 'initial'

# produce a corrupt addfile patch
echo 'addfile ./file' > _darcs/patches/pending
echo 'yny' | darcs rec -m 're-add file'

not darcs check
darcs repair
darcs check

# produce a corrupt adddir patch
echo 'adddir ./dir' > _darcs/patches/pending
echo 'yy' | darcs rec -m 're-add dir'

not darcs check
darcs repair
darcs check
