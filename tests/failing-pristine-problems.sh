#!/usr/bin/env bash
## Check for correct behaviour with missing files in pristine
##
## Copyright (C) 2010 Ganesh Sittampalam
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

rm -rf foo
mkdir foo
cd foo
darcs init

echo 'wibble' > wibble
darcs rec -lam 'wibble'

darcs check

roothash=`darcs show pristine | grep ' ./$' | cut -d' ' -f1`
wibblehash=`darcs show pristine | grep ' wibble$' | cut -d' ' -f1`

rm _darcs/pristine.hashed/$roothash

not darcs check
not darcs check
# At the time of writing this test goes wrong at the line above
# I'm not 100% certain if the rest of it is right.
darcs repair | grep -v 'The repository is already consistent'
darcs check

rm _darcs/pristine.hashed/$wibblehash

not darcs check
not darcs check
darcs repair | grep -v 'The repository is already consistent'
darcs check
