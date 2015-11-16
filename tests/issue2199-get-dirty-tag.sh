#!/usr/bin/env bash
## Test for issue2199 - "darcs get --tag" gets too much if the tag is dirty.
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

darcs init --repo R

cd R
echo 'wibble' > file
darcs rec -lam 'wibble'
echo 'wobble' > file
darcs rec -lam 'wobble'
cd ..

darcs get R R-temp

cd R-temp
darcs unpull --patch 'wobble' -a
darcs tag 'wibble'
cd ..

cd R
darcs pull ../R-temp -a
cd ..

darcs get --tag wibble R R-tag
cd R-tag
darcs changes | not grep wobble
