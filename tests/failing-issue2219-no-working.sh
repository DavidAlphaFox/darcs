#!/usr/bin/env bash
## Test for issue2219 - the --no-working-dir flag
##
### Copyright (C) 2011 Eric Kow 
##
## Permission is hereby granted, free of charge, to any person o
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

# getting a --no-working-dir repo should not create any of the wd files
mkdir R1
cd R1
darcs init
echo "howdy" > a
darcs add a
darcs record -a -m 'a file' a
cd ..
darcs get --no-working-dir R1 R2
test   -e R1/a
test ! -e R2/a

# absence of a working dir should not result in a pull conflict
# or a diff for that matter
## 2011-12-27: darcs failed:  error opening _darcs/pristine.hashed/7cb4df6851fc86f51ee33ff490a85c9f4f5cc31f7c7d9977c50942453a1a94a9
darcs get R1 R1b
cd R1b
echo "more stuff" > a
darcs record -a -m 'modify a'
cd ../R2
darcs pull ../R1b -a
# NB: not sure if this test expresses what I really intend
DIFFCOUNT=$(darcs diff | wc -l)
test [ $DIFFCOUNT -eq 0 ]

# 2011-12-27: unclear on what should happen here
mkdir S1
cd S1
darcs init --no-working-dir
echo "bonjour" > a
darcs add a
darcs record -a -m 'a file' a
cd ..

# --no-working-dir --working-dir flags should trump each other in the
# same fashion as the rest of darcs
#
## 2011-12-27: fails
darcs get --no-working-dir --with-working-dir R1 R3
test   -e R3/a
darcs get --with-working-dir --no-working-dir R1 R4
test ! -e R4
