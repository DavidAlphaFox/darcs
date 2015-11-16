#!/usr/bin/env bash
## Test for issue2138 - whatsnew --summary does not show conflicts
##
## Copyright (C) 2012 Lele Gaifax
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

rm -rf R S

darcs init --repo R

cd R
echo 'Example content.' > f
darcs record -lam 'Add f'
cd ..

darcs get R S

# Create a deliberate conflict
cd R
echo "Conflict on side R" >> f
darcs record -am 'CR'

cd ../S
echo "Conflict on side S" >> f
darcs record -am 'CS'

darcs pull -a ../R

darcs whatsnew > out
cat out
grep "side R" out | wc -l | grep 1
grep "side S" out | wc -l | grep 1

darcs whatsnew --summary > out
grep "^M!" out | wc -l | grep 1

cd ..
rm -rf R S
