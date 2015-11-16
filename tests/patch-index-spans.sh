#!/usr/bin/env bash
## Test for patch index integrity of spans - <SYNOPSIS: Test correctness
## of fid_map, fp_map>
##
## Copyright (C) YEAR  AUTHOR
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
darcs init --repo R --with-patch-index # Create our test repos.

cd R
mkdir d e                       # Change the working tree.
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'
darcs show patch-index-all > pi

grep -F "./d -> 1#./d" pi
grep -F "./d/f -> 1#./d/f" pi
grep -F "./e -> 1#./e" pi
grep -F "1#./d -> ./d" pi
grep -F "1#./d/f -> ./d/f" pi
grep -F "1#./e -> ./e" pi

darcs mv d/f e/
darcs record -am 'Move d/f to e/f.'
darcs show patch-index-all > pi

grep -F "./d -> 1#./d" pi
grep -F "./d/f -> 1#./d/f" pi
grep -F "./e -> 1#./e" pi
grep -F "./e/f -> 1#./d/f" pi
grep -F "1#./d -> ./d" pi
grep -F "1#./d/f -> ./e/f" pi
grep -F "1#./d/f -> ./d/f" pi
grep -F "1#./e -> ./e" pi

echo 'File 2' > d/f
darcs record -lam 'Add d/f'
darcs show patch-index-all > pi

grep -F "./d -> 1#./d" pi
grep -F "./d/f -> 2#./d/f" pi
grep -F "./d/f -> 1#./d/f" pi
grep -F "./e -> 1#./e" pi
grep -F "./e/f -> 1#./d/f" pi
grep -F "1#./d -> ./d" pi
grep -F "1#./d/f -> ./e/f" pi
grep -F "1#./d/f -> ./d/f" pi
grep -F "2#./d/f -> ./d/f" pi
grep -F "1#./e -> ./e" pi
