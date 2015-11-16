#!/usr/bin/env bash
## Test for issue2380 - darcs won't rename a file to a file that has been
## deleted in the working dir.
##
## Copyright (C) 2014 Owen Stephens
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
rm -rf R && darcs init --repo R

. lib

cd R
echo foostuff > foo
echo otherstuff > other
darcs rec -alm 'Add foo and other'

rm foo
darcs mv other foo

# foo should exist, with the correct contents, and other should not
[[ -e foo && ! -e other && $(<foo) == "otherstuff" ]]

echo -e 'hunk ./foo 1\n-foostuff\nrmfile ./foo\nmove ./other ./foo' > expected
darcs wh > actual

diff expected actual

darcs rev -a

[[ -e foo && -e other && $(<foo) == "foostuff" && $(<other) == "otherstuff" ]]
