#!/usr/bin/env bash
## Test for issue2287 - Obliterate overwrites the existing file 
## when the parameter -O is used.
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
darcs init      --repo R        # Create our test repos.

cd R
echo 'Example content 1.' > f
darcs record -lam 'Add f.'
echo 'Example content for obliterate.' >> f
darcs record -am 'content obliterate'

# Create 'existing useful dpatch'
p='content-obliterate.dpatch'
p1='content-obliterate_0.dpatch'
touch $p

# This command should finish with error, since file already exists.
not darcs obliterate -a -p 'content obliterate' -o $p

if [ ! -z "$(cat $p)" ]; then
   exit 2
fi

# This command create autogenerate non-conflict file
darcs obliterate -a -p 'content obliterate' -O

# Assert file was not overwritten
if [ ! -z "$(cat $p)" ]; then
   exit 2
fi

# Assert new file was created
if [ ! -f $p1 ]; then
   exit 2
fi
