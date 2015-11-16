#!/usr/bin/env bash
## Test for issue1579 - the diff-opts parameter with multiple parameters 
## separated by space are passed like an one parameter to diff.
##
## Copyright (C) 2013 dixiecko
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
abort_windows                   # No diff command available

darcs init      --repo R        # Create our test repos.

cd R
echo 'Example content line 1.' > f
darcs record -lam 'Add f.'
echo 'Example content line 2.' >> f

# Darcs passes the parameters to diff like [ "-wpurNd -U 999" ]
# instead of [ "-wpurNd","-U","999" ]
darcs diff --diff-opts '-wpurNd -U 999' > result

# Darcs doesn't indicate the error in return error code,
# when diff command didn't work the result is empty.

if [ -z "$(cat result)" ]; then 
   exit 2
fi
