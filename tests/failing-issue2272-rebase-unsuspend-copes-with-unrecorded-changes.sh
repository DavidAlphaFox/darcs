#!/usr/bin/env bash
## Test for issue2272 - darcs rebase unsuspend should cope with unrecorded changes.
##
## Copyright (C) 2013 Mark Stosberg
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
touch t.txt
darcs add t.txt
darcs record -am 'initial record' t.txt

echo 'original line' > t.txt
darcs record -am 'adding original line' t.txt

# Now make an unrecorded change that's unrelated.
touch 2.txt
darcs add 2.txt

# Suspend the initial patch
darcs rebase suspend -a -p 'adding original line'

# Now, unsuspend that patch. It should succeed despite unrecorded changes being present.
darcs rebase unsuspend -a

# Additional case: An unrecorded change that conflicts with the suspended patch
# This should succeed, but leave conflict markers.
echo 'modified line' >t.txt
darcs rebase unsuspend -a

cd ../
