#!/usr/bin/env bash

## Attempting to apply a patch which depends on a missing tag should not cause
## darcs to die.
##
## Copyright (C) 2012 Owen Stephens
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

rm -rf R S T patch.dpatch

darcs init --repo R

cd R

# Setup a repo with a tagged patch, and another patch ontop, so we have a split
# inventory
touch file1
darcs rec -alm 'Add file1'
darcs tag -m 'file1 tag'
touch file2
darcs rec -alm 'Add file2'

# Take a copy of the repo at this point
darcs get . ../S

# Add the tag which we will fail on
darcs tag -m 'file2 tag'

# Take a copy with the tag
darcs get . ../T

# Add our patch which will depend only on the last tag.
echo 'file1' > file1
darcs rec -am 'file1 content'

# Create a patch bundle with the new patch (by sending against the repo we
# copied, with the last tag)
darcs send ../T -a -o ../patch.dpatch --no-edit-description

cd ../S

# Try to apply to the patch which depends on the missing tag (we expect darcs
# to fail gracefully here)
not darcs apply ../patch.dpatch &> apply_output.txt

# A best-attempt at ensuring darcs warns about the missing tag:
grep "tagged file2 tag" apply_output.txt
grep "FATAL: Cannot apply this bundle. We are missing the above patches." apply_output.txt
