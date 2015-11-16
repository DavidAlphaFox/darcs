#!/usr/bin/env bash
## Test that shows failing commute of conflictors/duplicates.
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

# We want to create this situation:
#
# Context: [adddir "./dir1", addfile "./file1.txt"]
#
# (ParTree
#     (ParTree (DP "./dir1" RmDir)
#              (Move "./file1.txt" "./dir1/file3.txt")
#     (SeqTree (DP "./dir1" RmDir) (Move "./file1.txt" "./file2.txt")))
#
# And then create the merged tree consisting of merging the second "branch" of
# a par tree into the first, depth-first.
#
# Finally, we show that commute is broken for certain combinations of
# conflictors/duplicates, since we can commute out one way, but not the
# other. We do so, by creating both orderings of the final two patches, and
# show that only in one case can we obliterate the penultimate patch alone (due
# to non-commutation).
#
# See issue2047 on the BTS for more information.

rm -rf R1* R2 R3*

darcs init --repo R1
cd R1

mkdir dir1
touch file1.txt

darcs rec -alm 'Init'

darcs get . ../R2
darcs get . ../R3

rmdir dir1
darcs rec -am 'Remove dir1'

cd ../R2
darcs mv file1.txt dir1/file3.txt
darcs rec -am 'Move 1 -> 3'

# Create the first merged ParTree.
cd ../R1
darcs pull -a ../R2

# Revert conflict-markup
darcs rev -a

# Create a copy of R1, so we can show the effect of commuting out the final two
# patches, when we create a different ordered R3...
darcs get . ../R1_OTHER

cd ../R3

# Copy R3, so we can create the other ordering for its patches.
darcs get . ../R3_OTHER

#### In R3 we do rmdir; move file
rmdir dir1
darcs rec -alm 'Rmdir'

cd ../R1
darcs pull -a ../R3
darcs rev -a

cd ../R3
darcs mv file1.txt file2.txt
darcs rec -alm 'Move 1 -> 2'

cd ../R1
darcs pull -a ../R3
darcs rev -a

#### In R3_OTHER we do move file; rmdir
cd ../R3_OTHER
darcs mv file1.txt file2.txt
darcs rec -alm 'Move 1 -> 2'

cd ../R1_OTHER
darcs pull -a ../R3_OTHER
darcs rev -a

cd ../R3_OTHER
rmdir dir1
darcs rec -alm 'Rmdir'

cd ../R1_OTHER
darcs pull -a ../R3_OTHER
darcs rev -a

# Now, to show the bug, we can ob the penultimate patch from R1, but not
# R1_OTHER
cd ../R1
darcs ob -p 'Rmdir' -a

[[ $(darcs changes --count) -eq 4 ]]

cd ../R1_OTHER
echo y | darcs ob -p 'Move 1 -> 2' -a

# There should be 4 changes remaining, but due to the failure to commute, we'll
# actually obliterate 2 patches, leaving 3.
[[ $(darcs changes --count) -eq 4 ]]
