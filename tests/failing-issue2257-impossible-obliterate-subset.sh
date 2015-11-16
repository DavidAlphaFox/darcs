#!/usr/bin/env bash
## Test for issue2257 - impossible case encountered when obliterating a subset
## of patches
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

rm -rf issue2257
mkdir issue2257
cd issue2257

darcs init --repo single-patch
darcs init --repo duplicate-patch

cd single-patch

mkdir dir
touch dir/file1
darcs record -alm 'dir and file1'

touch dir/file2
darcs record -alm 'addfile file2'

cd ../duplicate-patch

# Split the dir/file patch into the dir add, and then the file add.
darcs pull -a ../single-patch -p 'dir and file'
darcs unrecord -a

# Only record 'adddir ./dir'
echo yny | darcs record -m 'adddir dir' 
# Now record the addfile in a separate patch.
darcs record -am 'addfile file1'

# Pull in the other patches we don't have (which will include the original 
# "add dir/file1" patch again since we've amended it to no longer exist, and
# the "addfile file2" patch)
darcs pull -a ../single-patch

# Attempt to obliterate 'addir dir' (but not 'addfile file1'). This seems to be
# a problem with the patch selection, since without -p, we aren't able to
# obliterate 'adddir dir', if we say no to 'addfile file1' (which is
# sensible!).
echo nyy | darcs obliterate -p 'adddir dir'
