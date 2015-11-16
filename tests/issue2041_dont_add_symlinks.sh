#!/usr/bin/env bash
## Test for issue1645 - Since Darcs does not version-contol symlinks,
## it should reject them when a user is trying to 'darcs add' them.
## Thist must hold for both file and directory symlinks.
##
## Copyright (C) 2011 Alexey Levan
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
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repo.

darcs --version

cd R

unset pwd # Since this test is pretty much linux-specific, hspwd.hs is not needed
abort_windows # and skip if we are on win32...

# test for file
touch test-file

ln -s ./test-file ./test-file-link1                # relative symlink
ln -s "`pwd`"/test-file ./test-file-link2          # absolute symlink
not darcs add test-file-link1       # darcs must fail to add symlinks
not darcs add test-file-link2

# test for directory
mkdir test-dir

ln -s ./test-dir ./test-dir-link1
ln -s "`pwd`"/test-dir ./test-dir-link2
not darcs add test-dir-link1
not darcs add test-dir-link2
