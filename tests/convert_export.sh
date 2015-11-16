#!/usr/bin/env bash
## test incremental fast-export to git
##
## Copyright (C) 2014 Guillaume Hoffmann
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

# only run if git present
git --version | grep -i "git version"   || exit 200

darcs init      --repo R        # Create our test repos.
cd R
mkdir d e                       # Change the working tree.
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'

git init gitmirror

darcs convert export --write-marks darcs-to-git.marks > fex
(cd gitmirror && git fast-import --export-marks=git.marks < ../fex)

darcs mv d/f e/
darcs record -am 'Move d/f to e/f.'

darcs convert export --read-marks darcs-to-git.marks --write-marks darcs-to-git.marks > fex2
(cd gitmirror && git fast-import --import-marks=git.marks --export-marks=git.marks < ../fex2)

# restore the git working tree by making a clone
git clone gitmirror gitmirror-clone

diff e/f gitmirror-clone/e/f
