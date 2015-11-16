#!/usr/bin/env bash
## Test for issue2270 - 
##          darcs changes should only show changes to relevant files
##
## Copyright (C) 2013 Sebastian Fischer
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

rm -rf R
mkdir R
cd R
darcs init

echo irrelevant > irrelevant
darcs add irrelevant
darcs record -am "recorded irrelevant file"

echo relevant-initial > relevant
echo other > other
darcs add relevant other
darcs record -am "recorded relevant and other file"


# --interactive --only-to-files should only show relevant
(echo y | darcs changes -i --only-to-files relevant > out) || true
cat out
not grep "addfile ./irrelevant" out
grep "addfile ./relevant" out
not grep "addfile ./other" out

# --verbose --only-to-files should only show relevant
(darcs changes -v --only-to-files relevant > out) || true
cat out
not grep "addfile ./irrelevant" out
grep "addfile ./relevant" out
not grep "addfile ./other" out

# --interactive should only show relevant and other
(echo yy | darcs changes -i relevant > out) || true
cat out
not grep "addfile ./irrelevant" out
grep "addfile ./relevant" out
grep "addfile ./other" out

# --verbose should only show relevant and other
(darcs changes -v relevant > out) || true
cat out
not grep "addfile ./irrelevant" out
grep "addfile ./relevant" out
grep "addfile ./other" out

# --interactive --only-to-files should show old name of moved
darcs move relevant renamed-relevant
darcs record -am "renamed relevant file"

(echo yy | darcs changes -i --only-to-files renamed-relevant > out) || true
cat out
grep "addfile ./relevant" out
not grep "addfile ./irrelevant" out
not grep "addfile ./other" out

# --verbose --only-to-files should show old name of moved
(darcs changes -v --only-to-files renamed-relevant > out) || true
cat out
grep "addfile ./relevant" out
not grep "addfile ./irrelevant" out
not grep "addfile ./other" out

# --interactive should show old name of moved
(echo yy | darcs changes -i renamed-relevant > out) || true
cat out
grep "addfile ./relevant" out
not grep "addfile ./irrelevant" out
grep "addfile ./other" out

# --verbose should show old name of moved
(darcs changes -v renamed-relevant > out) || true
cat out
grep "addfile ./relevant" out
not grep "addfile ./irrelevant" out
grep "addfile ./other" out

# adding a new file called 'relevant' :
# changes should now relate to that file and not to the original one
echo relevant-new > relevant
darcs add relevant
echo other-new > other
darcs record -am "recorded new add of relevant"

(echo yy | darcs changes -i --only-to-files relevant > out) || true
cat out
grep "addfile ./relevant" out
grep 'relevant-new' out
not grep 'other-new' out
not grep 'relevant-initial' out

(echo yy | darcs changes -v --only-to-files relevant > out) || true
cat out
grep "addfile ./relevant" out
grep 'relevant-new' out
not grep 'other-new' out
not grep 'relevant-initial' out

(echo yy | darcs changes -i relevant > out) || true
cat out
grep "addfile ./relevant" out
grep 'relevant-new' out
grep 'other-new' out
not grep 'relevant-initial' out

(echo yy | darcs changes -v relevant > out) || true
cat out
grep "addfile ./relevant" out
grep 'relevant-new' out
grep 'other-new' out
not grep 'relevant-initial' out

cd ..
rm -rf R
