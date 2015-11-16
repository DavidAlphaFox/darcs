#!/usr/bin/env bash
## Test for patch index automation - <SYNOPSIS: Patch index should
## always be in sync with repo after execution of any command.>
##
## Copyright (C) 2012  BSRK Aditya
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
rm -rf R S
darcs init --repo R --with-patch-index
darcs init --repo S --with-patch-index
cd R
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test init

mkdir d e
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test record
echo 'New line.' >> d/f
echo "y" | darcs amend-record -p 'Add d/f and e.' -a
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test amend-record

darcs push -a ../S
cd ../S
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test push

echo 'Changed Content' > d/f
darcs record -am 'Change d/f'
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test record 2
darcs send -ao test.dpatch  ../R

cd ..
darcs clone R T --with-patch-index
cd T
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test clone

darcs pull -a ../S
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test pull
darcs obliterate -p 'Add d/f and e.' -a
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test obliterate

cd ../R
darcs apply -a ../S/test.dpatch
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test apply
darcs unrecord -a -p 'Change d/f'
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test unrecord
darcs tag -m 'tag R'
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # test tag

cd ..
rm -rf R S T
