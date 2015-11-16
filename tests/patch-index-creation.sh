#!/usr/bin/env bash
## Test for patch index automation - Patch index should
## be recreated when running:
## * annotate on a file or directory
## * non-interative log on a file
## * init, clone and convert with flag --with-patch-index
##
## It should NOT be created when running:
## * annotate or log with --no-patch-index
## * init, clone and convert, without flags
##
## Copyright (C) 2012  BSRK Aditya, Ganesh Sittampalam
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

darcs init --repo R
cd R
darcs show patch-index-status | grep 'Patch Index is not yet created.' # init

cd ..
rm -rf R

darcs init --repo R --with-patch-index
cd R
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # init --patch-index

cd ..

darcs clone R S
cd S
darcs show patch-index-status | grep 'Patch Index is not yet created.' # clone

cd ..
rm -rf S

darcs clone R S --with-patch-index
cd S
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # clone --patch-index

cd ..

rm -rf repo repo2
gunzip -c $TESTDATA/simple-v1.tgz | tar xf -
echo 'I understand the consequences of my action' | darcs convert darcs-2 repo repo2 
cd repo2
darcs show patch-index-status | grep 'Patch Index is not yet created.' # convert

cd ..
rm -rf repo repo2

gunzip -c $TESTDATA/simple-v1.tgz | tar xf -
echo 'I understand the consequences of my action' | darcs convert darcs-2 repo repo2 --with-patch-index
cd repo2
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # convert --patch-index

cd ../R

mkdir d
echo 'New line.' >> d/f
darcs record -lam "Change d/f"
rm -rf _darcs/patch_index
darcs annotate d/f
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # annotate

rm -rf _darcs/patch_index
darcs annotate d/f --no-patch-index
darcs show patch-index-status | grep 'Patch Index is not yet created.' # annotate --no-patch-index

rm -rf _darcs/patch_index
darcs log -a d/f
darcs show patch-index-status | grep 'Patch Index is in sync with repo.' # log -a file

rm -rf _darcs/patch_index
darcs log -a d/f --no-patch-index
darcs show patch-index-status | grep 'Patch Index is not yet created.' # log -a file --no-patch-index

cd ..
rm -rf R S repo repo2
