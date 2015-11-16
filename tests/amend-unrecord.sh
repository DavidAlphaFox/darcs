#!/usr/bin/env bash
## Test for amend --unrecord
##
## Copyright (C) 2012  Ganesh Sittampalam
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

. lib                  # Load some portability helpers.
darcs init      --repo R        # Create our test repo.

cd R

(echo x ; echo y) > foo
darcs add foo
darcs rec -am "add foo"

(echo 1 ; echo x ; echo y ; echo 2) > foo
darcs rec -am "insert 1 and 2"

(echo yyny) | darcs amend --unrecord
(echo x ; echo y ; echo 2) > foo.expected
darcs show contents foo | diff -q foo.expected -

(echo yenyy) | DARCS_EDITOR="sed -i -e s/2/2j/" darcs amend --unrecord
(echo x ; echo y ; echo 2j) > foo.expected
darcs show contents foo | diff -q foo.expected -

echo 'ugh' > bar
darcs add bar
# use amend to check it's still a short form for amend-record
# if we make amend-unrecord visible rather than hidden that would change
echo y | darcs amend -a
darcs show contents bar | diff -q bar -

# test that amend --unrecord --all and specifying files works
echo y | darcs amend --unrecord -a foo
(echo x ; echo y) > foo.expected
darcs show contents foo | diff -q foo.expected -
darcs show contents bar | diff -q bar -
