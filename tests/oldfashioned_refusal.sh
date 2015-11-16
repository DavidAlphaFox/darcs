#!/usr/bin/env bash

## Test that darcs refuses to work on old-fashioned repositories
## for certain commands.
##
## Copyright (C) 2011 Guillaume Hoffmann
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

rm -rf old
gunzip -c $TESTDATA/many-files--old-fashioned-inventory.tgz | tar xf -
mv many-files--old-fashioned-inventory old

cd old
not darcs add
not darcs amend
not darcs annotate
not darcs apply
not darcs diff
not darcs dist
not darcs mark
not darcs move
not darcs pull
not darcs push
not darcs record
not darcs remove
not darcs repair
not darcs replace
not darcs revert
not darcs rollback
not darcs send
not darcs setpref
not darcs tag
not darcs test
not darcs unrecord
not darcs unrevert
