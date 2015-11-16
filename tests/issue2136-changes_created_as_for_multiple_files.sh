#!/usr/bin/env bash

## Ensure changes --xml reports correct original filenames for multiple files.
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

rm -rf R

darcs init --repo R

cd R

mkdir tldir

touch tldir/f1
darcs rec -alm 'Add tldir/f1'
echo foo >> tldir/f1
darcs rec -am 'Modify tldir/f1'
darcs move tldir/f1 tldir/f2
darcs rec -am 'Move tldir/f1 -> tldir/f2'

touch f3
darcs rec -alm 'Add f3'
darcs move f3 f4
darcs rec -am 'Move f3 -> f4'
darcs move f4 f5
darcs rec -am 'Move f4 -> f5'
touch f6
darcs rec -alm 'Add non-changing file f6'

mkdir tldir/d1
darcs rec -alm 'Add tldir/d1'
darcs move tldir/d1 tldir/d2
darcs rec -am 'Move tldir/d1 -> tldir/d2'

mkdir d3
darcs rec -alm 'Add d3'
darcs move d3 d4
darcs rec -am 'Move d3 -> d4'
darcs move d4 d5
darcs rec -am 'Move d4 -> d5'

# Ensure all original names are reported, both forwards, and reversed.
xmlChanges=$(darcs changes --xml tldir/f2 f5 tldir/d2 d5 f6)
xmlChangesRev=$(darcs changes --reverse --xml tldir/f2 f5 tldir/d2 d5 f6)

# xmlChanges needs to be quoted everywhere, otherwise this hack to retrieve the
# 2 following lines won't work.
checkRename () {
    echo "$1" | grep "<created_as current_name='\./$2' original_name='\./$3'>" -C2 | tail -1 | grep "<name>$4</name>"
}

checkInXML () {
    checkRename "$1" "d5" "d3" "Add d3"
    checkRename "$1" "f5" "f3" "Add f3"
    checkRename "$1" "tldir/d2" "tldir/d1" "Add tldir/d1"
    checkRename "$1" "tldir/f2" "tldir/f1" "Add tldir/f1"
}

checkInXML "$xmlChanges"
checkInXML "$xmlChangesRev"

# But don't mention unchanged files.
echo "$xmlChanges" | not grep "<created_as[^>]*'\./f6'"
