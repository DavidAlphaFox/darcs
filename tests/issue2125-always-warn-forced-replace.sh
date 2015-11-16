#!/usr/bin/env bash
## Test for issue2125 - darcs should not warn about forced replaces, if nothing
## needed to be forced.
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

echo -e 'first line of file\nsecond line of file' > file
darcs rec -alm 'Add file'

# No occurences of 'entry' - shouldn't warn.
darcs replace line entry file | not grep surprised

# We have an occurrence of 'second' - should fail, but not change anything (no
# --force)
noForce=$(darcs replace first second file)
echo "$noForce" | grep 'Skipping file'
echo "$noForce" | not grep 'surprised'

# Make sure nothing changed...
whatsnew=$(darcs wh)
echo "$whatsnew" | grep 'replace \./file \[A-Za-z_0-9\] line entry'
[[ $(echo "$whatsnew" | wc -l) -eq 1 ]]

force=$(darcs replace --force first second file) 
echo "$force" | grep 'surprised'
darcs wh

! read -r -d '' EXPECTED <<'EOF'
hunk ./file 2
-second line of file
+first line of file
replace ./file [A-Za-z_0-9] first second
replace ./file [A-Za-z_0-9] line entry

EOF

echo "$EXPECTED" > expected_whatsnew.txt
darcs wh > actual_whatsnew.txt

# Ensure we have the right changes in working/pending.
diff expected_whatsnew.txt actual_whatsnew.txt

darcs rev -a

# Test that unrecorded changes require --force, too.
echo 'third line of file' >> file
workingChangeNoForce=$(darcs replace second third file)
echo "$workingChangeNoForce" | grep 'Skipping file'
