#!/usr/bin/env bash
## Test for issue2382 - we can confuse darcs by moving a directory to where a
## file previously was.
##
## Copyright (C) 2014 Owen Stephens
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

function getRecordedChanges () {
darcs rec -am 1
# Ignore patch details and unindent - we should have the same contents as wh
darcs changes --last 1 -v | tail -n+5 | sed -e 's/^\s\+//' > $1
darcs unrecord --last 1 -a
}

function rmOutputFiles () {
    rm recoutput* expected* whoutput*
}

. lib
darcs init --repo R
cd R

# foo is recorded as a file
echo foo > foo
darcs rec -alm 'Add foo'

rm foo

darcs wh

# foo is now a dir in working, with a file within
mkdir foo
touch foo/bar

darcs wh > whoutput1
cat << EOF > expected1
hunk ./foo 1
-foo
rmfile ./foo
EOF

diff whoutput1 expected1

# Ensure recording everything isn't any different to asking whatsnew
getRecordedChanges recoutput1
diff recoutput1 expected1
rmOutputFiles

# To avoid the output file appearing in the output of wh -l
whl=$(darcs wh -l)
echo "$whl" > whoutput2
cat << EOF > expected2
R ./foo
a ./foo/
a ./foo/bar
EOF

diff whoutput2 expected2

darcs add foo

darcs wh > whoutput3
cat << EOF > expected3
hunk ./foo 1
-foo
rmfile ./foo
adddir ./foo
EOF

diff whoutput3 expected3
getRecordedChanges recoutput3
diff recoutput3 expected3
rmOutputFiles

darcs add foo/bar

darcs wh > whoutput4
cat << EOF > expected4
hunk ./foo 1
-foo
rmfile ./foo
adddir ./foo
addfile ./foo/bar
EOF

diff whoutput4 expected4
getRecordedChanges recoutput4
diff recoutput4 expected4

# Make sure we can remove the directory, without modifying working
darcs remove foo/bar
darcs remove foo

darcs add -r foo
darcs wh > whoutput4a
getRecordedChanges recoutput4a

diff whoutput4a expected4
diff recoutput4a expected4
rmOutputFiles

# Make sure foo is now recorded as a directory
darcs rec -alm 'Make foo a dir'

# Evil. Poor darcs, having to work all this out. We've made foo back into a
# file in working
rm -r foo
touch foo

darcs wh > whoutput5
cat << EOF > expected5
rmfile ./foo/bar
rmdir ./foo
EOF

diff whoutput5 expected5
getRecordedChanges recoutput5
diff recoutput5 expected5
rmOutputFiles

darcs rev -a

mkdir bar
darcs rec -alm 'Add bar dir'
rmdir bar

touch bar

darcs wh > whoutput6
cat << EOF > expected6
rmdir ./bar
EOF

diff whoutput6 expected6
getRecordedChanges recoutput6
diff recoutput6 expected6
rmOutputFiles

whl=$(darcs wh -l)
echo "$whl" > whoutput7

cat << EOF > expected7
R ./bar/
a ./bar
EOF

diff whoutput7 expected7

darcs add bar

darcs wh > whoutput8
cat << EOF > expected8
rmdir ./bar
addfile ./bar
EOF

diff whoutput8 expected8
getRecordedChanges recoutput8
diff recoutput8 expected8
rmOutputFiles
