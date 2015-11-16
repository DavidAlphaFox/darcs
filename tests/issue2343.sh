#!/usr/bin/env bash
## Test for issue2343 - 'darcs amend-record does not record my change'
##
## Copyright (C) 2013  José Neder
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

. ./lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
cat > file << FOO
{
    foo(foovar);

}

FOO
# The issue happens when the last line not common was a "boring" line. The diff
# algorithm was wrongly checking one line above and therefore it wasn't working
# right.
darcs record -m 'add' --all --look-for-adds
cat > file << FOO
{
    foo(foovar2);
}

FOO
# Here the last line in common is "}". The empty line between "foo(foovar;)""
# and "}" is checked and since it is a "boring" line the last line number is
# incremented, but the line "foo(foovar2);" isn't so the last line number in
# the "newfile" is not incremented and so it makes a bad diff later.
# is important to make a different line from the top
# "foo(foobar);" -> "foo(foovar2);" because if not it will only be an deleted
# line and the algorithm will skip the check of boring lines.
darcs wh >log 2>&1
cat > log.expected <<EOF
hunk ./file 2
-    foo(foovar);
-
+    foo(foovar2);
EOF
diff -u log log.expected
cd ..
rm -rf temp1