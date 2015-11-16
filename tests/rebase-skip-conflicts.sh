#!/bin/sh -e
##
## Test of rebase unsuspend --skip-conflicts
##
## Copyright (C) 2011 Ganesh Sittampalam
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
mkdir R
cd R
darcs init

cat > foo <<EOF
1
2
3
4
5
EOF
darcs rec -lam "initial foo"

cat > foo <<EOF
1
2A
3
4
5
EOF
darcs rec -am "change 2A"

cat > foo <<EOF
1
2A
3
4A
5
EOF
darcs rec -am "change 4A"

echo 'yyd' | darcs rebase suspend

cat > foo <<EOF
1
2B
3
4
5
EOF
darcs rec -am "change 2B"

echo 'yy' | darcs rebase unsuspend --skip-conflicts

cat > foo-expected <<EOF
1
2B
3
4A
5
EOF
diff -u foo-expected foo