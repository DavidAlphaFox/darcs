#!/bin/sh -e
##
## Basic test of rebase apply
##
## Copyright (C) 2011-2014 Ganesh Sittampalam
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

rm -rf R1 R2 R3
mkdir R1
cd R1
darcs init
echo '1' > foo
darcs rec -lam "add foo"
cd ..

darcs get R1 R2
cd R2
echo '2' > foo
darcs rec -am "2"
darcs send -a -o 2.dpatch ../R1
cd ..

darcs get R1 R3
cd R3
echo '3' > foo
darcs rec -am "3"

echo '4' > foo
darcs rec -am "4"

# TODO: figure out behaviour of --all and test it
# (should it answer 'yes' to both pulling and suspending?
echo yyy | darcs rebase apply -a ../R2/2.dpatch

darcs changes --count | grep "Rebase in progress: 2 suspended patches"
echo yny | darcs rebase unsuspend | grep "We have conflicts"
cat > expected <<EOF
v v v v v v v
1
=============
2
*************
3
^ ^ ^ ^ ^ ^ ^
EOF
diff -u expected foo

echo 23 > foo
echo yyy | darcs amend --patch '3'

echo yy | darcs rebase unsuspend | grep "We have conflicts"
cat > expected <<EOF
v v v v v v v
3
=============
23
*************
4
^ ^ ^ ^ ^ ^ ^
EOF
diff -u expected foo

echo 24 > foo
echo yyy | darcs amend --patch '4'

cd ..


