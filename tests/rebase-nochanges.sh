#!/bin/sh -e
##
## Test that having a rebase in progress doesn't impact
## the UI of other darcs commands.
##
## At the time of writing, rebase was represented by
## a special internal patch and so this test is specifically
## aimed at checked that this patch is not presented in the UI
## Even if this changes, it is still worth having tests of
## the general principle that normal commands should still
## work as usual.
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
echo 'wibble' > wibble
darcs rec -lam "add wibble"

echo 'yy' | darcs rebase suspend

darcs changes --count | grep 0
darcs unpull --dry-run | grep "No patches selected!"

echo 'wobble' > wobble
darcs rec -lam "add wobble" -A tester

darcs changes --count | grep 1
cat > expected <<EOF
  * add wobble

Making no changes: this is a dry run.
EOF
darcs unpull --dry-run | tail -n+5 | grep -v tester | diff -u expected -


echo 'yy' | darcs rebase unsuspend | grep "Rebase finished"
test -f wibble
