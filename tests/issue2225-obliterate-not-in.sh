#!/usr/bin/env bash
##
## Obliterate patches not in other repo(s)
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

. lib

for i in {1..4}
do
    rm -rf R$i
    darcs init --repo R$i
done

recFile() {
touch $1
darcs rec -lam "add $1"
}

cd R1

recFile file1
recFile file2
recFile file3
recFile file4
recFile file5

# Should fail, we haven't specified a repo, and we don't have a default repo
not darcs ob -a --not-in-remote

darcs push -a -p file2 --set-default ../R2
darcs push -a -p file3 ../R3
darcs push -a -p file4 ../R4

darcs ob -a --not-in-remote=../R3 --not-in-remote=../R4 --not-in-remote

# Confirm that the only patches that were deleted were file1 and file5
[[ -f file2 && -f file3 && -f file4 && ! -f file1 && ! -f file5 ]]
