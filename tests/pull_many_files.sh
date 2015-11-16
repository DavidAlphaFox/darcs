#!/usr/bin/env bash
. lib

#pragma repo-format darcs-1,darcs-2

rm -rf temp1 temp2

if grep darcs-1 .darcs/defaults; then
format=hashed
elif grep darcs-2 .darcs/defaults; then
format=darcs-2
else format=ERROR; fi

mkdir temp2
cd temp2
gunzip -c $TESTDATA/many-files--${format}.tgz | tar xf -
cd ..

mkdir temp1
cd temp1
darcs init
darcs pull -a ../temp2/many-files--${format} > log
grep -i 'finished pulling' log
cd ..
rm -rf temp1

# put things back how we found them.

rm -rf temp1 temp2
