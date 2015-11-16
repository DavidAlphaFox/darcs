#!/usr/bin/env bash
. ./lib

rm -rf temp-$$
mkdir temp-$$
cd temp-$$

darcs initialize
echo text > foo
darcs add foo
darcs rec -am 'add foo'
darcs annotate -p .

echo newtext > foo
darcs record -am 'modify foo'

darcs diff --no-unified --store-in-mem --last=1 > out1
cat out1
grep text out1
grep foo out1

darcs diff --no-unified --last=1 > out
cat out
grep text out
grep foo out

diff -u out1 out

cd ..

rm -rf temp-$$
