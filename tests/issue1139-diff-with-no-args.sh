#!/usr/bin/env bash
. ./lib

rm -rf temp-$$
mkdir temp-$$
cd temp-$$

darcs initialize
echo text > foo
darcs add foo
darcs record -am 'add foo'
echo newtext > foo
darcs wh

darcs diff --no-unified --store > out1
cat out1
grep text out1
grep foo out1

darcs diff --no-unified > out
cat out
grep text out
grep foo out

diff out out1

cd ..

rm -rf temp-$$
