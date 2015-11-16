#!/bin/sh

. ./lib

not () { "$@" && exit 1 || :; }

rm -rf temp
mkdir temp
cd temp
darcs init
not darcs add a/ 2> err
cat err
grep 'File a does not exist!' err
cd ..

rm -rf temp
