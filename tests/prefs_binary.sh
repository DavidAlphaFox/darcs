#!/usr/bin/env bash

# Test that _darcs/prefs/binaries
. lib
rm -rf temp1

mkdir temp1
cd temp1
darcs init
mkdir d
touch d/t.t
darcs add d/t.t
darcs record -am "initial record"
echo 'some change' >> d/t.t
echo ny | darcs record --interactive > log
# pre-test: plain text files are not binary
not grep binary log
echo 'd/t' >>  _darcs/prefs/binaries
echo ny | darcs record --interactive > log
grep binary log
cd ..

rm -rf temp1
