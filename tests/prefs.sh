#!/usr/bin/env bash
. ./lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs initialize
echo ALL ignore-times >> _darcs/prefs/defaults
cp _darcs/prefs/boring .boring
darcs add .boring
darcs setpref boringfile .boring
darcs record -a -m p1 -A me
cd ..
darcs get temp1 temp2
cmp temp1/_darcs/prefs/prefs temp2/_darcs/prefs/prefs
rm -rf temp1 temp2
