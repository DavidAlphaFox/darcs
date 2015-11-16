#!/usr/bin/env bash
. ./lib

rm -rf temp
mkdir temp
cd temp
darcs init
mkdir a b
touch a/a b/b
darcs add --rec .
darcs record -a -m ab -A test
darcs annotate a/a
darcs annotate . > inner
# annotate --repodir=something '.' should work
cd ..
darcs annotate --repodir temp '.' > temp/outer
cd temp
diff inner outer

cd ..
rm -rf temp

