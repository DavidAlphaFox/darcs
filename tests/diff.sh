#!/usr/bin/env bash
. ./lib

rm -rf temp-$$
mkdir temp-$$
cd temp-$$

darcs initialize
echo text > afile.txt
darcs add afile.txt
darcs record --author me --all --no-test --name init
darcs diff
darcs diff --no-unified -p . --store-in-mem > diffinmem
darcs diff --no-unified -p . --no-store-in-mem > diffondisk
diff diffinmem diffondisk

echo text >> afile.txt
darcs diff | sed 's/afile\.txt.*//'> diffnoarg
darcs diff . | sed 's/afile\.txt.*//' > diffdot
diff diffnoarg diffdot

cd ..
rm -rf temp-$$
