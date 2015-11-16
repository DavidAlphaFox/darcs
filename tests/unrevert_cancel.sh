#!/usr/bin/env bash

# From issue366 bug report

. ./lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch a 
touch b
darcs add *
darcs record -A moi -am init
echo plim >> a
echo plim >> b
echo yyyy | darcs revert
echo ploum >> a 
echo nyyy | darcs unrevert

cd ..
rm -rf temp1
