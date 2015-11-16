#!/usr/bin/env bash

. lib

DIR="`pwd`"

# set up the repository
rm -rf temp1                    # another script may have left a mess.
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
echo ny | darcs record
cd ..
rm -rf temp1
