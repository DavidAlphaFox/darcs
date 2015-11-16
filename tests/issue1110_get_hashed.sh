#!/usr/bin/env bash
. ./lib
rm -rf temp1 temp1-h

gunzip -c $TESTDATA/many-files--old-fashioned-inventory.tgz | tar xf -
mv many-files--old-fashioned-inventory temp1

darcs get temp1 temp1-h
test -e temp1-h/_darcs/hashed_inventory
rm -rf temp1 temp1-h
