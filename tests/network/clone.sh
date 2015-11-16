#!/usr/bin/env bash
set -ev

rm -rf temp temp2 temp3

#"$DARCS" clone http://hub.darcs.net/kowey/tabular temp

darcs clone --lazy http://hub.darcs.net/kowey/tabular temp2

darcs clone --lazy --tag . http://hub.darcs.net/kowey/tabular temp3

cd temp2
darcs obliterate --from-tag . -a
darcs pull --tag . -a
cd ..

diff -u temp2/_darcs/hashed_inventory temp3/_darcs/hashed_inventory

rm -rf temp temp2 temp3
