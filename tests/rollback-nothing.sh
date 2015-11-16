#!/usr/bin/env bash

. ./lib

rm -rf temp
mkdir temp
cd temp
darcs init
date > file1
darcs add file1
darcs record -am "test"
rm file1
darcs record -am "rm"
echo yYd | tr [A-Z] [a-z] | darcs rollback --last=1 | grep 'No changes selected'
cd ..
rm -rf temp
