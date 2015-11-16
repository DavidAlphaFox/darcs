#!/usr/bin/env bash
. ./lib

rm -rf temp
mkdir temp
cd temp
darcs init
echo 'record name' > _darcs/prefs/defaults # name requires an argument
echo 'ALL unified foobar' >> _darcs/prefs/defaults # unified takes no argument
darcs record && exit 1
darcs whats && exit 1
rm -rf temp
