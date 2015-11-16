#!/usr/bin/env bash
# Written in 2010 by Petr Rockai, placed in public domain

#pragma repo-format darcs-1,darcs-2

. lib

rm -rf S

if grep darcs-1 .darcs/defaults; then
format=hashed
elif grep darcs-2 .darcs/defaults; then
format=darcs-2
else format=ERROR; fi

gunzip -c $TESTDATA/many-files--${format}.tgz | tar xf -

cd many*

darcs optimize http
test -e _darcs/packs/basic.tar.gz
test -e _darcs/packs/patches.tar.gz
cd ..

serve_http # sets baseurl
darcs get --packs $baseurl/many-files--${format} S
cd S
rm _darcs/prefs/sources # avoid any further contact with the original repository
darcs check
