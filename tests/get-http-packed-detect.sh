#!/usr/bin/env bash
# 2011, by Petr Rockai, Guillaume Hoffmann, public domain

# Tets that darcs get --verbose reports getting a pack when there is one,
# and does not report when there is none or when --no-packs is passed.

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

# check that default behaviour is to get packs
darcs get $baseurl/many-files--${format} S --verbose |grep "Cloning packed basic repository"

# check that it does really not get packs when --no-packs is passed
rm -rf S
darcs get $baseurl/many-files--${format} S --no-packs --verbose  |not grep "Cloning packed basic repository"

# check that it does not clam getting packs when there are not
rm -rf S
rm -rf many-files--${format}/_darcs/packs/
darcs get $baseurl/many-files--${format} S --verbose |not grep "Cloning packed basic repository"
