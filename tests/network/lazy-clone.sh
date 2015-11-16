#!/usr/bin/env bash
set -ev

rm -rf temp temp2 temp3

darcs clone --lazy http://hub.darcs.net/kowey/tabular temp

darcs clone --lazy temp temp2

rm -rf temp

cd temp2

test ! -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2

darcs annotate -p 'Initial version'

test -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2

cd ..

rm -rf temp temp2 temp3
