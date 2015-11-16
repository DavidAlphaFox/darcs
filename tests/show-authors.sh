#!/usr/bin/env bash
. ./lib

rm -rf R

darcs init --repo R
cd R

echo zig > foo
darcs add foo
darcs record -a -m add_foo -A x

echo zag >> foo
darcs record -a -m mod_foo -A y

echo bar > foo
darcs record -a -m mod2 -A y

darcs show authors > authors
grep "#1	2	y" authors
grep "#2	1	x" authors

head -1 authors | grep y

darcs show authors --verbose
[[ $(darcs show authors --verbose | grep y | wc -l) -eq 2 ]]
