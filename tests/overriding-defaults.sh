#!/usr/bin/env bash

. lib

rm -rf temp
mkdir temp
cd temp
darcs init
darcs setpref test false
darcs record --no-test -a -m 'add failing test'

# should fail when test is run
not darcs test

# should pass with --no-test in defaults
echo record --no-test > _darcs/prefs/defaults
touch a
darcs record -alm a
touch b
not darcs record --test -alm b

# should fail with --test in defaults
echo record --test > _darcs/prefs/defaults
touch c
not darcs record -alm c
touch d
darcs record --no-test -alm d

# check global defaults
cp ~/.darcs/defaults defaults.backup
trap "cp defaults.backup ~/.darcs/defaults" EXIT
rm _darcs/prefs/defaults

# --no-test works in global defaults
echo record --no-test > ~/.darcs/defaults
touch e
darcs record -alm e
touch f
not darcs record --test -alm f

# --test works in global defaults
echo record --test > ~/.darcs/defaults
touch g
not darcs record -alm g
touch h
darcs record --no-test -alm h

# Verify that per-repository defaults override global defaults

# --no-test in repository defaults overrides global --test
echo record --test > ~/.darcs/defaults
echo record --no-test > _darcs/prefs/defaults
touch i
darcs record -alm i
touch j
not darcs record --test -alm j

# --test in repository defaults overrides global --no-test
echo record --no-test > ~/.darcs/defaults
echo record --test > _darcs/prefs/defaults
touch k
not darcs record -alm k
touch l
darcs record --no-test -alm l

trap - EXIT
cp defaults.backup ~/.darcs/defaults

cd ..
rm -rf temp
