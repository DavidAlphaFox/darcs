#!/usr/bin/env bash
. lib                           # Load some portability helpers.

t=`(dd if=/dev/urandom count=1 | tr -cd "a-zA-Z0-9" | head -c 10)` # Create de tag name.
darcs init --repo R        # Create our test repos.

# Test about issue 2244: darcs tag should warn about duplicate tags.

cd R
darcs tag "$t"
darcs show tag | grep "$t"
darcs tag "$t" | grep 'WARNING'
cd ..
