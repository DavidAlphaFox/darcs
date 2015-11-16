#!/usr/bin/env bash

## Test that apply --reorder moves to the top the uncommon set of patches between
## the current repository and the set of patches we are applying.

. lib                  # Load some portability helpers.

check_patches_order () {
    darcs changes | tr -d "\n" | grep $1.*$2.*$3
}

test_init () {
    rm -rf R1 R2
    
    darcs init "R1"
    cd R1

    touch "r1_0"
    darcs add "r1_0"
    darcs record -a --author=me -m "Adding r1_0" "r1_0"

    cd ..

    darcs clone "R1" "R2"

    cd R2

    touch "r2_0"
    darcs add "r2_0"
    darcs record -a --author=me -m "Adding r2_0" "r2_0"
    darcs send --no-minimize --author=me -a --no-edit-description -o ../R1/P 

    cd ..

    cd R1

    touch "r1_1"
    darcs add "r1_1"
    darcs record -a --author=me -m "Adding r1_1" "r1_1"
    
    cd ..
}


test_init

cd R1

darcs apply P
# Without reorder the expected order is r2_0, r1_1, r1_0 .
check_patches_order r2_0 r1_1 r1_0

# Test that apply --reorder reorders even if there is nothing to apply.
darcs apply --reorder P
check_patches_order r1_1 r2_0 r1_0

cd ..

test_init

cd R1

darcs apply --reorder P
# With reorder the expected order is r1_1, r2_0, r1_0 .
check_patches_order r1_1 r2_0 r1_0

cd ..
