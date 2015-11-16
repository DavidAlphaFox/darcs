#!/usr/bin/env bash

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
    darcs send --author=me -a --no-edit-description -o ../R1/P 

    cd ..

    cd R1

    touch "r1_1"
    darcs add "r1_1"
    darcs record -a --author=me -m "Adding r1_1" "r1_1"
    
    cd ..
}

test_init

cd R1

darcs rebase pull -a --reorder ../R2
check_patches_order r1_1 r2_0 r1_0

cd ..
