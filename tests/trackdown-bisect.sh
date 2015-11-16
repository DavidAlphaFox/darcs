#!/bin/env bash
# A test for test --linear, test --bisect and test --backoff.
# In general it construct various repositories and try
# to find the last recent failing patch and match it with
# expected position.
################################################################

. ./lib

if echo $OS | grep -i windows; then
    echo I do not know how to run a test program under windows
    exit 0
fi

ghc -o trackdown-bisect-helper $TESTBIN/trackdown-bisect-helper.hs

function make_repo_with_test {
    rm -fr temp1
    mkdir temp1 ; cd temp1 ; darcs init
    touch ./i
    touch ./j
    darcs add ./i
    darcs add ./j
    ../trackdown-bisect-helper $1
}

function cleanup_repo_after {
    cd ..
    rm -fr temp1
}

# You can replace --bisect by --linear for compare with linear trackdown
test_args='--bisect' 

# Function return true if given patch was found.
# It expects that last line has finish with <SPACE><patchname>
# For the linear it is second last from the end, and last line
# is sentence if test failed or succeed.
function is_found_good_patch  {
    if [ "$test_args" == "--linear" ]; then
    tail -n 2 | grep " $1\$"
    else 
    tail -n 1 | grep " $1\$"
    fi
}

# Test command - Success condition is that file 'j' have one inside (1)
# That means if it has zero (0) it is failing test. 
test_cmd='grep -q 1 j'

#############################################################################
# Section with test-cases
#############################################################################

# TEST01: Repo with success in the half
testTrackdown() {
make_repo_with_test $1
if darcs test $test_args "$test_cmd" | is_found_good_patch $2; then
    echo "ok 1"
else
    echo "not ok 1. the trackdown should find last failing patch = $2."
    exit 1
fi
cleanup_repo_after
}

# TEST01: Repo with success in the half
test01() {
testTrackdown '[1,1,0,0,0]' 3
}

# TEST02: Repo without success condition
test02() { 
testTrackdown '[0,0,0,0,0]' 1
}

# TEST03: Repo with success condition at before last patch
test03() {
testTrackdown '[1,1,1,1,0]' 5
}

# TEST04: Repo with success condition as first patch ever
test04() {
testTrackdown '[1,0,0,0,0]' 2
}

#############################################
# call test-cases for linear trackdown
#############################################
test_args='--linear'
test01
test02
#############################################
# Call test-cases for bisect trackdown
#############################################
test_args='--bisect' 
test01
test02
test03
test04
#############################################
# Call test-cases for backoff trackdown
#############################################
test_args='--backoff'
test01
test02
test03
test04
