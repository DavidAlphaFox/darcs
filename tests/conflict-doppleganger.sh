#!/usr/bin/env bash

#pragma repo-format darcs-1,darcs-2

. lib

# Tests for the doppleganger conflict bug.

# For Zooko, with love
# Also, for issue81.

check_conflict() {
cat out
if test "$format" = darcs-2; then
    not grep 'conflict' out
else
    grep 'conflict' out
fi
}

# check doppleganger conflicts
rm -rf tmp_dopple tmp_ganger
mkdir tmp_dopple
cd tmp_dopple
darcs init

touch a.txt
darcs add a.txt
darcs record -A base -am 'adding a.txt'
cd ..

darcs get tmp_dopple tmp_ganger

for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    cd $repo
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
check_conflict
cd ..

# Checking resolution dopplegangers conflicts
rm -rf temp0 temp1 temp2 tmp_dopple tmp_ganger
mkdir temp0
cd temp0
darcs init
cd ..

# Create a conflict
darcs get temp0 temp1
cd temp1
darcs show repo
echo temp1 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp1 a.txt'
cd ..
darcs get temp0 temp2
cd temp2
echo temp2 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp2 a.txt'
cd ..

# Resolve the conflict the same way on both sides
for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    darcs get temp1 $repo
    cd $repo
    darcs pull -a ../temp2
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
check_conflict
