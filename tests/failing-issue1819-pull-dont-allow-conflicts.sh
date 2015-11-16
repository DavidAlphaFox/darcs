#!/usr/bin/env bash
## Test for issue1819 - pull --dont-allow-conflicts doesn't work
##
## Dave Love <fx@gnu.org>, Public domain

. lib
rm -rf R S
for repo in R S; do
    darcs init --repo $repo
    cd $repo
    echo 'Example content.' >x
    darcs add x
    darcs record -lam 'Add x'
    echo $repo >x
    darcs record -lam 'Change x'
    cd ..
done

darcs get S S0
cd S0
# the 'echo |' is for the external merge prompt 'hit return to continue' prompt
echo | darcs pull --all --allow-conflicts --external-merge 'cp %2 %o' ../R
cd ..

darcs get S S0b
cd S0b
echo | not darcs pull --all --dont-allow-conflicts ../R
cd ..

darcs get S S1
cd S1
echo | not darcs pull --all --external-merge 'cp %2 %o' --dont-allow-conflicts ../R
cd ..

darcs get S S2
cd S2
echo | not darcs pull --all --dont-allow-conflicts --external-merge 'cp %2 %o' ../R
cd ..
