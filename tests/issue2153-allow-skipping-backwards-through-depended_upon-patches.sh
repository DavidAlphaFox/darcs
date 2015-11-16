#!/usr/bin/env bash

# Testing amend

. lib

rm -rf temp1
# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

# do some work here
cd temp1

touch foo
darcs add foo
echo -e 'line1\nline2\nline3' > foo
darcs record -a -m add_lines
echo -e 'line1ch\nline2\nline3' > foo
darcs record -a -m changedline1
echo -e 'line1ch\nline2changed\nline3' > foo
darcs record -a -m changed_line2
echo -e 'line1ch\nline2changedagain\nline3' > foo
darcs record -a -m changed_line2again

echo successmarker >> foo
echo jkya | darcs amend
echo FOOOOOO
darcs changes -v --patch "changed_line2again"|grep successmarker

cd ..

rm -rf temp1 temp2
