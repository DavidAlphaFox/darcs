#!/usr/bin/env bash

. ./lib

# We'd just use `diff -x _darcs -r' if -x was portable.
diffx () {
    { find $1 -type f; find $2 -type f; } |
      sed  -e '/.*\/_darcs\//d' -e 's;^[^/]*;;' | grep -v darcs.tix | sort | uniq |
      {
	while read part; do
	    diff -c $1$part $2$part
	done
      }
}

makepristine () {
    rm -rf pristine
    mkdir pristine
    for i in `darcs show files --no-files --no-pending`; do
        echo mkdir -p pristine/$i;
        mkdir -p pristine/$i;
    done
    for i in `darcs show files --no-directories --no-pending`; do
        echo darcs show contents $i ">" pristine/$i;
        darcs show contents $i > pristine/$i;
        cat pristine/$i;
    done
}



mkdir temp1
cd temp1
darcs init --darcs-1
touch foo
darcs add foo
darcs rec -m t1 -a -A tester
echo 1 >> foo
darcs what -s | grep -v No\ changes
darcs what -l | grep -v No\ changes
darcs what -sl | grep -v No\ changes
makepristine
cd ..

darcs get temp1 temp2
cd temp2
darcs changes
makepristine
cd ..

darcs get temp1 temp3
cd temp3
darcs changes
cp _darcs/hashed_inventory inv
darcs optimize clean
diff -c inv _darcs/hashed_inventory
rm inv
makepristine
cd ..
cat temp3/pristine/foo

diffx temp2 temp3
diff -rc temp1/pristine temp3/pristine
diff -rc temp2/pristine temp3/pristine

cd temp1
darcs record -a -A tester -m t2
darcs push ../temp2 -a
darcs push ../temp3 -a
makepristine
cd ..

cd temp3
makepristine
cd ..
cd temp2
makepristine
cd ..

diffx temp2 temp3
diff -rc temp1/pristine temp3/pristine
diff -rc temp2/pristine temp3/pristine

cd temp1
date > foo
darcs record -a -A tester -m t3
makepristine
cd ../temp2
darcs pull -a
makepristine
cd ../temp3
darcs pull -a
darcs check
makepristine
cd ..

diffx temp2 temp3
diff -rc temp1/pristine temp3/pristine
diff -rc temp2/pristine temp3/pristine

cd temp1
darcs get . ../temp4
cd ..
cd temp4
makepristine
cd ..

diffx temp2 temp4
diff -rc temp2/pristine temp4/pristine

cd temp1
darcs tag -A tagger -m atag
darcs check
darcs optimize clean
darcs check
darcs changes | grep t1
cd ..

cd temp3
date > foobarpatch
darcs add foobarpatch
darcs record -a -A silly -m foobarpatch
darcs check
darcs optimize clean
darcs check
darcs pull -a ../temp1
darcs check
darcs optimize reorder
darcs check
grep 'Starting with inventory' _darcs/hashed_inventory
cd ..

cd temp1
darcs pull -a ../temp3
cd ..

diff -c temp1/_darcs/hashed_inventory temp3/_darcs/hashed_inventory

cd temp4
darcs pull -p foobarpatch -a ../temp3
darcs pull -a ../temp1
darcs optimize reorder
darcs check
darcs push ../temp1
cd ..

diff temp1/_darcs/hashed_inventory temp4/_darcs/hashed_inventory

darcs get temp1 temp5
cd temp5
darcs obliterate --last 3 -a
darcs pull ../temp1 -a
darcs obliterate --last 3 -a
darcs pull ../temp2 -a
darcs check
darcs obliterate --last 3 -a
darcs pull ../temp4 -a
cd ..

cd temp4
darcs obliterate --last 3 -a
darcs pull ../temp5 -a
cd ..

cd temp2
darcs obliterate --last 3 -a
darcs pull ../temp5 -a
cd ..

cd temp1
darcs obliterate --last 3 -a
darcs pull ../temp5 -a
cd ..

