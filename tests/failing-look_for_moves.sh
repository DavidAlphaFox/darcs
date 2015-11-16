#!/usr/bin/env bash

. ./lib

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1

# # exchange of dirs with contents and exchange filenames inside

darcs init
mkdir dir dir2
touch dir/foo dir2/foo dir2/foo2
darcs record -a -m add_files_and_dirs -A x --look-for-adds
mv dir dir.tmp
mv dir2 dir
mv dir.tmp dir2
mv dir/foo dir/foo.tmp
mv dir/foo2 dir/foo
mv dir/foo.tmp dir/foo2
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./dir -> ./dir.tmp~
 ./dir2 -> ./dir
 ./dir.tmp~ -> ./dir2
 ./dir/foo -> ./dir/foo.tmp~
 ./dir/foo2 -> ./dir/foo
 ./dir/foo.tmp~ -> ./dir/foo2
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_dirs -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *


cd ..
rm -rf temp1
