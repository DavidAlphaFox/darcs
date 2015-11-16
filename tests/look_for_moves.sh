#!/usr/bin/env bash

. ./lib

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1

# simple add and move

darcs init
touch foo
darcs record -a -m add_file -A x --look-for-adds
mv foo foo2
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_file -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds  >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# simple add and move dir

darcs init
mkdir foo
darcs record -a -m add_dir -A x --look-for-adds
mv foo foo2
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_file -A x --look-for-moves
darcs wh --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# add, move and add same name

darcs init
touch foo
darcs record -a -m add_file -A x --look-for-adds
mv foo foo2
touch foo
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs wh --summary --look-for-moves --look-for-adds >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
a ./foo
EOF
darcs record -a -m move_file_add_file -A x --look-for-moves --look-for-adds
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# amend-record

darcs init
touch foo
darcs record -a -m add_file -A x --look-for-adds
mv foo foo2
echo 'yyy' | darcs amend-record -p add_file -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
darcs annotate --patch add_file | grep "] addfile ./foo2"
rm -rf *

# add, move, add same name and amend-record

darcs init
touch foo
darcs record -a -m add_file -A x --look-for-adds
mv foo foo2
touch foo
echo 'yyyy' | darcs amend-record -p add_file -A x --look-for-moves --look-for-adds
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
darcs annotate --patch add_file >log 2>&1
grep "addfile ./foo" log
grep "addfile ./foo2" log
rm -rf *

# add, move, amend-record, move, amend-record

darcs init
touch foo
darcs record -a -m add_file -A x --look-for-adds
mv foo foo2
echo 'yyy' | darcs amend-record -p add_file -A x --look-for-moves
mv foo2 foo
echo 'yyy' | darcs amend-record -p add_file -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# move dir with content

darcs init
touch foo # created before dir to get a lower inode
mkdir dir
mv foo dir
darcs record -a -m add_files -A x --look-for-adds
mv dir dir2
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./dir -> ./dir2
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_dir -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# name swapping -- unsupported thus ignored

darcs init
touch foo foo2
darcs record -lam add_file -A x
mv foo foo.tmp
mv foo2 foo
mv foo.tmp foo2
not darcs wh --look-for-moves
rm -rf *

# dir swapping -- ignored

darcs init
mkdir dir dir2
touch dir/foo dir2/foo2
darcs record -a -m add_files_and_dirs -A x --look-for-adds
mv dir dir.tmp
mv dir2 dir
mv dir.tmp dir2
not darcs wh --summary --look-for-moves
rm -rf *

# darcs mv before a plain mv

darcs init
touch foo
darcs record -a -m add_files_and_dirs -A x --look-for-adds
darcs mv foo foo2
mv foo2 foo3
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo3
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_dirs -A x --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# mv to a boring filename

darcs init
touch foo
darcs record -a -m add_files_and_dirs -A x --look-for-adds
mv foo foo~
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
R ./foo
EOF
diff -u log log.expected
rm -rf *

cd ..
rm -rf temp1
