#!/usr/bin/env bash
## Test for issue2209 - Automatically detect replace patch.
##
## Copyright (C) 2013 Jose Neder
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib

rm -rf R
mkdir R
cd R
# simple full complete replace (record)
darcs init
echo "foo" > file
darcs record -al -m "add file"
echo "bar_longer" > file          # replace by token of different length
echo yy | darcs record --look-for-replaces -m "replace foo bar_longer file"
darcs changes --last 1 -v 2>&1 | tail -n +4  > log
cat > log.expected <<EOF
  * replace foo bar_longer file
    replace ./file [A-Za-z_0-9] foo bar_longer
EOF
diff -u log log.expected
rm -rf *

# simple full complete replace (amend-record)
darcs init
echo "foo" > file
darcs record -al -m "add file"
echo "bar" > file
echo yyy | darcs amend-record --look-for-replaces
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * add file
    addfile ./file
    hunk ./file 1
    +foo
    replace ./file [A-Za-z_0-9] foo bar
EOF
diff -u log log.expected
rm -rf *

# simple full complete replace (whatsnew)
darcs init
echo "foo" > file
darcs record -al -m "add file"
echo "bar" > file
darcs whatsnew --look-for-replaces 2>&1 > log
cat > log.expected <<EOF
replace ./file [A-Za-z_0-9] foo bar
EOF
diff -u log log.expected
rm -rf *

# partial replace (only some of the words/chunks replaced) (record)
darcs init
echo "foo foo" > file
darcs record -al -m "add file"
echo "bar foo" > file
echo yyy | darcs record --look-for-replaces -m "replace foo bar file"
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * replace foo bar file
    replace ./file [A-Za-z_0-9] foo bar
    hunk ./file 1
    -bar bar
    +bar foo
EOF
diff -u log log.expected
rm -rf *

# partial replace (only some of the words/chunks replaced) (amend-record)
darcs init
echo "foo foo" > file
darcs record -al -m "add file"
echo "bar foo" > file
echo yyyy | darcs amend-record --look-for-replaces
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * add file
    addfile ./file
    hunk ./file 1
    +foo foo
    replace ./file [A-Za-z_0-9] foo bar
    hunk ./file 1
    -bar bar
    +bar foo
EOF
diff -u log log.expected
rm -rf *

# partial replace (only some of the words/chunks replaced) (whatsnew)
darcs init
echo "foo foo" > file
darcs record -al -m "add file"
echo "bar foo" > file
darcs whatsnew --look-for-replaces > log
cat > log.expected <<EOF
replace ./file [A-Za-z_0-9] foo bar
hunk ./file 1
-bar bar
+bar foo
EOF
diff -u log log.expected
rm -rf *

# forced replace (the word is in the file) (record)
darcs init
cat > file <<EOF
foo
foo
bar
EOF
darcs record -al -m "add file"
cat > file <<EOF
bar
bar
bar
EOF
echo yyy | darcs record --look-for-replaces -m "replace foo bar file"
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * replace foo bar file
    hunk ./file 3
    -bar
    +foo
    replace ./file [A-Za-z_0-9] foo bar
EOF
diff -u log log.expected
rm -rf *

# forced replace (the word is in the file) (amend-record)
darcs init
cat > file <<EOF
foo
foo
bar
EOF
darcs record -al -m "add file" --ignore-times
cat > file <<EOF
foo
bar
bar
EOF
darcs record -a -m "change file" --ignore-times
cat > file <<EOF
bar
bar
bar
EOF
echo yyyy | darcs amend-record --look-for-replaces --ignore-times
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * change file
    hunk ./file 3
    -bar
    +foo
    replace ./file [A-Za-z_0-9] foo bar
EOF
diff -u log log.expected
rm -rf *

# forced replace (the word is in the file) (whatsnew)
darcs init
cat > file <<EOF
foo
foo
bar
EOF
darcs record -al -m "add file"
cat > file <<EOF
bar
bar
bar
EOF
darcs whatsnew --look-for-replaces 2>&1 > log
cat > log.expected <<EOF
hunk ./file 3
-bar
+foo
replace ./file [A-Za-z_0-9] foo bar
EOF
diff -u log log.expected
rm -rf *

# multiple replaces (record)
darcs init
cat > file <<EOF
foo
bar
ser
EOF
darcs record -al -m "add file"
cat > file <<EOF
ter
rur
kee
EOF
echo yyyy | darcs record --look-for-replaces -m "multiple replaces"
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * multiple replaces
    replace ./file [A-Za-z_0-9] bar rur
    replace ./file [A-Za-z_0-9] foo ter
    replace ./file [A-Za-z_0-9] ser kee
EOF
diff -u log log.expected
rm -rf *

# multiple replaces (amend-record)
darcs init
cat > file <<EOF
foo
bar
ser
EOF
darcs record -al -m "add file"
cat > file <<EOF
ter
rur
kee
EOF
echo yyyyy | darcs amend-record --look-for-replaces
darcs changes --last 1 -v 2>&1 | tail -n +4 | grep -v "^    {\|    }$" > log
cat > log.expected <<EOF
  * add file
    addfile ./file
    hunk ./file 1
    +foo
    +bar
    +ser
    replace ./file [A-Za-z_0-9] bar rur
    replace ./file [A-Za-z_0-9] foo ter
    replace ./file [A-Za-z_0-9] ser kee
EOF
diff -u log log.expected
rm -rf *

# multiple replaces (whatsnew)
darcs init
cat > file <<EOF
foo
bar
ser
EOF
darcs record -al -m "add file"
cat > file <<EOF
ter
rur
kee
EOF
darcs whatsnew --look-for-replaces 2>&1 > log
cat > log.expected <<EOF
replace ./file [A-Za-z_0-9] bar rur
replace ./file [A-Za-z_0-9] foo ter
replace ./file [A-Za-z_0-9] ser kee
EOF
diff -u log log.expected
rm -rf *

# conflicting replaces (whatsnew)
darcs init
cat > file <<EOF
foo
foo
EOF
darcs record -al -m "add file"
cat > file <<EOF
bar
ber
EOF
darcs whatsnew --look-for-replaces 2>&1 > log
cat > log.expected <<EOF
hunk ./file 1
-foo
-foo
+bar
+ber
EOF
diff -u log log.expected
rm -rf *

# same line replaces
darcs init
cat > file <<EOF
foo foo2 foo3
fee fee2 fee3
EOF
darcs record -al -m "add file"
cat > file <<EOF
bar bar2 bar3
bor bor2 bor3
EOF
darcs whatsnew --look-for-replaces 2>&1 > log
cat > log.expected <<EOF
replace ./file [A-Za-z_0-9] fee bor
replace ./file [A-Za-z_0-9] fee2 bor2
replace ./file [A-Za-z_0-9] fee3 bor3
replace ./file [A-Za-z_0-9] foo bar
replace ./file [A-Za-z_0-9] foo2 bar2
replace ./file [A-Za-z_0-9] foo3 bar3
EOF
diff -u log log.expected
rm -rf *
