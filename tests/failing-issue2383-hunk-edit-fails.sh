#!/bin/bash
## Test for issue2383 hunk-edit/last-regrets being able to put darcs into a
## state that it can't apply the recorded patch
##
## Copyright (C) 2013  Owen Stephens
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

. lib                           # Load some portability helpers.

rm -rf R
darcs init --repo R

cd R

cat << EOF > file
-- a comment

data D = C Int
       deriving (Eq, Show)
EOF

darcs rec -alm 'Add file'


cat << EOF > file
-- another comment

-- a comment

data D = C !Int
       deriving (Eq)
EOF

# Massive hack!
cat << EOF > change
========================== BEFORE (reference) ==========================
data D = C Int
       deriving (Eq, Show)
============================= AFTER (edit) =============================
data D = C !Int
       deriving (Eq, Show)
============================= (edit above) =============================
EOF

# Shell script "editor" that makes the change
cat << EOF > foo
#!/bin/bash
cp change darcs-patch-edit-0
EOF
chmod +x foo

export DARCS_EDITOR='./foo'

# Don't pick the first comment, do initially pick the data type change, then at
# last regrets, go back (k) and edit the hunk (e), before accepting (y) the new
# hunk change (keeping the Show instance).
echo nykeyy | darcs rec -m 'Add strictness annotation to C Int'
