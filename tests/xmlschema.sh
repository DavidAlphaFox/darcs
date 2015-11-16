#!/usr/bin/env bash
## Test for XML schema - <SYNOPSIS: Verify that darcs output is valid
## against the XSD and vice versa>
##
## Copyright (C) 2011  Radoslav Dorcik <dixiecko@gmail.com>
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

repod="$TESTDATA/../../"
xsdf="$repod/contrib/darcs.xsd"
tmpf="changes_tmp.xml"

if [ -z "$(which xmllint)" ]; then
    echo this test does not work on this machine
    echo it needs xmllint for XML validation.
    exit 200
fi

if [ ! -d "$repod/_darcs" ]; then
    echo this test does not work on this machine
    echo it needs source repo
    exit 200
fi

if [ ! -f "$xsdf" ]; then
    echo this test does not work on this machine
    echo it needs $(basename $xsdf)
    exit 200
fi

#######################################################
# Test preparation 
#######################################################

# Create repository with all kind of operations
darcs init      --repo R        # Create our test repos.
cd R

# Add File and Dir
mkdir dir1 dir2
echo "Example content." > dir1/file1
echo "Example content." > dir2/file2
darcs record -lam 'Add dir1 and dir2'
# Modify (add)
echo "Example content." >> dir1/file1
echo "Example content." >> dir2/file2
echo "Example content." >> dir1/file1
echo "Example content." >> dir2/file2
darcs record -lam 'Modify dir1 and dir2 (add)'
# Modify (add,del)
echo "Example content." > dir1/file1
echo "Example content." > dir2/file2
echo "Example contentx." >> dir1/file1
echo "Example contentx." >> dir2/file2
darcs record -lam 'Modify dir1 and dir2 (add,del)'
# Replace 
darcs replace contentx contenty dir1/file1 dir2/file2
darcs record -lam 'Replace contentx contenty'
# Modify (del)
echo "Example content." > dir1/file1
echo "Example content." > dir2/file2
darcs record -lam 'Modify dir1 and dir2 (del)'
# Moving
darcs mv dir1 dir11
darcs mv dir2 dir22
darcs record -lam 'Move dir1 and dir2'
# Remove
rm -fr dir11
rm -fr dir22
darcs record -lam 'Remove dir1 and dir2'
# Complex patch Replace and Modify, Add and Delete
mkdir dir3 dir4
echo "Example contentx." >> dir3/file3
echo "Example contentx." >> dir3/file3
echo "Example contentx." >> dir3/file3
echo "Example contentz." >> dir3/file3
echo "Example contentx." >> dir4/file4
darcs record -lam 'Complex patch (prep)'
darcs replace contentx contenty dir3/file3
mkdir dir5
echo "Example contentx." >> dir5/file4
darcs mv dir3 dir7
echo "Example contentx." >> dir7/file3
darcs record -lam 'Complex patch (done)'
rm -fr dir4

cd ..

#######################################################
# Testing 
#######################################################
# Darcs produces XMLs with non-UTF8 characters. 
echo '<?xml version="1.0" encoding="ISO-8859-1"?>'      > $tmpf
darcs log -s --xml-output --repodir=R                  >> $tmpf
xmllint --noout --schema $xsdf $tmpf || exit 1

echo '<?xml version="1.0" encoding="ISO-8859-1"?>'      > $tmpf
darcs log --xml-output --repodir=R                     >> $tmpf
xmllint --noout --schema $xsdf $tmpf || exit 1

exit 0; # Comment this out for next long running tests 

## Next tests are disabled by default because they are long running (~10 minutes)
## But they are be helpfull for manual verifications
echo '<?xml version="1.0" encoding="ISO-8859-1"?>'      > $tmpf
darcs log --xml-output -s --repodir=$repod              >> $tmpf
xmllint --noout --schema $xsdf $tmpf || exit 1
