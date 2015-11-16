#!/usr/bin/env bash
## Test for patch index self-healing after used non-patch-index-enabled darcs
## Copyright (C) 2012  Mark Stosberg
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
darcs init --repo R --with-patch-index
cd R

#Record some example content
echo 'Example content.' > f
darcs record -lam 'create file.'

# Simulate use by an older darcs binary, by backup in the index and disabling patch index.
mv _darcs/patch_index _darcs/patch_index.bak
touch _darcs/no_patch_index

# Now record Some new changes.
echo 'Example content updated.' > f
echo 'Example content updated.' > f2
darcs record -lam 'Update by older darcs.'

# Now return the repo to the state that the patch-index-enabled darcs would see it.
rm _darcs/no_patch_index
mv _darcs/patch_index.bak _darcs/patch_index

# Now, perform an action with the this darcs. ( and make sure we see the changes  from the older darcs. ) 
darcs changes f | grep 'older darcs'

# Now, check: Did the patch-index self heal?
darcs show patch-index-status | grep 'Patch Index is in sync with repo.'

# Another check, just to be sure.
echo 'Example content updated again.' > f
darcs record -lam 'Another update by newer darcs'

# Still good?
darcs show patch-index-status | grep 'Patch Index is in sync with repo.'

# Clean up.
cd ..
rm -rf R
