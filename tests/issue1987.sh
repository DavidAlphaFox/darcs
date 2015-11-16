#!/usr/bin/env bash
## Test for issue1987 - Garbage collection for inventories and patches
##
## Copyright (C) 2014  Marcio Diaz
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

# This script test that 'darcs optimize' cleans
# the directories _darcs/patches/ and _darcs/inventories/
# from unnecesary files.

. lib

# The script will read the following directories:
PATCHES_DIR='_darcs/patches/'
INV_DIR='_darcs/inventories/'
PRISTINE_DIR='_darcs/pristine.hashed/'

#################################################
# Testing garbage collection on _darcs/patches/ #
#################################################

rm -rf R

darcs init --repo R
cd R

touch f
darcs add f

# In the next line, 'ls -1' list files in a single column,
# so that $PENDING_FILES should looks like:
# pending
# pending.tentative
PENDING_FILES=$(ls -1 $PATCHES_DIR)
darcs record -am 'Add f.'

# After doing 'record', the contents of $PATCHES_DIR
# should looks like (the hash will be different):
# 0000000120-8462241e685a983cff956f05a6a32f3ff6b27a485e67d19b84b2b5fef31fab84
# pending
# pending.tentative
PATCHES_DIR_AFTER_RECORD=$(ls -1 $PATCHES_DIR)

# The following line gets the names of files added to $INV_DIR after doing
# record, i.e., the name of the new patch.
# Then $PATCH should looks like:
# 0000000120-8462241e685a983cff956f05a6a32f3ff6b27a485e67d19b84b2b5fef31fab84
PATCH=`comm -13 <(echo "$PENDING_FILES") <(echo "$PATCHES_DIR_AFTER_RECORD")`

touch g
darcs add g
echo y | darcs amend-record -a

# $PATCHES_DIR_AFTER_AMEND should looks like:
# 0000000120-8462241e685a983cff956f05a6a32f3ff6b27a485e67d19b84b2b5fef31fab84
# 0000000132-23ee2a79b097dd4a134c50d196c6f7ecd5c85655ad44d34f6c1732f4b491ca35
# pending
# pending.tentative
PATCHES_DIR_AFTER_AMEND=$(ls -1 $PATCHES_DIR)

# This is one of the important parts of the script. If issue1987 is
# correctly solved, 'darcs optimize' should delete the patch:
# 0000000120-8462241e685a983cff956f05a6a32f3ff6b27a485e67d19b84b2b5fef31fab84
darcs optimize clean

# Then $PATCHES_DIR_AFTER_OPTIMIZE should looks like:
# 0000000132-23ee2a79b097dd4a134c50d196c6f7ecd5c85655ad44d34f6c1732f4b491ca35
# pending
# pending.tentative
PATCHES_DIR_AFTER_OPTIMIZE=$(ls -1 $PATCHES_DIR)

# If issue1987 is solved $REMOVED_PATCH should looks like:
# 0000000120-8462241e685a983cff956f05a6a32f3ff6b27a485e67d19b84b2b5fef31fab84,
# i.e., must be equal to $PATCH.
# Otherwise $REMOVED_PATCH == '', and then $REMOVED_PATCH != $PATCH.
REMOVED_PATCH=`comm -13 <(echo "$PATCHES_DIR_AFTER_OPTIMIZE") \
						<(echo "$PATCHES_DIR_AFTER_AMEND")`

[ "$PATCH" == "$REMOVED_PATCH" ]

cd ..

#####################################################
# Testing garbage collection on _darcs/inventories/ #
#####################################################

rm -rf R

darcs init --repo R
cd R

touch f
darcs add f
darcs record -am 'Add f.'

# $INV_DIR_AFTER_RECORD should looks like:
# 0000000192-3863012ed1377e2d80e0f97bccbad0260d9e186bc28080549563f22d8b968e33
INV_DIR_AFTER_RECORD=$(ls -1 $INV_DIR)

touch g
darcs add g
darcs record -am 'Add g.'

# $INV_DIR_AFTER_SND_RECORD should looks like:
# 0000000192-3863012ed1377e2d80e0f97bccbad0260d9e186bc28080549563f22d8b968e33
# 0000000384-e5683733407c4aae642604adf29d582a8fbbb6c50a96d6e8bba20058f7892b68
INV_DIR_AFTER_SND_RECORD=$(ls -1 $INV_DIR)

# $SND_PATCH should looks like:
# 0000000384-e5683733407c4aae642604adf29d582a8fbbb6c50a96d6e8bba20058f7892b68
SND_PATCH=`comm -13 <(echo "$INV_DIR_AFTER_RECORD") \
					<(echo "$INV_DIR_AFTER_SND_RECORD")`

# We don't need any of the files in $INV_DIR (since we have not done
# 'darcs tag', all the information is in _darcs/hashed_inventory),
# therefore 'darcs optimize' can delete all the files in $INV_DIR.
darcs optimize clean

# $INV_DIR_AFTER_OPTIMIZE should be equal to ''.
INV_DIR_AFTER_OPTIMIZE=$(ls -1 $INV_DIR)

[ "$INV_DIR_AFTER_OPTIMIZE" == "" ]

cd ..

#####################################################
# Testing garbage collection on _darcs/inventories/ #
# (this time using 'darcs tag')
#####################################################

rm -rf R

darcs init --repo R
cd R

touch f
darcs add f
darcs record -am 'Add f.'

# $INV_DIR_AFTER_RECORD should looks like:
# 0000000192-3208a6a8d8b0a12f9f99c5c89529f9bf773553cd5e985cee7dd0221b8cfe5018
INV_DIR_AFTER_RECORD=$(ls -1 $INV_DIR)

darcs tag -m 'Add f.'

# $INV_DIR_AFTER_FST_TAG should looks like:
# 0000000192-3208a6a8d8b0a12f9f99c5c89529f9bf773553cd5e985cee7dd0221b8cfe5018
# 0000000297-eed3c68ee2d145f499f00d8367ec09a2cebdf79de7341e873e7bc68088236fc6
INV_DIR_AFTER_TAG=$(ls -1 $INV_DIR)

# $SND_PATCH should looks like:
# 0000000297-eed3c68ee2d145f499f00d8367ec09a2cebdf79de7341e873e7bc68088236fc6
SND_PATCH=`comm -13 <(echo "$INV_DIR_AFTER_RECORD") \
					<(echo "$INV_DIR_AFTER_TAG")`

touch g
darcs add g
darcs record -am 'Add g.'

# $INV_DIR_AFTER_SND_RECORD should looks like:
# 0000000192-3208a6a8d8b0a12f9f99c5c89529f9bf773553cd5e985cee7dd0221b8cfe5018
# 0000000297-eed3c68ee2d145f499f00d8367ec09a2cebdf79de7341e873e7bc68088236fc6
# 0000000489-6892c56cd7ec4381ad7d8bbcf10ed5a3366d3ac40f2a569d9bf7d2e5633fe32a
# The last file contains the data of the second so that the second file is
# useless.
# The latest file points to the first, so that 'optimize' should not delete
# the first file.
# The content of the last file is in _darcs/hashed_inventory
# so it can be deleted.
INV_DIR_AFTER_SND_RECORD=$(ls -1 $INV_DIR)

darcs optimize clean

# $INV_DIR_AFTER_OPTIMIZE should looks like:
# 0000000192-3208a6a8d8b0a12f9f99c5c89529f9bf773553cd5e985cee7dd0221b8cfe5018
INV_DIR_AFTER_OPTIMIZE=$(ls -1 $INV_DIR)

[ "$INV_DIR_AFTER_OPTIMIZE" == "$INV_DIR_AFTER_RECORD" ]

cd ..

#########################################################
# Testing garbage collection on _darcs/pristine.hashed/ #
# (this should work even before issue1987)              #
#########################################################

rm -rf R
darcs init --repo R
cd R

# $PRISTINE_DIR_AFTER_INIT should looks like:
# e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
PRISTINE_DIR_AFTER_INIT=$(ls -1 $PRISTINE_DIR)

echo "Hello darcs" > f
darcs add f
darcs record -am 'Hello darcs.'

# $PRISTINE_DIR_AFTER_RECORD should looks like:
# 34e7e68e2ba4d79facc7ddffdab700608d4abceebbc8ff98d77479b8a5127820
# a8c49fa16eaed0a0a601834c98132a32c24f078e58fcba4d9e036fadb92ca91d
# e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
# The third file represents the empty working copy and is no longer
# needed. Therefore 'optimize' must remove it.
PRISTINE_DIR_AFTER_RECORD=$(ls -1 $PRISTINE_DIR)

EXPECTED_PRISTINE_AFTER_OPTIMIZE=`comm -13 <(echo "$PRISTINE_DIR_AFTER_INIT") \
										   <(echo "$PRISTINE_DIR_AFTER_RECORD")`
darcs optimize clean

PRISTINE_DIR_AFTER_OPTIMIZE=$(ls -1 $PRISTINE_DIR)

[ "$PRISTINE_DIR_AFTER_OPTIMIZE" == "$EXPECTED_PRISTINE_AFTER_OPTIMIZE" ]

cd ..
rm -rf R