#!/bin/bash
echo 'Comment this line out and run the script by hand'; exit 200

. $(dirname $0)/../lib
. $(dirname $0)/sshlib

# ================ Setting up remote repositories ===============
${SSH} ${REMOTE} "\
rm -rf ${REMOTE_DIR}; \
mkdir ${REMOTE_DIR}; \
cd ${REMOTE_DIR}; \
\
mkdir testrepo; cd testrepo; \
darcs init; \
echo moi > _darcs/prefs/author; \
touch a; darcs add a; darcs record a --ignore-times -am 'add file a'; \
echo 'first line' > a; darcs record a --ignore-times -am 'add first line to a'; \
cd ..; \
\
darcs clone testrepo testrepo-pull; \
cd testrepo-pull; \
echo moi > _darcs/prefs/author; \
touch b; darcs add b; darcs record b --ignore-times -am 'add file b'; \
echo 'other line' > b; darcs record b --ignore-times -am 'add other line to b'; \
cd ..; \
\
darcs clone testrepo testrepo-push; \
darcs clone testrepo testrepo-send; \
"

# ================ Settings ===============
echo ${DARCS_SSH_FLAGS}
echo ${DARCS_SSH}
echo ${DARCS_SCP}
echo ${DARCS_SFTP}

# ================ Checking darcs clone ==================
darcs clone ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo ${DARCS_SSH_FLAGS}
# check that the test repo made it over
[ -d testrepo ]
[ -d testrepo/_darcs ]
[ -f testrepo/a ]

# if the above test is disabled we just init a blank repo
# so that the other tests can continue
if [ ! -d testrepo ]; then
  mkdir testrepo
  cd testrepo
  darcs init
  cd ..
fi

# ================ Checking darcs pull =================
darcs clone ${DARCS_SSH_FLAGS} testrepo testrepo-pull
cd testrepo-pull
echo yyy | darcs pull ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-pull
# see if the changes got pulled over
grep "other line" b

cd ..

# ================ Checking darcs push and send ================="
darcs clone ${DARCS_SSH_FLAGS} testrepo testrepo-push
cd testrepo-push
echo moi > _darcs/prefs/author
echo "second line" >> a; darcs record a --ignore-times -am "add second line to a"
touch c; darcs add c
darcs record --ignore-times -am "add file c" c
echo yyy | darcs push ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-push
# check that the file c got pushed over
${SSH} ${REMOTE} "[ -f ${REMOTE_DIR}/testrepo-push/c ]"
echo yyy | darcs send --no-edit-description ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-send -o mybundle.dpatch
# check that the bundle was created
grep "add file c" mybundle.dpatch
cd ..

# ================ Checking darcs clone to ssh destination =================="
cd testrepo
darcs clone . ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-clone
# check that the clone was successful
${SSH} ${REMOTE} "[ -d ${REMOTE_DIR}/testrepo-clone/_darcs ]"
${SSH} ${REMOTE} "[ -f ${REMOTE_DIR}/testrepo-clone/a ]"
cd ..

# ======== Checking push over ssh with a conflict ========="
${SSH} ${REMOTE} "echo apply no-allow-conflicts >> ${REMOTE_DIR}/testrepo-clone/_darcs/prefs/defaults"

cd testrepo 
echo moi > _darcs/prefs/author
echo 'change for remote' > a
darcs record --ignore-times -am 'change for remote'
darcs push -a
darcs ob --last 1 -a
echo 'change for local' > a
darcs record --ignore-times -am 'change for local'
darcs push -a > log 2>&1 || :
grep -q 'conflicts options to apply' log

cd ..
cleanup
