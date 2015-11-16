#!/usr/bin/env bash

. lib

# Demonstrates issue385 and others
darcs log --repo=http://darcs.net GNUmakefile --last 300

# Test things mentioned in issue2461:

# no _darcs should remain
test ! -d _darcs
# go to a directory where we have no write access
cd /
# and try again (with less patches to fetch)
darcs log --repo=http://darcs.net GNUmakefile --last 3
# an absolute path should give an error
not darcs log --repo=http://darcs.net /GNUmakefile --last 3
