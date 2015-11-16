#!/usr/bin/env bash
. lib

darcs init
mkdir a b
touch a/a b/b
darcs add --rec .
darcs record -a -m ab -A test
darcs annotate a/a
echo x > c
darcs add c
darcs record -a -m foo -A 'Mark Stosberg <a@b.com>'
darcs annotate c
darcs annotate c | grep "a@b.com"
cd ..
rm -rf temp
