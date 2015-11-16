#!/usr/bin/env bash
. lib

rm -rf temp && mkdir temp

cd temp
darcs init
echo "one line" > test_file.txt
for i in {1..18}
	do cat test_file.txt test_file.txt > test_file2.txt && mv -f test_file2.txt test_file.txt
done
echo -n "last line without newline" >> test_file.txt

darcs add test_file.txt
darcs wh -l | grep "A ./test_file.txt"

cd .. && rm -rf temp