#!/usr/bin/env bash
## Test for issue2447 - get contents of deleted file 
##
## Copyright (C) 2015  Ben Franksen
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
darcs init      --repo R        # Create our test repos.

cd R

echo 'example content' > f
darcs add f
darcs record -am 'add f'
hash1=$(darcs log --last=1 | grep '^patch' | cut -d ' ' -f 2)
rm f
darcs record -am 'removed f'
darcs show contents --hash $hash1 f | grep 'example content'

mkdir d
echo 'example content' > d/f
darcs add d/f
darcs record -am 'add d/f'
hash2=$(darcs log --last=1 | grep '^patch' | cut -d ' ' -f 2)
rm d/f
darcs record -am 'removed d/f'
darcs show contents --hash $hash2 d/f | grep 'example content'

darcs obliterate -a --last=1

rm -rf d
darcs record -am 'removed d and d/f'
darcs show contents --hash $hash2 d/f | grep 'example content'
