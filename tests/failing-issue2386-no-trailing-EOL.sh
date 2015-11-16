#!/usr/bin/env bash
## Test for issue2386 - no trailing EOF produces spurious diff output
##
## Copyright (C) 2014 Owen Stephens
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
darcs init --repo R
cd R

# foo will contain "1\n2\n3"
echo '0: 310a 320a 33' | xxd -r > foo

darcs add foo

# Bad output looked like:
#
# addfile ./foo
# hunk ./foo 1
# -
# +1
# +2
# +3
darcs wh | not grep '^-$'

darcs rec -am 'Add foo'

rm foo

# Bad output looked like:
# hunk ./foo 1
# -1
# -2
# -3
# +
# rmfile ./foo
darcs wh | not grep '^+$'
