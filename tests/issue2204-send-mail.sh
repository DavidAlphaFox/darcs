#!/usr/bin/env bash
## Test for issue2204 - darcs send --mail vs --output
##
## Copyright (C) 2012 Eric Kow
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

# copied from issue2186
# Create a script that will be used instead of sendmail. It simply saves its
# input to file "message".
cat <<FAKE > dummy-sendmail.hs
import System.IO
main = hGetContents stdin >>= writeFile "message"
FAKE
ghc -o dummy-sendmail --make dummy-sendmail.hs

cd R
echo 'foo@example.com' > _darcs/prefs/email
cd ..

darcs get R S
cd S
echo 'send sendmail-command ../dummy-sendmail' > _darcs/prefs/defaults
echo 'Example content.' > f
darcs add f
darcs record -lam p

# no options (should generate a bundle)
darcs send -a
[ ! -e message ]
[ -e p.dpatch ]

# just --mail
darcs send -a --mail
[ -e message ]
rm message

# both mail and output
darcs send -a --mail -O
[ ! -e message ]
[ -e p.dpatch ]

cd ..
