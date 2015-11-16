#!/usr/bin/env bash
## Test that set-default hint messages are produced at the right times
##
## Copyright (C) 2011 Ganesh Sittampalam
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
rm -rf R1 R2 R3                 # Another script may have left a mess.

darcs init      --repo R1
darcs get R1 R2
darcs get R1 R3

HINTSTRING="issue the same command with the --set-default flag"

cd R3
for command in pull push send
do
   opt=
   test "$command" = "send" && opt=-O

   # R1 should be the default for R3
   darcs $command $opt ../R1 | not grep "$HINTSTRING"
   darcs $command $opt ../R2 | grep "$HINTSTRING"

   # can disable message on the command-line
   darcs $command $opt --no-set-default ../R2 | not grep "$HINTSTRING"

   # or using defaults
   echo "$command no-set-default" >> ../.darcs/defaults
   darcs $command $opt ../R2 | not grep "$HINTSTRING"

   darcs $command $opt --set-default ../R2 | not grep "$HINTSTRING"
   darcs $command $opt --set-default ../R1 | not grep "$HINTSTRING"
done
