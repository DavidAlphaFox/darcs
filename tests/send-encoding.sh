#!/usr/bin/env bash
## Copyright (C) 2011 Ganesh Sittampalam <ganesh@earth.li>
##
## Test that darcs send uses the UTF-8 encoding for emails
## when non-ASCII characters are in the message
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

switch_to_utf8_locale

darcs init --repo empty

darcs init --repo send

cd send
echo 'file1' > file1
darcs record -lam 'file1'

LANG=en_GB.UTF-8 \
 DARCS_EDITOR='echo Non-ASCII chars: é è ề Ψ ޡ ߐ ह ჴ Ᏻ ‱ ⁂ ∰ ✈ ⢅ .. >' \
   darcs send -a ../empty --to=invalid@invalid --edit \
      --sendmail-command='grep "Content-Type: text/plain; charset=\"utf-8\"" %<'
