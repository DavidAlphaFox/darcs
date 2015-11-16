#!/usr/bin/env bash
## Test for issue2186 - If a patch fails (e.g. because of a conflict), 
## "apply --reply" does not email a status report.
##
## Copyright (C) 2012 Ilya Perminov
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

# Create a script that will be used instead of sendmail. It simply saves its 
# input to file "message".
echo "#!/usr/bin/env bash" >dummy-sendmail
echo "cat >message" >>dummy-sendmail
chmod a+x dummy-sendmail

darcs init --repo R
darcs get R S

# Create an email message with a patch
cd R
echo 'Example content.' >file1
darcs add file1
darcs rec -a --name patch1
darcs send --dont-edit-description --to noreply@example.net --sendmail-command '../dummy-sendmail %<' -a ../S
mv message patch-set1

# Create the same file in the target repository to trigger a conflict.
cd ../S
echo 'Some text.' >file1
darcs add file1
darcs rec -a --name patch2

# Apply the patch set. darcs should email a report. If it does our dummy-sendmail script will 
# create file "message".
darcs apply --reply noreply@example.net --sendmail-command '../dummy-sendmail %<' ../R/patch-set1 || :;
[ -f ./message ] || exit 1
