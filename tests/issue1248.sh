#!/usr/bin/env bash
## Test for issue1248 - darcs doesn't handle darcs 1 repos with compressed
## inventories
##
## Placed into the public domain by Ganesh Sittampalam, 2009

. lib

gunzip -c  $TESTDATA/oldfashioned-compressed.tgz | tar xf -
cd oldfashioned-compressed
darcs optimize upgrade
darcs check
