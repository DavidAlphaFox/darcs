#!/usr/bin/env bash
. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
all_commands=$(darcs --commands | grep -v -- --)

for cmd in $all_commands; do
  # --disable works on command line
  not darcs $cmd --disable 2> log
  grep disable log
  rm log
  # --disable works from defaults
  sub_commands="$(darcs $cmd --list-options | grep -v -- -- || true)"
  # disabling super commands in the defaults file is broken
  if test -z "$sub_commands"; then
    echo "$cmd --disable" > _darcs/prefs/defaults
    not darcs $cmd 2> log
    rm _darcs/prefs/defaults
    grep disable log
    rm log
  elif test $cmd != "setpref" -a $cmd != "help"; then
    # setpref and help are not proper super commands
    for scmd in $sub_commands; do
      echo "$cmd $scmd --disable" > _darcs/prefs/defaults
      not darcs $cmd 2> log
      rm _darcs/prefs/defaults
      grep disable log
      rm log
    done
  fi
done

cd ..
rm -rf temp1
