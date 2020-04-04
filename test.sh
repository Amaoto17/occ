#!/bin/bash

function unit_test() {
  testname="$1"
  input="test/$testname/$testname.c"
  expect="test/$testname/expect.txt"

  dune exec src/occ.exe $input > tmp/out.s
  if [ $? != 0 ]; then
    echo "failed to compile $testname.c"
    exit 1
  fi

  gcc -o tmp/out tmp/out.s
  if [ $? != 0 ]; then
    echo "failed to assemble"
    exit 1
  fi

  diff -u <("./tmp/out") "$expect"
  if [ $? == 0 ]; then
    echo -e "\033[32;1mPASSED\033[0m ($testname.c)"
  else
    echo -e "\033[31;1mFAILED\033[0m ($testname.c)"
    exit 1
  fi
}

if [ "$1" != "" ]; then
  unit_test "$1"
  exit
fi

for testname in `ls test/`; do
  unit_test "$testname"
done
