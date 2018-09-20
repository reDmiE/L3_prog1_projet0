#!/bin/sh
if [ $# -eq 1 ]
then
  make
  ocaml penrose_base.ml $1
  make clean
else
  echo "Usage : ./run_hanoi.sh <number_of_discs>"
fi
