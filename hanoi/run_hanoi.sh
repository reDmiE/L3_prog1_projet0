#!/bin/sh
if [ $# -eq 1 ]
then
  make
  ocaml hanoi_r.ml
  R -f times.r
  mkdir -p pdf
  mv times.pdf pdf/times.pdf
  ocaml hanoi_ext.ml $1
  make clean
else
  echo "Usage : $0 <number_of_discs>"
fi
