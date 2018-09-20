if [ $# -eq 1 ]
then
  make
  ocaml hanoi_r.ml
  R -f times.r
  mkdir pdf
  mv times.pdf pdf/times.pdf
  ocaml hanoi_ext.ml $1
  make clean
else
  echo "Usage : ./run_hanoi.sh <number_of_discs>"
fi
