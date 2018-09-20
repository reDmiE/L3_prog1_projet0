if [ $# -eq 1 ]
then
  make
  ocaml hanoi_r.ml $1
  R -f times.r
  mkdir -p pdf
  mv times.pdf pdf/times.pdf
  make clean
else
  echo "Usage : $0 <number_of_discs>"
fi
