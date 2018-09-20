# L3_prog1_projet0
Project 0 (prog1)  Hanoi/Penrose

## Hanoi Towers

## Penrose Tessellation
Run the basic penrose tesselation (contained in `penrose/penrose.base.ml`) with:
```
cd penrose
./run_penrose <generation_number>
```

But one can also run a version in which there is no line drawed 2 times with:
```
./run_penrose_noDoubleLine.sh <generation_number>
```

There ! Is the **Ultimate** version where you can move around:
```
./run_penrose_anime.sh
```
Use: `z` `q` `s` `d` to move, `a` to zoom, `w` to draw the next generation and `e` to exit
Note: This one *does not need* a number as argument

Global Note: be sure to be in the directory before running the script
as they have no awareness of where they are launched from.
