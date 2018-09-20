#load "graphics.cma"
#load "api_rod.cma"
#load "unix.cma"
open Graphics
open Unix
open Api_rod

let width  = 800;;
let height = 600;;
let sleep_time = 0.25;;

let init_window w h title =
  open_graph "";
  set_window_title title;
  resize_window w h;;

(*count the number of discs in an array of rods*)
let count_disc rods =
  let s = ref 0 in
  for i = 0 to (Array.length rods)-1 do
    s := !s + (get_size rods.(i));
  done;
  !s;;

(*coords are the center of the disc*)
let draw_disc x y w h =
  fill_poly [|(x-w/2, y-h/2); (x+w/2, y-h/2); (x+w/2, y+h/2); (x-w/2, y+h/2)|];;

(*just draw the current state of the game*)
let draw_state rods colored_disc color =
  clear_graph ();
  set_color black;
  let n = count_disc rods in
  let w = (size_x ()) / (Array.length rods) and h = (size_y ()) / (2*n) in

  (*the y coord is the y coord of the disc at the top*)
  let rec draw_rod x y rod =
    if not (is_empty rod) then
    begin
      let disc = pop rod in

      draw_rod x (y-h) rod;
      if disc = colored_disc then
        set_color color
      else
        set_color black;

      draw_disc x y (w*disc/n) h;

      push rod disc;
    end;
  in

  for i = 0 to (Array.length rods)-1 do
    let y_top_disc = (get_size rods.(i))*h-(h/2) in
    let x = ((w/2)+i*w) in
    draw_rod x y_top_disc rods.(i);
  done;;


let print_movement origin destination =
  print_string ("I move a disc from rod "^origin^" to rod "^destination^"\n");;

let movement rods origin destination =
  let rod_o = rods.(origin) and rod_d = rods.(destination) in
  let disc = top rod_o in

  draw_state rods disc red;
  Unix.sleepf sleep_time;

  print_movement (get_name rod_o) (get_name rod_d);
  move rod_d rod_o;

  draw_state rods disc red;
  Unix.sleepf sleep_time;
  draw_state rods disc black;
  Unix.sleepf sleep_time;;


let hanoi3_rec start inter goal n =
  let count = ref 0
  and rods = [|(init_rod n start); (init_rod 0 inter); (init_rod 0 goal)|] in
  let rec aux start inter goal n =
    match n with
    | 0 ->  print_string "No discs... no move required\n"
    | 1 ->  movement rods start goal;
            incr count;
    | _ ->  aux start goal inter (n-1);
            aux start inter goal 1;
            aux inter start goal (n-1);
  in aux 0 1 2 n;
  !count;;

(*make only possible move with two given rods*)
let do_possible_move rods rod1 rod2 =
  if is_empty rods.(rod1) then
    movement rods rod2 rod1
  else if is_empty rods.(rod2) then
    movement rods rod1 rod2
  else if (top rods.(rod1)) > (top rods.(rod2)) then
    movement rods rod2 rod1
  else
    movement rods rod1 rod2;;

(*this is an iterative solution, can be easily applied by any human being (able to move discs...)*)
let hanoi3_iter start inter goal n =
  let rods = [|(init_rod n start); (init_rod 0 inter); (init_rod 0 goal)|]
  and smallest_pos = ref 0          (* position of the smallest disc*)
  and direction = 1 - (n mod 2)*2   (*direction of the smallest disc*)
  and count = ref 0 in

  (* two moves per iteration, since we do 2^n-1 moves, just stop before the last move (smallest disc)*)
  while (get_size rods.(2)) <> n-1 do
    (*move the smallest disc, according to the direction*)
    let next_smlst_pos = (!smallest_pos + direction + 3) mod 3 in
    movement rods !smallest_pos next_smlst_pos;
    smallest_pos := next_smlst_pos;

    incr count;

    (*make the only possible move that doesn't involve moving the smallest disc*)
    let rod1 = (next_smlst_pos + 1) mod 3
    and rod2 = (next_smlst_pos + 2) mod 3 in
    do_possible_move rods rod1 rod2;

    incr count;
  done;

  (*last move*)
  let next_smlst_pos = (!smallest_pos + direction + 3) mod 3 in
  movement rods !smallest_pos next_smlst_pos;

  !count+1;;

(*give the id of the disc moved at step k, just write k in binary and find the first 0!*)
(*easily doable by any human begin (able to write numbers in binary)*)
(*first step has id 0!*)
let rec disc_moved_at_step k =
  match k mod 2 with
  | 0 -> 1
  | _ -> 1 + disc_moved_at_step (k / 2);;


init_window width height "Hanoi3";;
hanoi3_rec "A" "B" "C" (int_of_string Sys.argv.(1));;


prerr_string "Type Return to exit...";;
prerr_newline ();;
read_line ();;
