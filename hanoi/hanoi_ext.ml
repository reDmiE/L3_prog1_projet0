#load "graphics.cma"
open Graphics
open Printf

type rod = {name : string; mutable size : int; mutable discs : int list};;

let push rod x =
  rod.size <- rod.size+1;
  rod.discs <- x::rod.discs;;

let pop rod =
  match rod.discs with
  | [] -> failwith "pop : Rod is empty!"
  | h::q -> rod.discs <- q;
            rod.size <- rod.size-1;
            h;;

let is_empty rod = rod.size == 0;;

let top rod =
  match rod.discs with
  | [] -> failwith "top : Rod is empty!"
  | h::q -> h;;

let init_rod n name =
  let rec aux n rod =
    match n with
    | 0 -> rod
    | k when k < 0 -> failwith "Invalid parameter in init_rod : number of disc should be >= 0"
    | _ -> aux (n-1) (n::rod)
  in {name = name; size = n; discs = (aux n [])};;



let print_movement origin destination = print_string ("I move a disc from rod "^origin^" to rod "^destination^"\n");;

let movement rods origin destination =
  print_movement rods.(origin).name rods.(destination).name;
  push rods.(destination) (pop rods.(origin));;

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

print_int (hanoi3_rec "a" "b" "c" 4);;
print_newline ();;

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
  while rods.(2).size <> n-1 do
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

print_int (hanoi3_iter "a" "b" "c" 4);;
print_newline ();;

(*give the id of the disc moved at step k, just write k in binary and find the first 0!*)
(*easily doable by any human begin (able to write numbers in binary)*)
(*first step has id 0!*)
let rec disc_moved_at_step k =
  match k mod 2 with
  | 0 -> 1
  | _ -> 1 + disc_moved_at_step (k/2);;

let get_disc_context_first_move d n =
  if (n-d) mod 2 == 0 then [|0; 1; 2|]
  else [|0; 2; 1|];;

let pow2 n = 1 lsl n;;

let rec get_step_info k n =
  let disc_id = disc_moved_at_step k in
  let context_first_move = get_disc_context_first_move disc_id n in
  let offset = pow2 disc_id in
  let move_count = k/offset in
  print_string "DISC : "; print_int disc_id; print_newline ();
  print_string "ALREADY MOVED "; print_int move_count; print_string " TIME(S)"; print_newline ();;
  (*print_string "MOVE FROM ROD "; print_int context_first_move.(move_count+2 mod 3); print_string " TO ROD "; print_int context_first_move.(move_count+1 mod 3); print_newline ();;*)



let add_result file n t = fprintf file "%d,%d\n" n t;;

let store_hanoi3_results f =
  let file = open_out "results.dat" in
  for n=1 to 10 do
    add_result file n (f "a" "b" "c" n);
  done;
  close_out file;;


let draw_disc n i x y =
  let w = 200/(n-i) and h = 400/n in
  fill_poly [|(x-w/2, y-h/2); (x+w/2, y-h/2); (x+w/2, y+h/2); (x-w/2, y+h/2)|];;


  (*let draw_state n rods = *)
