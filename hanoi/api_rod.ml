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

let move rod1 rod2 =
  push rod1 (pop rod2);;

let get_size rod = rod.size;;
let get_name rod = rod.name;;
