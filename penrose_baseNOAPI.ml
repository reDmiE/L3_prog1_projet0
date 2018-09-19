#load "graphics.cma";;
open Graphics;;

(* DEFINE const *)
let screen_width = 800;;
let screen_height = 600;;
let fill_screen = true;;
let generation = 9;;
(* DEFINE NUMERICAL CONST *)
let phi = (1.0 +. sqrt(5.0))/.2.0

(* DEFINE types *)
type angletype = Obtuse | Acute;;
type point = (float * float);;
(* DEFINE operators on points *)
let (++) p1 p2 = let x1,y1 = p1 and x2,y2 = p2 in (x1 +. x2, y1 +. y2);;
let (--) p1 p2 = let x1,y1 = p1 and x2,y2 = p2 in (x1 -. x2, y1 -. y2);;
let ( ** ) p lambda = let x1,y1 = p in (x1 *. lambda, y1 *. lambda);;
let (//) p lambda = let x1,y1 = p in (x1 /. lambda, y1 /. lambda);;
let norm p1 = let x1,y1 = p1 in (x1*.x1 +. y1*.y1);;

(* DEFINE functions *)

(* WARNING: only works with positive values  *)
let round x = int_of_float (floor (x +. 0.5))


let compute_first_triangle fill_screen01 screen_width01 screen_height01 =
        let aux fill_screen02 = 
                match fill_screen02 with
                | true -> ((float_of_int screen_width01) +. (phi *. (float_of_int screen_height01) /. (sqrt(1.0 -. ((phi *. phi) /. 4.0)))))
                | false -> (float_of_int screen_width01);
        in let l = aux fill_screen01 and w = float_of_int screen_width in
        [|((w -. l) /. 2.0, 0.0);((w +. l) /. 2.0, 0.0);
        (w /. 2.0,(l /. phi) *. sqrt(1. -. (phi *. phi) /. 4.0))|];;


let coordvect_of_floatvect floatvect = 
        let n = Array.length floatvect in
        let intvect = Array.make (n) (0,0) in
        for i=0 to (n-1) do
                let x,y = floatvect.(i) in intvect.(i) <- (round x, round y)
        done;
        intvect;;


let draw points typ= 
        let intpoints = coordvect_of_floatvect points in
        if typ = Obtuse then
                set_color yellow
        else
                set_color blue;
        fill_poly intpoints;
        set_color black;
        draw_poly intpoints;;


let rec divide generation points typ =
        if generation = 0
        then draw points typ
        else
                begin
                if typ = Obtuse then
                        begin
                        let u01 = (points.(1) -- points.(0))
                        in let p4 = points.(0) ++ (u01 // (phi))
                        in let triangle_obt = [|points.(1);points.(2);p4|] and triangle_ang = [|p4;points.(2);points.(0)|]
                        in divide (generation - 1) triangle_obt Obtuse;
                        divide (generation - 1) triangle_ang Acute;
                        end
                else
                        begin
                        let u02 = (points.(2) -- points.(0)) and u12 =  (points.(2) -- points.(1))
                        in let p4 = points.(2) -- (u02 // phi) and p5 = points.(1) ++ (u12 // phi)
                        in let triangle_obt = [|points.(2);p4;p5|]
                                and triangle_ang1 = [|p4;points.(0);points.(1)|]
                                and triangle_ang2 = [|p5;p4;points.(1)|]
                                in divide (generation - 1) triangle_obt Obtuse;
                        divide (generation -1) triangle_ang1 Acute;
                        divide (generation -1) triangle_ang2 Acute;
                        end
                end;;


(* create window based on the already specified const *)
let param=" "^(string_of_int screen_width)^"x"^(string_of_int screen_height)^"-0+0" in open_graph param;;
set_window_title "My Superb fractal";;
(* compute first triangle size *)
let first_triangle = compute_first_triangle fill_screen screen_width screen_height;;
(* finally compute the penrose tessellation *)
divide generation first_triangle Obtuse;;
(* wait before closing *)
prerr_string "Type Return to exit...";;
prerr_newline ();;
read_line ();;
