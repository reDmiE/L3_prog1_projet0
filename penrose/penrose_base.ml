#load "api_triangle.cma";;
open Api_triangle;;

#use "trgraphics.ml";;

(* DEFINE const *)
let screen_width = 800;;
let screen_height = 600;;
let fill_screen = true;;
let generation = 9;;
(* DEFINE NUMERICAL CONST *)
let phi = (1.0 +. sqrt(5.0))/.2.0

(* DEFINE functions *)

let rec divide generation atriangle =
        if generation = 0
        then draw_triangle_with_line atriangle
        else
                begin
                if not(is_acute atriangle) then
                        begin
                        let p0,p1,p2 = get_points atriangle in
                        let u01 = (p1 -- p0)
                        in let p3 = p0 ++ (u01 // (phi))
                        in let triangle_obt = (new_triangle p1 p2 p3 false) and triangle_acu = (new_triangle p3 p2 p0 true)
                        in divide (generation - 1) triangle_obt;
                        divide (generation - 1) triangle_acu;
                        end
                else
                        begin
                        let p0,p1,p2 = get_points atriangle in
                        let u02 = (p2 -- p0) and u12 =  (p2 -- p1)
                        in let p3 = p2 -- (u02 // phi) and p4 = p1 ++ (u12 // phi)
                        in let triangle_obt = new_triangle p2 p3 p4 false
                                and triangle_acu1 = new_triangle p3 p0 p1 true
                                and triangle_acu2 = new_triangle p4 p3 p1 true
                                in divide (generation - 1) triangle_obt;
                        divide (generation -1) triangle_acu1;
                        divide (generation -1) triangle_acu2;
                        end
                end;;




create_window screen_width screen_height;;
let first_triangle = compute_first_triangle fill_screen screen_width screen_height
in divide (int_of_string Sys.argv.(1)) first_triangle;;

(* wait before closing *)
prerr_string "Type Return to exit...";;
prerr_newline ();;
read_line ();;
