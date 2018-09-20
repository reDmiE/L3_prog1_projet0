#load "api_triangle.cma";;
open Api_triangle;;

#use "trgraphics.ml";;

(* DEFINE const *)
let screen_width = 800;;
let screen_height = 600;;
let fill_screen = false;;
let maxgen = 9;;
(* DEFINE NUMERICAL CONST *)
let phi = (1.0 +. sqrt(5.0))/.2.0

(* DEFINE functions *)

let rec divide atriangle=
        if not(is_acute atriangle) then
                begin
                let p0,p1,p2 = get_points atriangle in
                let u01 = (p1 -- p0)
                in let p3 = p0 ++ (u01 // (phi))
                in let triangle_obt = (new_triangle p1 p2 p3 false) and triangle_acu = (new_triangle p3 p2 p0 true)
                in (triangle_obt,triangle_acu,triangle_acu)
                end
        else
                begin
                let p0,p1,p2 = get_points atriangle in
                let u02 = (p2 -- p0) and u12 =  (p2 -- p1)
                in let p3 = p2 -- (u02 // phi) and p4 = p1 ++ (u12 // phi)
                in let triangle_obt = new_triangle p2 p3 p4 false
                        and triangle_acu1 = new_triangle p3 p0 p1 true
                        and triangle_acu2 = new_triangle p4 p3 p1 true
                in (triangle_obt,triangle_acu1,triangle_acu2)
                end;;


let next_env env=
        let rec new_list lst acc =
                match lst with
                | [] -> acc
                | h::t when (is_acute h) -> let t1,t2,t3 = (divide h) in new_list t (t1::t2::t3::acc);
                | h::t -> let t1,t2,t3 = (divide h) in new_list t (t1::t2::acc);
        in
        (env.table <- (new_list env.table []));;

let trt = [(compute_first_triangle fill_screen screen_width screen_height)];;
let env = {table=trt; offset=(new_point 0.0 0.0); scale=1.0;};;

let h_keys = gen_handle_key next_env drawenv env;;

let anime () = 
           skel (fun () -> gen_init_anime screen_width screen_height) (end_anime) (h_keys);;

anime ();;
