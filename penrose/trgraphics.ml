#load "graphics.cma";;
open Graphics;;
#load "api_triangle.cma";;
open Api_triangle;;

let phi = (1.0 +. sqrt(5.0))/.2.0;;

let compute_first_triangle fill_screen01 screen_width01 screen_height01 =
        let aux fill_screen02 = 
                match fill_screen02 with
                | true -> ((float_of_int screen_width01) +. (phi *. (float_of_int screen_height01) /. (sqrt(1.0 -. ((phi *. phi) /. 4.0)))))
                | false -> (float_of_int screen_width01);
        in let l = aux fill_screen01 and w = float_of_int screen_width01 in
        let p0 = new_point ((w -. l) /. 2.0) 0.0 and p1 = new_point ((w +. l) /. 2.0) 0.0
        and p2 = new_point (w /. 2.0) ((l /. phi) *. sqrt(1. -. (phi *. phi) /. 4.0))
        in new_triangle p0 p1 p2 false;;

let draw_triangle_with_line atriangle= 
        let coordpoints = coordvect_of_triangle atriangle in
        if not(is_acute atriangle) then
                set_color magenta
        else
                set_color cyan;
        fill_poly coordpoints;
        set_color black;
        draw_poly coordpoints;;

let draw_triangle atriangle= 
        let coordpoints = coordvect_of_triangle atriangle in
        if not(is_acute atriangle) then
                set_color magenta
        else
                set_color cyan;
        fill_poly coordpoints;;

let draw_line (p0 :point) (p1 :point)=
        set_color black;
        let points = coordvect_of_pointvect [|p0; p1|]
        in let x0,y0 = points.(0) and x1,y1 = points.(1) in
        moveto x0 y0; lineto x1 y1;;

let create_window screen_width screen_height=
        let param=" "^(string_of_int screen_width)^"x"^(string_of_int screen_height)^"-0+0" in open_graph param;
        set_window_title "My Superb fractal";;

(* Define event loop for animation *)
(* Thanks to https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora050.html *)
exception End;;
let skel f_init f_end f_key= 
  f_init ();
  try 
      while true do 
        try 
          let s = Graphics.wait_next_event 
                    [Graphics.Button_down; Graphics.Key_pressed] 
          in if s.Graphics.keypressed then f_key s.Graphics.key
             else ()
        with 
             End -> raise End
      done
  with 
        End  -> f_end ();;

let gen_init_anime screen_width screen_height = create_window screen_width screen_height; auto_synchronize false;;
let end_anime () = close_graph ();;

type environnement = { mutable table:triangle list; mutable scale:float; mutable offset: point};;

let gen_handle_key nextstep drawfonction env c = 
        (match c with
        |'q' -> env.offset <-  env.offset ++ (new_point 2.0 0.0)
        |'d' -> env.offset <-  env.offset -- (new_point 2.0 0.0)
        |'z' -> env.offset <-  env.offset -- (new_point 0.0 2.0)
        |'s' -> env.offset <-  env.offset ++ (new_point 0.0 2.0)
        |'w' -> nextstep env
        |'a' -> env.scale <- (env.scale +. 0.2)
        |'e' -> raise End
        | _ -> ());
        clear_graph ();
        drawfonction env;
        synchronize ();;

let rec draw_scaled_triangle atriangle offset scale=
        let p0,p1,p2 = get_points atriangle
        in let pp0  = (p0 -- offset) ** scale
        and pp1  = (p1 -- offset) ** scale
        and pp2  = (p2 -- offset) ** scale
        in let trtmp = new_triangle pp0 pp1 pp2 (is_acute atriangle)
        in draw_triangle_with_line trtmp;;

let drawenv env = 
        let rec aux tbl =
                match tbl with
                | [] -> ()
                | h::t -> (draw_scaled_triangle h env.offset env.scale); aux t
        in
        aux env.table;;
