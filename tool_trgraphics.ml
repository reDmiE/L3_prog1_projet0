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
        let p0 = (((w -. l) /. 2.0, 0.0) :point) and p1 = (((w +. l) /. 2.0, 0.0) :point)
        and p2 = ((w /. 2.0,(l /. phi) *. sqrt(1. -. (phi *. phi) /. 4.0)) :point)
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

let create_window screen_width screen_height=
        let param=" "^(string_of_int screen_width)^"x"^(string_of_int screen_height)^"-0+0" in open_graph param;
        set_window_title "My Superb fractal";;
