(* DEFINE types *)
type angletype = Obtuse | Acute;;
type point = (float * float);; (* more precise but not cannot be displayed *)
type coord = (int * int);; (* coord adapted to the display library *)
type triangle = { points:(point array); typ:angletype};;
(* DEFINE operators on points *)
let (++) (p1 :point) (p2 :point) = let x1,y1 = p1 and x2,y2 = p2 in let p3 = (x1 +. x2, y1 +. y2) in (p3 :point);;
let (--) (p1 :point) (p2 :point) = let x1,y1 = p1 and x2,y2 = p2 in let p3 = (x1 -. x2, y1 -. y2) in (p3 :point);;
let ( ** ) (p :point) (lambda :float)= let x1,y1 = p in let p1 = (x1 *. lambda, y1 *. lambda) in (p1 :point);;
let (//) (p :point) (lambda :float)= let x1,y1 = p in let p1 = (x1 /. lambda, y1 /. lambda) in (p1 :point);;
let norm (p1 :point) = let x1,y1 = p1 in let n = (x1*.x1 +. y1*.y1) in (n :float);;

let scale (p1 :point) factor = let x1,y1 = p1 in let p = (x1 *. factor, y1 *. factor) in (p :point);;
let translate (p1 :point) xoffset yoffset = let x1,y1 = p1 in let p = (x1 +. xoffset, y1 +. yoffset) in (p :point);;
(* DEFINE functions *)

let new_point x y = ((x,y):point);;
(* for triangles *)
let new_triangle p0 p1 p2 is_acute =
        let trpoints = ([|p0;p1;p2|] :point array) in
        if is_acute then
                let atriangle = { points= trpoints; typ = Acute} in atriangle;
        else
                let atriangle = { points= trpoints; typ = Obtuse} in atriangle;;

let is_acute atriangle = (atriangle.typ = Acute);;
let get_point_vect atriangle = (atriangle.points :point array);;
let get_points atriangle = 
        let p0 = atriangle.points.(0)
        and p1 = atriangle.points.(1)
        and p2 = atriangle.points.(2)
        in (p0,p1,p2);;
let get_point n atriangle = 
        match n with
        | n when ((n < 3) && (n >= 0)) -> (atriangle.points.(n) :point);
        | _ -> failwith "please specify a number between 0 and 2 included";;

(* WARNING: only works with positive values  *)
let round x = int_of_float (floor (x +. 0.5))

let coordvect_of_pointvect (pointvect :point array)= 
        let n = Array.length pointvect in
        let coordvect = Array.make (n) (0,0) in
        for i=0 to (n-1) do
                let x,y = pointvect.(i) in coordvect.(i) <- (round x, round y)
        done;
        (coordvect :coord array);;

let coordvect_of_triangle atriangle =
        let pts = get_point_vect atriangle in coordvect_of_pointvect pts;;
