type angletype
type point
type coord = int * int
type triangle
val ( ++ ) : point -> point -> point
val ( -- ) : point -> point -> point
val ( ** ) : point -> float -> point
val ( // ) : point -> float -> point
val norm : point -> float
val scale : point -> float -> point
val translate : point -> float -> float -> point
val new_point : float -> float -> point
val new_triangle : point -> point -> point -> bool -> triangle
val is_acute : triangle -> bool
val get_point_vect : triangle -> point array
val get_points : triangle -> point * point * point
val get_point : int -> triangle -> point
val round : float -> int
val coordvect_of_pointvect : point array -> coord array
val coordvect_of_triangle : triangle -> coord array
