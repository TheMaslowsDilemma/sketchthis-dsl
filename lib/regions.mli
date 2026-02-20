(*
-----------------------------------------------------------
regions.mli
*)
open Vector
open Ir

val convex_hull : vec list -> vec list
val point_in_polygon : vec -> vec list -> bool
val shade_region : ?hatch_dir:vec -> vec list -> Splines.noise_level -> ir
val bbox : vec list -> float * float * float * float
