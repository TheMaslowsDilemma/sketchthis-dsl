(*
-----------------------------------------------------------
regions.mli
-----------------------------------------------------------
convex hulls, point-in-polygon, and shade fill operations.
*)

open Vector
open Ir

val convex_hull : vec list -> vec list
val point_in_polygon : vec -> vec list -> bool
val shade_region : vec list -> Splines.noise_level -> ir