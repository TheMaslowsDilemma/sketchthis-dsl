(***
----------------------------------------------------------- 
vector.mli
----------------------------------------------------------- 
2d vector type definition and operations.
***)

type vec = { x : float; y : float }

(* create vector from x, y *)
val vec : float -> float -> vec

(* a + b *)
val vec_add : vec -> vec -> vec

(* a - b *)
val vec_sub : vec -> vec -> vec

(* v * scalar *)
val vec_scale : vec -> float -> vec

(* squared length *)
val vec_len_sq : vec -> float

(* length *)
val vec_length : vec -> float

(* dot product *)
val vec_dot : vec -> vec -> float

(* distance between two points *)
val vec_distance : vec -> vec -> float

(* 2d cross product, returns scalar *)
val vec_cross_2d : vec -> vec -> float

(* perpendicular, rotated 90° ccw *)
val vec_perp : vec -> vec

(* normalize, returns (1,0) if near zero *)
val vec_normalize : vec -> vec

(* lerp from a to b by t *)
val vec_lerp : vec -> vec -> float -> vec

(* vec_mirror axis point -> mirrors point about axis.
   axis is normalized, so no need to normalize *)
val vec_mirror : vec -> vec -> vec

(* closest point on segment ab to p, returns (closest, distance) *)
val point_to_segment_closest : vec -> vec -> vec -> vec * float
