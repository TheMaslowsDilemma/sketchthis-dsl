(***
----------------------------------------------------------- 
vector.mli
----------------------------------------------------------- 
2d vector type definition and operations.
***)

type vec = { x : float; y : float }

val vec : float -> float -> vec
val vec_add : vec -> vec -> vec
val vec_sub : vec -> vec -> vec
val vec_scale : vec -> float -> vec
val vec_len_sq : vec -> float
val vec_length : vec -> float
val vec_dot : vec -> vec -> float
val vec_distance : vec -> vec -> float
val vec_cross_2d : vec -> vec -> float
val vec_perp : vec -> vec
val vec_normalize : vec -> vec
val vec_lerp : vec -> vec -> float -> vec
val point_to_segment_closest : vec -> vec -> vec -> vec * float
