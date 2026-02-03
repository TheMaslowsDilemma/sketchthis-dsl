(*
----------------------------------------------------------- 
splines.ml
-----------------------------------------------------------
segment definitions, catmull-rom spline logic, noising logic
*)
open Vector

type noise_level = NoiseTrace | NoiseDraw | NoiseScribble

(* point on catmull-rom curve at parameter t, defined by p0..p3 *)
val catmull_rom_point : vec -> vec -> vec -> vec -> float -> vec

(* unit tangent on catmull-rom curve at parameter t *)
val catmull_rom_tangent : vec -> vec -> vec -> vec -> float -> vec

(* convert point list to segment list *)
val points_to_segments : vec list -> Ir.segment list

(* apply jitter to a point based on noise level *)
val jitter_vec : noise_level -> vec -> vec

(* insert jittered points between p0 and p1 based on noise level and distance *)
val add_noise : noise_level -> vec -> vec -> vec list

(* flatten spline control points to evenly sampled points *)
val flatten_spline : ?samples_per_span:int -> vec list -> vec list

(* flatten spline and convert to segments with noise applied *)
val spline_to_segments :
  ?samples_per_span:int -> noise_level -> vec list -> Ir.segment list
