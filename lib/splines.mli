(*
  splines.mli - Catmull-Rom spline evaluation and flattening
*)

open Vector

type arc_data = { endpoint : vec; center : vec; clockwise : bool }
type segment = MoveTo of vec | LineTo of vec | ArcTo of arc_data
type noise_level = NoiseTrace | NoiseDraw | NoiseScribble

val catmull_rom_point : vec -> vec -> vec -> vec -> float -> vec
val catmull_rom_tangent : vec -> vec -> vec -> vec -> float -> vec

val spline_to_segments : ?segments_per_span:int -> vec list -> segment list
val spline_to_segments_adaptive : ?tolerance:float -> ?use_arcs:bool -> ?min_segments:int -> vec list -> segment list
val spline_to_segments_smart : ?tolerance:float -> ?use_arcs:bool -> ?min_segments:int -> vec list -> segment list
val spline_to_line_segments : ?segments_per_span:int -> vec list -> segment list
val spline_to_segments_with_jitter : noise_level -> vec list -> int -> segment list
val spline_to_line_segments_with_jitter : noise_level -> vec list -> int -> segment list

val jitter_vec : noise_level -> vec -> vec
val sample_spline_with_jitter : noise_level -> vec list -> int -> vec list
val add_wobble_points : noise_level -> vec -> vec -> vec list
val add_wobble_points_adaptive : noise_level -> float -> vec -> vec -> vec list

val arc_center_from_tangent : vec -> vec -> vec -> vec option
val sweep_angle : float -> float -> bool -> float
val fit_biarc : vec -> vec -> vec -> vec -> max_sweep:float -> segment list