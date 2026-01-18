(*
  splines.ml - Catmull-Rom spline evaluation and flattening
*)

open Vector

type arc_data = { endpoint : vec; center : vec; clockwise : bool }
type segment = MoveTo of vec | LineTo of vec | ArcTo of arc_data
type noise_level = NoiseTrace | NoiseDraw | NoiseScribble

(* ═══════════════════════════════════════════════════════════════════════════
   Catmull-Rom Evaluation
   ═══════════════════════════════════════════════════════════════════════════ *)

let catmull_rom_point p0 p1 p2 p3 t =
  let t2 = t *. t in
  let t3 = t2 *. t in
  let eval c0 c1 c2 c3 =
    0.5 *. (
      2.0 *. c1 +.
      (c2 -. c0) *. t +.
      (2.0 *. c0 -. 5.0 *. c1 +. 4.0 *. c2 -. c3) *. t2 +.
      (3.0 *. c1 -. c0 -. 3.0 *. c2 +. c3) *. t3
    )
  in
  vec (eval p0.x p1.x p2.x p3.x) (eval p0.y p1.y p2.y p3.y)

let catmull_rom_tangent p0 p1 p2 p3 t =
  let t2 = t *. t in
  let eval c0 c1 c2 c3 =
    0.5 *. (
      (c2 -. c0) +.
      (4.0 *. c0 -. 10.0 *. c1 +. 8.0 *. c2 -. 2.0 *. c3) *. t +.
      (9.0 *. c1 -. 3.0 *. c0 -. 9.0 *. c2 +. 3.0 *. c3) *. t2
    )
  in
  let dx = eval p0.x p1.x p2.x p3.x in
  let dy = eval p0.y p1.y p2.y p3.y in
  let len = Float.sqrt (dx *. dx +. dy *. dy) in
  if len < Globals.epsilon then vec 1.0 0.0
  else vec (dx /. len) (dy /. len)

(* ═══════════════════════════════════════════════════════════════════════════
   Arc Geometry
   ═══════════════════════════════════════════════════════════════════════════ *)

let arc_center_from_tangent p1 t1 p2 =
  let n = vec_perp t1 in
  let v = vec_sub p2 p1 in
  let denom = vec_dot v n in
  if Float.abs denom < Globals.epsilon then None
  else Some (vec_add p1 (vec_scale n (vec_dot v v /. (2.0 *. denom))))

let sweep_angle a_start a_end clockwise =
  let delta = if clockwise then a_start -. a_end else a_end -. a_start in
  let delta = Float.rem delta (2.0 *. Float.pi) in
  if delta < 0.0 then delta +. 2.0 *. Float.pi else delta

let arc_is_clockwise p1 center t1 =
  vec_cross_2d t1 (vec_sub center p1) > 0.0

let try_arc p1 t1 p2 ~max_sweep ~max_radius_ratio =
  let chord_len = vec_distance p1 p2 in
  if chord_len < Globals.precision then None
  else match arc_center_from_tangent p1 t1 p2 with
    | None -> None
    | Some center ->
        let radius = vec_distance center p1 in
        if radius > max_radius_ratio *. chord_len then None
        else
          let clockwise = arc_is_clockwise p1 center t1 in
          let a_start = Float.atan2 (p1.y -. center.y) (p1.x -. center.x) in
          let a_end = Float.atan2 (p2.y -. center.y) (p2.x -. center.x) in
          let sweep = sweep_angle a_start a_end clockwise in
          if sweep > max_sweep then None
          else Some { endpoint = p2; center; clockwise }

(* ═══════════════════════════════════════════════════════════════════════════
   Biarc Fitting
   ═══════════════════════════════════════════════════════════════════════════ *)

let fit_biarc p1 t1 p2 t2 ~max_sweep =
  let v = vec_sub p2 p1 in
  let chord_len = vec_length v in
  if chord_len < Globals.precision then []
  else
    let t_dot = vec_dot t1 t2 in
    if t_dot > 0.99 then
      match try_arc p1 t1 p2 ~max_sweep ~max_radius_ratio:20.0 with
      | Some arc -> [ArcTo arc]
      | None -> [LineTo p2]
    else
      let t1_dot_v = vec_dot t1 v in
      let t2_dot_v = vec_dot t2 v in
      let denom = 2.0 *. (1.0 -. t_dot) in
      if Float.abs denom < Globals.precision then [LineTo p2]
      else
        let sum = t1_dot_v +. t2_dot_v in
        if Float.abs sum < Globals.precision then [LineTo p2]
        else
          let d = (chord_len *. chord_len -. 2.0 *. t1_dot_v *. t2_dot_v /. sum) /. denom in
          let d = Float.max Globals.precision d in
          if d > 20.0 *. chord_len then [LineTo p2]
          else
            let pm = vec_lerp (vec_add p1 (vec_scale t1 d)) (vec_sub p2 (vec_scale t2 d)) 0.5 in
            let t_sum = vec_add t1 t2 in
            let tm = if vec_len_sq t_sum < Globals.epsilon then vec_normalize (vec_perp v)
                     else vec_normalize t_sum in
            let arc1 = try_arc p1 t1 pm ~max_sweep ~max_radius_ratio:20.0 in
            let arc2 = try_arc pm tm p2 ~max_sweep ~max_radius_ratio:20.0 in
            match arc1, arc2 with
            | Some a1, Some a2 -> [ArcTo a1; ArcTo a2]
            | Some a1, None -> [ArcTo a1; LineTo p2]
            | None, Some a2 -> [LineTo pm; ArcTo a2]
            | None, None -> [LineTo p2]

(* ═══════════════════════════════════════════════════════════════════════════
   Error Measurement
   ═══════════════════════════════════════════════════════════════════════════ *)

let midpoint_error p0 p1 p2 p3 t0 t1 =
  let t_mid = (t0 +. t1) /. 2.0 in
  let pt_start = catmull_rom_point p0 p1 p2 p3 t0 in
  let pt_end = catmull_rom_point p0 p1 p2 p3 t1 in
  let pt_mid = catmull_rom_point p0 p1 p2 p3 t_mid in
  vec_distance pt_mid (vec_lerp pt_start pt_end 0.5)

let estimate_arc_length p0 p1 p2 p3 t0 t1 =
  let n = 8 in
  let len = ref 0.0 in
  let prev = ref (catmull_rom_point p0 p1 p2 p3 t0) in
  for i = 1 to n do
    let t = t0 +. (float_of_int i /. float_of_int n) *. (t1 -. t0) in
    let pt = catmull_rom_point p0 p1 p2 p3 t in
    len := !len +. vec_distance !prev pt;
    prev := pt
  done;
  !len

let arc_fit_error p0 p1 p2 p3 t0 t1 segments =
  let n_samples = 32 in
  let chain_start = catmull_rom_point p0 p1 p2 p3 t0 in
  let max_err = ref 0.0 in
  for i = 0 to n_samples do
    let t = t0 +. (float_of_int i /. float_of_int n_samples) *. (t1 -. t0) in
    let pt = catmull_rom_point p0 p1 p2 p3 t in
    let min_dist = ref Float.infinity in
    let prev = ref chain_start in
    List.iter (fun seg ->
      let seg_end = match seg with
        | MoveTo v | LineTo v -> v
        | ArcTo a -> a.endpoint
      in
      (match seg with
      | MoveTo _ -> ()
      | LineTo _ ->
          let _, d = point_to_segment_closest pt !prev seg_end in
          min_dist := Float.min !min_dist d
      | ArcTo arc ->
          let radius = vec_distance arc.center !prev in
          let to_pt = vec_sub pt arc.center in
          let dist_to_center = vec_length to_pt in
          let a_start = Float.atan2 (!prev.y -. arc.center.y) (!prev.x -. arc.center.x) in
          let a_end = Float.atan2 (arc.endpoint.y -. arc.center.y) (arc.endpoint.x -. arc.center.x) in
          let angle = Float.atan2 to_pt.y to_pt.x in
          let arc_sweep = sweep_angle a_start a_end arc.clockwise in
          let pt_sweep = sweep_angle a_start angle arc.clockwise in
          let d = if pt_sweep <= arc_sweep +. Globals.epsilon
                  then Float.abs (dist_to_center -. radius)
                  else Float.min (vec_distance pt !prev) (vec_distance pt arc.endpoint) in
          min_dist := Float.min !min_dist d);
      prev := seg_end
    ) segments;
    max_err := Float.max !max_err !min_dist
  done;
  !max_err

(* ═══════════════════════════════════════════════════════════════════════════
   Adaptive Flattening
   ═══════════════════════════════════════════════════════════════════════════ *)

let rec flatten p0 p1 p2 p3 t0 t1 ~tol ~use_arcs ~depth ~max_depth =
  let pt0 = catmull_rom_point p0 p1 p2 p3 t0 in
  let pt1 = catmull_rom_point p0 p1 p2 p3 t1 in
  let chord_len = vec_distance pt0 pt1 in

  if chord_len < Globals.epsilon then []
  else if chord_len < Globals.precision then [LineTo pt1]
  else if depth >= max_depth then
    let n = 8 in
    List.init n (fun i ->
      let t = t0 +. (float_of_int (i + 1) /. float_of_int n) *. (t1 -. t0) in
      LineTo (catmull_rom_point p0 p1 p2 p3 t))
  else
    let mid_err = midpoint_error p0 p1 p2 p3 t0 t1 in
    let arc_len = estimate_arc_length p0 p1 p2 p3 t0 t1 in
    let rel_err = if arc_len > Globals.epsilon then mid_err /. arc_len else 1.0 in
    let flat_enough = depth >= 0 && mid_err < tol *. 0.5 && rel_err < 0.01 in

    if flat_enough then [LineTo pt1]
    else if use_arcs && depth >= 0 && depth < 6 then
      let tan0 = catmull_rom_tangent p0 p1 p2 p3 t0 in
      let tan1 = catmull_rom_tangent p0 p1 p2 p3 t1 in
      let segs = fit_biarc pt0 tan0 pt1 tan1 ~max_sweep:(Float.pi *. 0.5) in
      let arc_err = arc_fit_error p0 p1 p2 p3 t0 t1 segs in
      if arc_err < tol && arc_err < mid_err *. 0.8 then segs
      else subdivide p0 p1 p2 p3 t0 t1 ~tol ~use_arcs ~depth ~max_depth
    else subdivide p0 p1 p2 p3 t0 t1 ~tol ~use_arcs ~depth ~max_depth

and subdivide p0 p1 p2 p3 t0 t1 ~tol ~use_arcs ~depth ~max_depth =
  let tm = (t0 +. t1) /. 2.0 in
  flatten p0 p1 p2 p3 t0 tm ~tol ~use_arcs ~depth:(depth + 1) ~max_depth @
  flatten p0 p1 p2 p3 tm t1 ~tol ~use_arcs ~depth:(depth + 1) ~max_depth

(* ═══════════════════════════════════════════════════════════════════════════
   Control Point Handling
   ═══════════════════════════════════════════════════════════════════════════ *)

let get_control_point arr n i =
  if i < 0 then vec_sub arr.(0) (vec_sub arr.(1) arr.(0))
  else if i >= n then vec_add arr.(n - 1) (vec_sub arr.(n - 1) arr.(n - 2))
  else arr.(i)

(* ═══════════════════════════════════════════════════════════════════════════
   Public API
   ═══════════════════════════════════════════════════════════════════════════ *)

let spline_to_segments ?(segments_per_span = 8) points =
  match points with
  | [] | [_] -> []
  | [_; p1] -> [LineTo p1]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get = get_control_point arr n in
      List.concat_map (fun i ->
        List.init segments_per_span (fun j ->
          let t = float_of_int (j + 1) /. float_of_int segments_per_span in
          LineTo (catmull_rom_point (get (i-1)) (get i) (get (i+1)) (get (i+2)) t))
      ) (List.init (n - 1) Fun.id)

let spline_to_segments_adaptive ?(tolerance = 0.05) ?(use_arcs = true) ?(min_segments = 4) points =
  match points with
  | [] | [_] -> []
  | [_; p1] -> [LineTo p1]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get = get_control_point arr n in
      let start_depth =
        if min_segments <= 2 then -1
        else if min_segments <= 4 then -2
        else if min_segments <= 8 then -3
        else -4
      in
      List.concat_map (fun i ->
        flatten (get (i-1)) (get i) (get (i+1)) (get (i+2))
          0.0 1.0 ~tol:tolerance ~use_arcs ~depth:start_depth ~max_depth:8
      ) (List.init (n - 1) Fun.id)

let spline_to_segments_smart ?(tolerance = 0.05) ?(use_arcs = true) ?(min_segments = 4) points =
  spline_to_segments_adaptive ~tolerance ~use_arcs ~min_segments points

(* ═══════════════════════════════════════════════════════════════════════════
   Jitter
   ═══════════════════════════════════════════════════════════════════════════ *)

let () = Random.init 42

let jitter_mag = function
  | NoiseTrace -> 0.0
  | NoiseDraw -> 0.05
  | NoiseScribble -> 0.1

let jitter_vec noise v =
  let mag = jitter_mag noise in
  if mag < Globals.epsilon then v
  else vec (v.x +. Random.float (2.0 *. mag) -. mag)
           (v.y +. Random.float (2.0 *. mag) -. mag)

let spline_to_segments_with_jitter noise points samples_per_span =
  match points with
  | [] | [_] -> []
  | [_; p1] -> [LineTo (jitter_vec noise p1)]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get = get_control_point arr n in
      List.concat_map (fun i ->
        List.init samples_per_span (fun j ->
          let t = float_of_int (j + 1) /. float_of_int samples_per_span in
          LineTo (jitter_vec noise (catmull_rom_point (get (i-1)) (get i) (get (i+1)) (get (i+2)) t)))
      ) (List.init (n - 1) Fun.id)

let spline_to_line_segments = spline_to_segments
let spline_to_line_segments_with_jitter = spline_to_segments_with_jitter

let sample_spline_with_jitter noise points samples_per_span =
  match points with
  | [] | [_] -> points
  | [p0; p1] -> [jitter_vec noise p0; jitter_vec noise p1]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get = get_control_point arr n in
      let result = ref [jitter_vec noise arr.(0)] in
      for i = 0 to n - 2 do
        for j = 1 to samples_per_span do
          let t = float_of_int j /. float_of_int samples_per_span in
          result := jitter_vec noise (catmull_rom_point (get (i-1)) (get i) (get (i+1)) (get (i+2)) t) :: !result
        done
      done;
      List.rev !result

let add_wobble_points noise p0 p1 =
  match noise with
  | NoiseTrace -> [p0; p1]
  | NoiseDraw -> [p0; jitter_vec noise (vec_lerp p0 p1 0.5); p1]
  | NoiseScribble ->
      [p0;
       jitter_vec noise (vec_lerp p0 p1 0.25);
       jitter_vec noise (vec_lerp p0 p1 0.5);
       jitter_vec noise (vec_lerp p0 p1 0.75);
       p1]

let add_wobble_points_adaptive noise scale p0 p1 =
  match noise with
  | NoiseTrace -> [p0; p1]
  | _ ->
      let dist = vec_distance p0 p1 in
      let base = Float.max 1.0 (scale *. 0.05) in
      let factor = if noise = NoiseScribble then 0.5 else 1.0 in
      let min_n = if noise = NoiseScribble then 3 else 2 in
      let count = max min_n (int_of_float (Float.ceil (dist /. (base *. factor)))) in
      p0 :: List.init count (fun i ->
        jitter_vec noise (vec_lerp p0 p1 (float_of_int (i + 1) /. float_of_int (count + 1)))
      ) @ [p1]