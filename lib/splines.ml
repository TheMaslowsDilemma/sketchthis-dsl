(*
----------------------------------------------------------- 
splines.ml
-----------------------------------------------------------
*)

open Vector

type segment = { p0 : vec; p1 : vec }
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
  let tangent = vec (eval p0.x p1.x p2.x p3.x) (eval p0.y p1.y p2.y p3.y) in
  let len = vec_length tangent in
  if len < Globals.epsilon then vec 1.0 0.0
  else vec_scale tangent (1.0 /. len)

(* ═══════════════════════════════════════════════════════════════════════════
   Segment Conversion
   ═══════════════════════════════════════════════════════════════════════════ *)

let points_to_segments points =
  let rec aux ps acc =
    match ps with
    | [] | [_] -> List.rev acc
    | p0 :: (p1 :: _ as rest) -> aux rest ({ p0; p1 } :: acc)
  in
  aux points []

(* ═══════════════════════════════════════════════════════════════════════════
   Noise / Jitter
   ═══════════════════════════════════════════════════════════════════════════ *)

let () = Random.init 42

let jitter_mag = function
  | NoiseTrace -> 0.0
  | NoiseDraw -> 0.03
  | NoiseScribble -> 0.08

let jitter_vec noise v =
  let mag = jitter_mag noise in
  if mag < Globals.epsilon then v
  else vec (v.x +. Random.float (2.0 *. mag) -. mag)
           (v.y +. Random.float (2.0 *. mag) -. mag)

let noise_point_count noise dist =
  match noise with
  | NoiseTrace -> 0
  | NoiseDraw -> max 1 (int_of_float (dist /. 2.0))
  | NoiseScribble -> max 2 (int_of_float (dist /. 1.0))

let add_noise noise p0 p1 =
  let dist = vec_distance p0 p1 in
  let n = noise_point_count noise dist in
  if n = 0 then [p0; p1]
  else
    let rec aux i acc =
      if i > n then p1 :: acc
      else
        let t = float_of_int i /. float_of_int (n + 1) in
        let pt = jitter_vec noise (vec_lerp p0 p1 t) in
        aux (i + 1) (pt :: acc)
    in
    List.rev (aux 1 [p0])

(* ═══════════════════════════════════════════════════════════════════════════
   Spline Flattening
   ═══════════════════════════════════════════════════════════════════════════ *)

let get_control_point arr n i =
  if i < 0 then vec_sub arr.(0) (vec_sub arr.(1) arr.(0))
  else if i >= n then vec_add arr.(n - 1) (vec_sub arr.(n - 1) arr.(n - 2))
  else arr.(i)

let sample_span p0 p1 p2 p3 count =
  let rec aux i acc =
    if i > count then List.rev acc
    else
      let t = float_of_int i /. float_of_int count in
      aux (i + 1) (catmull_rom_point p0 p1 p2 p3 t :: acc)
  in
  aux 0 []

let flatten_spline ?(samples_per_span = 8) points =
  match points with
  | [] -> []
  | [p] -> [p]
  | [p0; p1] -> [p0; p1]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get = get_control_point arr n in
      let rec aux i acc =
        if i >= n - 1 then List.rev acc
        else
          let span_pts = sample_span (get (i-1)) (get i) (get (i+1)) (get (i+2)) samples_per_span in
          let pts = if i = 0 then span_pts else List.tl span_pts in
          aux (i + 1) (List.rev_append pts acc)
      in
      aux 0 []

(* ═══════════════════════════════════════════════════════════════════════════
   Combined: Spline to Segments with Noise
   ═══════════════════════════════════════════════════════════════════════════ *)

let spline_to_segments ?(samples_per_span = 8) noise points =
  let flat_pts = flatten_spline ~samples_per_span points in
  let noised_pts =
    match flat_pts with
    | [] -> []
    | [p] -> [p]
    | first :: rest ->
        let rec aux prev ps acc =
          match ps with
          | [] -> List.rev acc
          | p :: rest ->
              let noised = add_noise noise prev p in
              let to_add = if acc = [] then noised else List.tl noised in
              aux p rest (List.rev_append to_add acc)
        in
        aux first rest [first]
  in
  points_to_segments noised_pts