(*
-----------------------------------------------------------
regions.ml
-----------------------------------------------------------
convex hulls, point-in-polygon, and shade fill operations.
*)

open Vector
open Ir
open Splines

(* Graham scan — O(n log n) convex hull *)

let convex_hull pts =
  let pts =
    List.sort_uniq
      (fun a b ->
        let c = Float.compare a.y b.y in
        if c <> 0 then c else Float.compare a.x b.x)
      pts
  in
  match pts with
  | [] | [ _ ] | [ _; _ ] -> pts
  | _ -> (
      let pivot = List.hd pts in
      let rest = List.tl pts in
      let angle_cmp a b =
        let da = vec_sub a pivot and db = vec_sub b pivot in
        let cross = vec_cross_2d da db in
        if Float.abs cross < Globals.epsilon then
          Float.compare (vec_len_sq da) (vec_len_sq db)
        else if cross > 0.0 then -1
        else 1
      in
      let sorted = List.sort angle_cmp rest in
      let rec build stack = function
        | [] -> stack
        | p :: rest ->
            let rec pop = function
              | a :: (b :: _ as tail) ->
                  let cross = vec_cross_2d (vec_sub a b) (vec_sub p b) in
                  if cross <= 0.0 then pop tail else a :: b :: tail
              | s -> s
            in
            build (p :: pop stack) rest
      in
      let hull = build [ pivot ] sorted in
      match hull with [] -> [] | first :: _ -> hull @ [ first ])

(* Winding number point-in-polygon *)

let point_in_polygon p poly =
  let rec go winding = function
    | [] | [ _ ] -> winding <> 0
    | a :: (b :: _ as rest) ->
        let w =
          if a.y <= p.y then
            if b.y > p.y && vec_cross_2d (vec_sub b a) (vec_sub p a) > 0.0 then
              1
            else 0
          else if b.y <= p.y && vec_cross_2d (vec_sub b a) (vec_sub p a) < 0.0
          then -1
          else 0
        in
        go (winding + w) rest
  in
  go 0 poly

(* Bounding box of a point list *)

let bbox pts =
  List.fold_left
    (fun (mnx, mny, mxx, mxy) v ->
      ( Float.min mnx v.x,
        Float.min mny v.y,
        Float.max mxx v.x,
        Float.max mxy v.y ))
    (Float.infinity, Float.infinity, Float.neg_infinity, Float.neg_infinity)
    pts

(* Ensure polygon is closed *)

let close_polygon poly =
  match poly with
  | [] | [ _ ] -> poly
  | first :: _ ->
      let last = List.nth poly (List.length poly - 1) in
      if vec_distance first last < Globals.epsilon then poly
      else poly @ [ first ]

(* Intersect infinite line (origin + t * dir) with segment (a -> b).
   Returns Some t if the intersection lies on the segment. *)

let line_seg_intersect origin dir a b =
  let edge = vec_sub b a in
  let denom = vec_cross_2d dir edge in
  if Float.abs denom < Globals.epsilon then None
  else
    let w = vec_sub a origin in
    let t = vec_cross_2d w edge /. denom in
    let s = vec_cross_2d w dir /. denom in
    if s >= 0.0 && s <= 1.0 then Some t else None

(* Collect all intersection t-values of a line with polygon edges *)

let line_poly_intersections origin dir poly =
  let rec go acc = function
    | [] | [ _ ] -> acc
    | a :: (b :: _ as rest) ->
        let acc' =
          match line_seg_intersect origin dir a b with
          | Some t -> t :: acc
          | None -> acc
        in
        go acc' rest
  in
  let ts = go [] poly in
  (* Deduplicate near-coincident intersections (vertex hits) *)
  let sorted = List.sort Float.compare ts in
  let rec dedup = function
    | [] -> []
    | [ x ] -> [ x ]
    | a :: (b :: _ as rest) ->
        if Float.abs (b -. a) < Globals.epsilon then dedup rest
        else a :: dedup rest
  in
  dedup sorted

(* Shade a closed region with hatch lines.
   hatch_dir controls the orientation of the parallel lines. *)

let shade_region ?(hatch_dir = vec_normalize (vec 1.0 0.3)) poly noise =
  let poly = close_polygon poly in
  let spacing, jitter_amt, subdivisions =
    match noise with
    | NoiseTrace -> (2.5, 0.0, 1)
    | NoiseDraw -> (2.5, 0.3, 5)
    | NoiseScribble -> (2.5, 0.6, 7)
  in
  let hatch_dir = vec_normalize hatch_dir in
  let perp = vec_perp hatch_dir in
  (* Project all vertices onto perpendicular axis to find line range *)
  let proj_min, proj_max =
    List.fold_left
      (fun (mn, mx) v ->
        let d = vec_dot v perp in
        (Float.min mn d, Float.max mx d))
      (Float.infinity, Float.neg_infinity) poly
  in
  let paths = ref [] in
  let offset = ref (proj_min +. (spacing *. 0.5)) in
  while !offset < proj_max do
    let line_origin = vec_scale !offset perp in
    let ts = line_poly_intersections line_origin hatch_dir poly in
    (match ts with
     | t0 :: t1 :: _ ->
         let p0 = vec_add line_origin (vec_scale t0 hatch_dir) in
         let p1 = vec_add line_origin (vec_scale t1 hatch_dir) in
         if subdivisions <= 1 then
           (* Clean straight segment *)
           let p0j = jitter_vec noise p0 in
           let p1j = jitter_vec noise p1 in
           paths :=
             { start = p0j; segments = [ { p0 = p0j; p1 = p1j } ] } :: !paths
         else
           (* Subdivide and jitter for hand-drawn wobble *)
           let pts =
             List.init (subdivisions + 1) (fun i ->
               let t = float_of_int i /. float_of_int subdivisions in
               jitter_vec noise (vec_lerp p0 p1 t))
           in
           paths :=
             { start = List.hd pts; segments = points_to_segments pts }
             :: !paths
     | _ -> ());
    offset := !offset +. spacing +. (Random.float jitter_amt)
  done;
  List.rev !paths