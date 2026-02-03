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
  let pts = List.sort_uniq (fun a b ->
    let c = Float.compare a.y b.y in
    if c <> 0 then c else Float.compare a.x b.x)
    pts
  in
  match pts with
  | [] | [_] | [_; _] -> pts
  | _ ->
    let pivot = List.hd pts in
    let rest = List.tl pts in
    let angle_cmp a b =
      let da = vec_sub a pivot and db = vec_sub b pivot in
      let cross = vec_cross_2d da db in
      if Float.abs cross < Globals.epsilon then
        Float.compare (vec_len_sq da) (vec_len_sq db)
      else if cross > 0.0 then -1 else 1
    in
    let sorted = List.sort angle_cmp rest in
    let rec build stack = function
      | [] -> stack
      | p :: rest ->
        let rec pop = function
          | a :: (b :: _ as tail) ->
            let cross = vec_cross_2d (vec_sub a b) (vec_sub p b) in
            if cross <= 0.0 then pop tail else a :: (b :: tail)
          | s -> s
        in
        build (p :: pop stack) rest
    in
    let hull = build [pivot] sorted in
    match hull with
    | [] -> []
    | first :: _ -> hull @ [first]

(* Winding number point-in-polygon *)

let point_in_polygon p poly =
  let rec go winding = function
    | [] | [_] -> winding <> 0
    | a :: (b :: _ as rest) ->
      let w =
        if a.y <= p.y then
          (if b.y > p.y && vec_cross_2d (vec_sub b a) (vec_sub p a) > 0.0
           then 1 else 0)
        else
          (if b.y <= p.y && vec_cross_2d (vec_sub b a) (vec_sub p a) < 0.0
           then -1 else 0)
      in
      go (winding + w) rest
  in
  go 0 poly

(* Bounding box of a point list *)

let bbox pts =
  List.fold_left
    (fun (mnx, mny, mxx, mxy) v ->
      (Float.min mnx v.x, Float.min mny v.y,
       Float.max mxx v.x, Float.max mxy v.y))
    (Float.infinity, Float.infinity, Float.neg_infinity, Float.neg_infinity)
    pts

(* Shade a closed region with random dashes *)

let shade_region poly noise =
  let poly = match poly with
    | [] | [_] -> poly
    | first :: _ ->
        let last = List.nth poly (List.length poly - 1) in
        if vec_distance first last < Globals.epsilon then poly
        else poly @ [first]
  in
  let spacing, dash_len, jitter_amt = match noise with
    | NoiseTrace   -> 3.0, 2.0, 0.0
    | NoiseDraw    -> 4.0, 2.5, 0.3
    | NoiseScribble -> 5.0, 3.0, 0.8
  in
  let mnx, mny, mxx, mxy = bbox poly in
  let paths = ref [] in
  let y = ref (mny +. spacing *. 0.5) in
  while !y < mxy do
    let x = ref (mnx +. spacing *. 0.5) in
    while !x < mxx do
      let cx = !x and cy = !y in
      let p = jitter_vec noise (vec cx cy) in
      if point_in_polygon p poly then begin
        let angle = Random.float Float.pi in
        let dx = dash_len *. Float.cos angle *. 0.5 in
        let dy = dash_len *. Float.sin angle *. 0.5 in
        let p0 = jitter_vec noise (vec (p.x -. dx) (p.y -. dy)) in
        let p1 = jitter_vec noise (vec (p.x +. dx) (p.y +. dy)) in
        paths := { start = p0; segments = [{ p0; p1 }] } :: !paths
      end;
      x := !x +. spacing +. (Random.float jitter_amt)
    done;
    y := !y +. spacing +. (Random.float jitter_amt)
  done;
  List.rev !paths
