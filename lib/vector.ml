(*
----------------------------------------------------------- 
vector.ml
-----------------------------------------------------------
*)

type vec = { x : float; y : float }

let vec x y : vec = { x; y }
let vec_add a b = { x = a.x +. b.x; y = a.y +. b.y }
let vec_sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let vec_scale v s = { x = v.x *. s; y = v.y *. s }
let vec_len_sq v = (v.x *. v.x) +. (v.y *. v.y)
let vec_length v = Float.sqrt (vec_len_sq v)
let vec_dot a b = (a.x *. b.x) +. (a.y *. b.y)
let vec_cross_2d a b = (a.x *. b.y) -. (a.y *. b.x)
let vec_perp v = { x = -.v.y; y = v.x }

let vec_normalize v =
  let len = vec_length v in
  if len < Globals.epsilon then { x = 1.0; y = 0.0 }
  else vec_scale v (1.0 /. len)

let vec_distance a b = vec_length (vec_sub b a)
let vec_lerp a b t = vec_add a (vec_scale (vec_sub b a) t)

let point_to_segment_closest p a b =
  let ab = vec_sub b a in
  let len_sq = vec_len_sq ab in
  if len_sq < Globals.epsilon then (a, vec_distance p a)
  else
    let t =
      Float.max 0.0 (Float.min 1.0 (vec_dot (vec_sub p a) ab /. len_sq))
    in
    let closest = vec_add a (vec_scale ab t) in
    (closest, vec_distance p closest)
