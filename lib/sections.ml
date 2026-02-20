(*---------------------------------------------------------
sections.ml - section overlap detection
---------------------------------------------------------*)

open Vector
open Ir

type render_info = {
  section : string;
  method_ : string;
  line : int;
  column : int;
}

type rendered_sketch = {
  ir : ir;
  info : render_info;
}

type section_data = {
  name : string;
  sketches : rendered_sketch list;
}

type sketch_intersection = {
  point : vec;
  sketch1 : render_info;
  sketch2 : render_info;
}

type overlap_warning = {
  msg : string;
  intersections : sketch_intersection list;
}

let empty_section name = { name; sketches = [] }

let add_sketch sec info ir =
  { sec with sketches = { ir; info } :: sec.sketches }

(* Bounding box *)

type bbox = { min_x : float; max_x : float; min_y : float; max_y : float }

let empty_bbox = {
  min_x = Float.infinity;
  max_x = Float.neg_infinity;
  min_y = Float.infinity;
  max_y = Float.neg_infinity;
}

let update_bbox b v = {
  min_x = Float.min b.min_x v.x;
  max_x = Float.max b.max_x v.x;
  min_y = Float.min b.min_y v.y;
  max_y = Float.max b.max_y v.y;
}

let path_bbox (p : path) =
  List.fold_left (fun b (s : segment) ->
    update_bbox (update_bbox b s.p0) s.p1
  ) (update_bbox empty_bbox p.start) p.segments

let ir_bbox ir =
  List.fold_left (fun b p ->
    let pb = path_bbox p in
    { min_x = Float.min b.min_x pb.min_x;
      max_x = Float.max b.max_x pb.max_x;
      min_y = Float.min b.min_y pb.min_y;
      max_y = Float.max b.max_y pb.max_y }
  ) empty_bbox ir

let sketch_bbox sk = ir_bbox sk.ir

let bbox_intersects a b =
  not (a.max_x < b.min_x || b.max_x < a.min_x ||
       a.max_y < b.min_y || b.max_y < a.min_y)

(* Segment-segment intersection *)

let segment_intersect (s1 : segment) (s2 : segment) : vec option =
  let p = s1.p0 in
  let r = vec_sub s1.p1 s1.p0 in
  let q = s2.p0 in
  let s = vec_sub s2.p1 s2.p0 in
  let r_cross_s = (r.x *. s.y) -. (r.y *. s.x) in
  if Float.abs r_cross_s < Globals.epsilon then
    None
  else
    let qp = vec_sub q p in
    let t = ((qp.x *. s.y) -. (qp.y *. s.x)) /. r_cross_s in
    let u = ((qp.x *. r.y) -. (qp.y *. r.x)) /. r_cross_s in
    if t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0 then
      Some (vec (p.x +. t *. r.x) (p.y +. t *. r.y))
    else
      None

let all_segments ir : segment list =
  List.concat_map (fun (p : path) -> p.segments) ir

(* Find all intersection points between two sketches *)
let sketch_intersections sk1 sk2 : vec list =
  let segs1 = all_segments sk1.ir in
  let segs2 = all_segments sk2.ir in
  List.concat_map (fun s1 ->
    List.filter_map (fun s2 -> segment_intersect s1 s2) segs2
  ) segs1

(* Check two sketches for intersection *)
let check_sketch_pair sk1 sk2 : sketch_intersection list =
  let b1 = sketch_bbox sk1 in
  let b2 = sketch_bbox sk2 in
  if not (bbox_intersects b1 b2) then []
  else
    let points = sketch_intersections sk1 sk2 in
    List.map (fun point -> {
      point;
      sketch1 = sk1.info;
      sketch2 = sk2.info;
    }) points

(* Check two sections for overlap *)
let check_section_pair sec1 sec2 : sketch_intersection list =
  List.concat_map (fun sk1 ->
    List.concat_map (fun sk2 ->
      check_sketch_pair sk1 sk2
    ) sec2.sketches
  ) sec1.sketches

(* Check all section pairs, excluding "default" *)
let check_overlaps sections =
  let non_default = List.filter (fun s -> s.name <> "default") sections in
  let rec check_all acc = function
    | [] -> acc
    | sec :: rest ->
        let new_intersections = List.concat_map (check_section_pair sec) rest in
        check_all (acc @ new_intersections) rest
  in
  match check_all [] non_default with
  | [] -> None
  | intersections -> Some {
      msg = "intersecting sections found";
      intersections;
    }

(* JSON formatting *)

let format_vec v = Printf.sprintf "(%.2f, %.2f)" v.x v.y

let format_render_info info =
  Printf.sprintf "{ \"section\": \"%s\", \"method\": \"%s\", \"line\": %d, \"col\": %d }"
    info.section info.method_ info.line info.column

let format_sketch_intersection si =
  Printf.sprintf "{ \"sketch1\": %s, \"sketch2\": %s, \"point\": \"%s\" }"
    (format_render_info si.sketch1)
    (format_render_info si.sketch2)
    (format_vec si.point)

let take n lst =
  let rec go acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> go (x :: acc) (n - 1) xs
  in
  go [] n lst

let format_warning w =
  let total = List.length w.intersections in
  let shown = take 3 w.intersections in
  let intersections_json = List.map format_sketch_intersection shown in
  let truncated = if total > 3 then Printf.sprintf ", \"truncated\": %d" (total - 3) else "" in
  Printf.sprintf "{ \"msg\": \"%s\", \"intersections\": [ %s ]%s }"
    w.msg (String.concat ", " intersections_json) truncated