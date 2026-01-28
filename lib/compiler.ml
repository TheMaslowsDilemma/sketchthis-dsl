(*
-----------------------------------------------------------
compiler.ml
sketchlang compiler logic
-----------------------------------------------------------
*)

open Ast
open Vector
open Splines
open Environment

type path = { start : vec; segments : segment list }
type ir = path list
type bounds = { min_x : float; max_x : float; min_y : float; max_y : float }
type compile_error = UndefinedVariable of string | InvalidOperation of string

exception CompileError of compile_error

let error e = raise (CompileError e)

(* Flow Field Logic *)

type flow_source = { origin : vec; target : vec; dir : vec }

let make_flow_source p0 p1 =
  let d = vec_sub p1 p0 in
  let len = vec_length d in
  let dir =
    if len < Globals.epsilon then vec 1.0 0.0 else vec_scale d (1.0 /. len)
  in
  { origin = p0; target = p1; dir }

let sample_flow_field sources p =
  match sources with
  | [] -> vec 1.0 0.0
  | _ ->
      let sx, sy, sw =
        List.fold_left
          (fun (ax, ay, aw) src ->
            let closest, _ = point_to_segment_closest p src.origin src.target in
            let d = vec_distance p closest in
            let w = 1.0 /. (Globals.precision +. (d *. d)) in
            (ax +. (src.dir.x *. w), ay +. (src.dir.y *. w), aw +. w))
          (0.0, 0.0, 0.0) sources
      in
      if sw < Globals.epsilon then vec 1.0 0.0
      else vec_normalize (vec (sx /. sw) (sy /. sw))

(* Computing Bounds and Centers of intermediate repr *)

let empty_bounds =
  {
    min_x = Float.infinity;
    max_x = Float.neg_infinity;
    min_y = Float.infinity;
    max_y = Float.neg_infinity;
  }

let update_bounds b v =
  {
    min_x = Float.min b.min_x v.x;
    max_x = Float.max b.max_x v.x;
    min_y = Float.min b.min_y v.y;
    max_y = Float.max b.max_y v.y;
  }

let segment_bounds b s = update_bounds (update_bounds b s.p0) s.p1

let path_bounds b p =
  List.fold_left segment_bounds (update_bounds b p.start) p.segments

let compute_bounds ir = List.fold_left path_bounds empty_bounds ir

let compute_center ir =
  if ir = [] then vec 0.0 0.0
  else
    let b = compute_bounds ir in
    vec ((b.min_x +. b.max_x) /. 2.0) ((b.min_y +. b.max_y) /. 2.0)

(* Transformations *)

let transform_segment f s = { p0 = f s.p0; p1 = f s.p1 }

let transform_path f path =
  {
    start = f path.start;
    segments = List.map (transform_segment f) path.segments;
  }

let transform_ir f ir = List.map (transform_path f) ir
let mirror_ir axis ir = transform_ir (vec_mirror axis) ir
let translate_ir dir ir = transform_ir (vec_add dir) ir

(* Expression Evaluation *)

let rec eval_num env = function
  | NumLit f -> f
  | NumVar name -> (
      try lookup_num name env
      with Environment.UndefinedVariable n -> error (UndefinedVariable n))
  | NumNeg e -> -.eval_num env e
  | NumAdd (a, b) -> eval_num env a +. eval_num env b
  | NumSub (a, b) -> eval_num env a -. eval_num env b
  | NumMul (a, b) -> eval_num env a *. eval_num env b
  | NumDiv (a, b) ->
      let d = eval_num env b in
      if Float.abs d < Globals.epsilon then
        error (InvalidOperation "division by zero")
      else eval_num env a /. d

(* Basic Evaluation - cheap, for bounds/center/flow only *)

let rec eval_vec_basic env = function
  | VecLit (x, y) -> vec x y
  | VecConstruct (x, y) -> vec (eval_num env x) (eval_num env y)
  | VecVar name -> (
      try lookup_vec name env
      with Environment.UndefinedVariable n -> error (UndefinedVariable n))
  | VecCenter sk -> compute_center (eval_sketch_basic env sk)
  | VecAdd (a, b) -> vec_add (eval_vec_basic env a) (eval_vec_basic env b)
  | VecSub (a, b) -> vec_sub (eval_vec_basic env a) (eval_vec_basic env b)
  | VecScale (v, n) -> vec_scale (eval_vec_basic env v) (eval_num env n)

and eval_sketch_basic env = function
  | Primitive prim -> eval_primitive_basic env prim
  | SketchVar name -> (
      try eval_sketch_basic env (lookup_sketch name env)
      with Environment.UndefinedVariable n -> error (UndefinedVariable n))
  | SketchList sks -> List.concat_map (eval_sketch_basic env) sks
  | MirrorSketch (sk, axis) ->
      let axis_vec = eval_vec_basic env axis in
      let sk_basic_ir = eval_sketch_basic env sk in
      let sk_center = compute_center sk_basic_ir in
      let sk_normalized =
        translate_ir (vec_scale sk_center (-1.0)) sk_basic_ir
      in
      let sk_mirrored = mirror_ir axis_vec sk_normalized in
      translate_ir sk_center sk_mirrored

and eval_primitive_basic env = function
  | Dot v ->
      let p = eval_vec_basic env v in
      let r = 0.5 in
      let pts =
        [
          vec (p.x +. r) p.y;
          vec p.x (p.y +. r);
          vec (p.x -. r) p.y;
          vec p.x (p.y -. r);
          vec (p.x +. r) p.y;
        ]
      in
      [ { start = List.hd pts; segments = points_to_segments pts } ]
  | Dash v ->
      let p = eval_vec_basic env v in
      let p0, p1 = (vec (p.x -. 1.0) p.y, vec (p.x +. 1.0) p.y) in
      [ { start = p0; segments = [ { p0; p1 } ] } ]
  | Stroke (v0, via, v1) ->
      let p0 = eval_vec_basic env v0 in
      let p1 = eval_vec_basic env v1 in
      let via_pts = List.map (eval_vec_basic env) via in
      let pts = [ p0 ] @ via_pts @ [ p1 ] in
      [ { start = p0; segments = points_to_segments pts } ]

(* Flow Collection
   TODO: this seems innefficient. why are we collecting flows
   of statments and not just the final-non-flow-segments?
   currently we are evaluating strokes and 
 *)

let rec pairs = function
  | a :: (b :: _ as rest) -> (a, b) :: pairs rest
  | _ -> []

let rec collect_flow env = function
  | Primitive (Stroke (v0, via, v1)) ->
      let pts =
        [ eval_vec_basic env v0 ]
        @ List.map (eval_vec_basic env) via
        @ [ eval_vec_basic env v1 ]
      in
      List.map (fun (a, b) -> make_flow_source a b) (pairs pts)
  | Primitive _ -> []
  | SketchVar name -> (
      try collect_flow env (lookup_sketch name env) with _ -> [])
  | SketchList sks -> List.concat_map (collect_flow env) sks
  | MirrorSketch _ -> [] (* todo: evaluate *)

(* Full Evaluation - produces final IR with splines flattened *)

let rec eval_vec env flow = function
  | VecLit (x, y) -> vec x y
  | VecConstruct (x, y) -> vec (eval_num env x) (eval_num env y)
  | VecVar name -> (
      try lookup_vec name env
      with Environment.UndefinedVariable n -> error (UndefinedVariable n))
  | VecCenter sk -> compute_center (eval_sketch_basic env sk)
  | VecAdd (a, b) -> vec_add (eval_vec env flow a) (eval_vec env flow b)
  | VecSub (a, b) -> vec_sub (eval_vec env flow a) (eval_vec env flow b)
  | VecScale (v, n) -> vec_scale (eval_vec env flow v) (eval_num env n)

let eval_primitive env noise flow = function
  | Dot v ->
      let p = jitter_vec noise (eval_vec env flow v) in
      let r = 0.5 in
      let pts =
        [
          vec (p.x +. r) p.y;
          vec p.x (p.y +. r);
          vec (p.x -. r) p.y;
          vec p.x (p.y -. r);
          vec (p.x +. r) p.y;
        ]
      in
      [ { start = List.hd pts; segments = points_to_segments pts } ]
  | Dash v ->
      let p = jitter_vec noise (eval_vec env flow v) in
      let dir = sample_flow_field flow p in
      let p0 = jitter_vec noise (vec (p.x -. dir.x) (p.y -. dir.y)) in
      let p1 = jitter_vec noise (vec (p.x +. dir.x) (p.y +. dir.y)) in
      [ { start = p0; segments = [ { p0; p1 } ] } ]
  | Stroke (v0, via, v1) ->
      let p0 = jitter_vec noise (eval_vec env flow v0) in
      let p1 = jitter_vec noise (eval_vec env flow v1) in
      let via_pts =
        List.map (fun v -> jitter_vec noise (eval_vec env flow v)) via
      in
      let control_pts = [ p0 ] @ via_pts @ [ p1 ] in
      let samples =
        match noise with
        | NoiseTrace -> 12
        | NoiseDraw -> 10
        | NoiseScribble -> 8
      in
      let segments =
        spline_to_segments ~samples_per_span:samples noise control_pts
      in
      let start = match segments with [] -> p0 | s :: _ -> s.p0 in
      [ { start; segments } ]

let rec eval_sketch env noise flow = function
  | Primitive prim -> eval_primitive env noise flow prim
  | SketchVar name -> eval_sketch env noise flow (lookup_sketch name env)
  | SketchList sks -> List.concat_map (eval_sketch env noise flow) sks
  | MirrorSketch (sk, axis) ->
      let axis_vec = eval_vec env flow axis in
      let sk_ir = eval_sketch env noise flow sk in
      let sk_center = compute_center sk_ir in
      let sk_normalized = translate_ir (vec_scale sk_center (-1.0)) sk_ir in
      let sk_mirrored = mirror_ir axis_vec sk_normalized in
      translate_ir sk_center sk_mirrored

(* Statement Evaluation *)

let eval_stmt env flow = function
  | LetNum (name, e) -> (bind name (VNum (eval_num env e)) env, flow, None)
  | LetVec (name, e) -> (bind name (VVec (eval_vec_basic env e)) env, flow, None)
  | LetSketch (name, e) -> (bind name (VSketch e) env, flow, None)
  | Scribble e ->
      let f = flow @ collect_flow env e in
      (env, f, Some (eval_sketch env NoiseScribble f e))
  | Draw e ->
      let f = flow @ collect_flow env e in
      (env, f, Some (eval_sketch env NoiseDraw f e))
  | Trace e ->
      let f = flow @ collect_flow env e in
      (env, f, Some (eval_sketch env NoiseTrace f e))

(* Compilation *)

let compile program =
  let rec go env flow acc = function
    | [] -> List.concat (List.rev acc)
    | stmt :: rest ->
        let env', flow', ir = eval_stmt env flow stmt in
        let acc' = match ir with Some i -> i :: acc | None -> acc in
        go env' flow' acc' rest
  in
  go empty_env [] [] program

let compile_safe program =
  try Ok (compile program) with CompileError e -> Error e

let format_error = function
  | UndefinedVariable name -> Printf.sprintf "Undefined variable: %s" name
  | InvalidOperation msg -> Printf.sprintf "Invalid operation: %s" msg

(* Debug / Pretty Printing *)

let vec_str v = Printf.sprintf "(%.3f, %.3f)" v.x v.y
let seg_str s = Printf.sprintf "%s -> %s" (vec_str s.p0) (vec_str s.p1)

let path_str p =
  Printf.sprintf "start: %s\n  %s" (vec_str p.start)
    (String.concat "\n  " (List.map seg_str p.segments))

let ir_to_string ir = String.concat "\n" (List.map path_str ir)

let bounds_to_string b =
  Printf.sprintf "x=[%.2f,%.2f] y=[%.2f,%.2f]" b.min_x b.max_x b.min_y b.max_y

let ir_stats ir =
  let seg_count =
    List.fold_left (fun acc p -> acc + List.length p.segments) 0 ir
  in
  Printf.sprintf "Paths: %d, Segments: %d, %s" (List.length ir) seg_count
    (bounds_to_string (compute_bounds ir))
