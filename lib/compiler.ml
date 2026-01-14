(* Sketch DSL Compiler *)
open Ast

(*** Intermediate Representation ***)

type segment = MoveTo of vec | LineTo of vec
type path = { start : vec; segments : segment list }
type ir = path list
type bounds = { min_x : float; max_x : float; min_y : float; max_y : float }
type noise_level = NoiseScribble | NoiseDraw | NoiseTrace

(*** Compiler Errors ***)

type compile_error =
  | UndefinedVariable of string
  | TypeMismatch of string
  | InvalidOperation of string (* adding this - all values are constant! *)

exception CompileError of compile_error (* id like to add *)

let error e = raise (CompileError e)

let format_error = function
  | UndefinedVariable name -> Printf.sprintf "Undefined variable: %s" name
  | TypeMismatch msg -> Printf.sprintf "Type mismatch: %s" msg
  | InvalidOperation msg -> Printf.sprintf "Invalid operation: %s" msg

(*** Random/Noise ***)

let () = Random.init 42
let random_jitter magnitude = Random.float (2.0 *. magnitude) -. magnitude

let jitter_vec noise v =
  match noise with
  | NoiseTrace -> v
  | NoiseDraw ->
      let mag = 0.05 in
      { x = v.x +. random_jitter mag; y = v.y +. random_jitter mag }
  | NoiseScribble ->
      let mag = 0.1 in
      { x = v.x +. random_jitter mag; y = v.y +. random_jitter mag }

(*** Environment ***)

type value = VNum of float | VVec of vec | VSketch of sketch_expr

module Env = Map.Make (String)

type env = value Env.t

let type_of_value = function
  | VNum _ -> "number"
  | VVec _ -> "vec"
  | VSketch _ -> "sketch"

let empty_env : env = Env.empty
let bind name value env = Env.add name value env

let lookup name env =
  match Env.find_opt name env with
  | Some v -> v
  | None -> error (UndefinedVariable name)

let lookup_num name env =
  match lookup name env with
  | VNum f -> f
  | v ->
      error
        (TypeMismatch
           (Printf.sprintf "%s is a '%s' not a 'number'" name (type_of_value v)))

let lookup_vec name env =
  match lookup name env with
  | VVec v -> v
  | v ->
      error
        (TypeMismatch
           (Printf.sprintf "%s is a '%s' not a 'vec'" name (type_of_value v)))

let lookup_sketch name env =
  match lookup name env with
  | VSketch sk -> sk
  | v ->
      error
        (TypeMismatch
           (Printf.sprintf "%s is a '%s' not a 'sketch'" name (type_of_value v)))

(*** Vector Operations ***)

let vec x y : vec = { x; y }
let vec_add a b = { x = a.x +. b.x; y = a.y +. b.y }
let vec_sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let vec_scale v s = { x = v.x *. s; y = v.y *. s }
let vec_length v = Float.sqrt ((v.x *. v.x) +. (v.y *. v.y))

let vec_normalize v =
  let len = vec_length v in
  if len < 1e-10 then { x = 1.0; y = 0.0 }
  else { x = v.x /. len; y = v.y /. len }

let vec_distance a b =
  let dx = b.x -. a.x in
  let dy = b.y -. a.y in
  Float.sqrt ((dx *. dx) +. (dy *. dy))

let vec_lerp a b t =
  { x = a.x +. (t *. (b.x -. a.x)); y = a.y +. (t *. (b.y -. a.y)) }

(* Project point p onto line segment ab, return closest point and parameter t *)
let point_to_segment_closest p a b =
  let ab = vec_sub b a in
  let ap = vec_sub p a in
  let len_sq = (ab.x *. ab.x) +. (ab.y *. ab.y) in
  if len_sq < 1e-10 then (a, 0.0)
  else
    let t =
      Float.max 0.0
        (Float.min 1.0 (((ap.x *. ab.x) +. (ap.y *. ab.y)) /. len_sq))
    in
    let closest = vec_add a (vec_scale ab t) in
    (closest, t)

(*** Catmull-Rom Spline Evaluation ***)

let catmull_rom_eval p0 p1 p2 p3 t =
  let t2 = t *. t in
  let t3 = t2 *. t in
  let x =
    0.5
    *. ((2.0 *. p1.x)
       +. ((-.p0.x +. p2.x) *. t)
       +. (((2.0 *. p0.x) -. (5.0 *. p1.x) +. (4.0 *. p2.x) -. p3.x) *. t2)
       +. ((-.p0.x +. (3.0 *. p1.x) -. (3.0 *. p2.x) +. p3.x) *. t3))
  in
  let y =
    0.5
    *. ((2.0 *. p1.y)
       +. ((-.p0.y +. p2.y) *. t)
       +. (((2.0 *. p0.y) -. (5.0 *. p1.y) +. (4.0 *. p2.y) -. p3.y) *. t2)
       +. ((-.p0.y +. (3.0 *. p1.y) -. (3.0 *. p2.y) +. p3.y) *. t3))
  in
  { x; y }

let flatten_catmull_rom_segment p0 p1 p2 p3 num_segments =
  let rec go i acc =
    if i > num_segments then acc
    else
      let t = float_of_int i /. float_of_int num_segments in
      let pt = catmull_rom_eval p0 p1 p2 p3 t in
      go (i + 1) (LineTo pt :: acc)
  in
  List.rev (go 1 [])

let spline_to_segments ?(segments_per_span = 8) points =
  match points with
  | [] | [ _ ] -> []
  | [ _; p1 ] -> [ LineTo p1 ]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      let get i =
        if i < 0 then vec_sub arr.(0) (vec_sub arr.(1) arr.(0))
        else if i >= n then
          vec_add arr.(n - 1) (vec_sub arr.(n - 1) arr.(n - 2))
        else arr.(i)
      in
      let segments = ref [] in
      for i = 0 to n - 2 do
        let segs =
          flatten_catmull_rom_segment
            (get (i - 1))
            (get i)
            (get (i + 1))
            (get (i + 2))
            segments_per_span
        in
        segments := List.rev_append segs !segments
      done;
      List.rev !segments

(*** Noise: Wobble Points ***)

let add_wobble_points noise p0 p1 =
  match noise with
  | NoiseTrace -> [ p0; p1 ]
  | NoiseDraw ->
      let mid = jitter_vec noise (vec_lerp p0 p1 0.5) in
      [ p0; mid; p1 ]
  | NoiseScribble ->
      let t1 = jitter_vec noise (vec_lerp p0 p1 0.25) in
      let t2 = jitter_vec noise (vec_lerp p0 p1 0.5) in
      let t3 = jitter_vec noise (vec_lerp p0 p1 0.75) in
      [ p0; t1; t2; t3; p1 ]

(*** Segment Transformations ***)

let transform_segment f = function
  | MoveTo v -> MoveTo (f v)
  | LineTo v -> LineTo (f v)

let transform_path f (path : path) : path =
  {
    start = f path.start;
    segments = List.map (transform_segment f) path.segments;
  }

let transform_ir f (ir : ir) : ir = List.map (transform_path f) ir

(*** Flow Field ***)

(* A flow source represents a directed stroke segment *)
type flow_source = {
  fs_p0 : vec; (* start point *)
  fs_p1 : vec; (* end point *)
  fs_dir : vec; (* normalized direction from p0 to p1 *)
  fs_length : float; (* length of the segment *)
}

let make_flow_source p0 p1 =
  let dir = vec_sub p1 p0 in
  let len = vec_length dir in
  {
    fs_p0 = p0;
    fs_p1 = p1;
    fs_dir =
      (if len < 1e-10 then { x = 1.0; y = 0.0 } else vec_scale dir (1.0 /. len));
    fs_length = len;
  }

(* Sample the flow field at point p
   Uses inverse-distance weighting based on closest point on each stroke segment *)
let sample_flow_field sources p =
  if sources = [] then { x = 1.0; y = 0.0 }
  else
    let sx, sy, sw =
      List.fold_left
        (fun (ax, ay, aw) src ->
          (* Find closest point on this stroke segment to p *)
          let closest, _ = point_to_segment_closest p src.fs_p0 src.fs_p1 in
          let dist = vec_distance p closest in
          (* Inverse square falloff, with small epsilon to avoid division by zero *)
          let w = 1.0 /. (1.0 +. (dist *. dist)) in
          (ax +. (src.fs_dir.x *. w), ay +. (src.fs_dir.y *. w), aw +. w))
        (0.0, 0.0, 0.0) sources
    in
    if sw < 1e-10 then { x = 1.0; y = 0.0 }
    else vec_normalize { x = sx /. sw; y = sy /. sw }

(*** Expression Evaluation ***)

let rec eval_num env (expr : num_expr) : float =
  match expr with
  | NumLit f -> f
  | NumVar name -> lookup_num name env
  | NumNeg e -> -.eval_num env e
  | NumAdd (a, b) -> eval_num env a +. eval_num env b
  | NumSub (a, b) -> eval_num env a -. eval_num env b
  | NumMul (a, b) -> eval_num env a *. eval_num env b
  | NumDiv (a, b) ->
      let divisor = eval_num env b in
      if Float.abs divisor < 1e-10 then
        error (InvalidOperation "Division by zero")
      else eval_num env a /. divisor

(* Vector evaluation with flow sources available *)
and eval_vec env flow_sources (expr : vec_expr) : vec =
  match expr with
  | VecLit (x, y) -> vec x y
  | VecConstruct (x_expr, y_expr) ->
      vec (eval_num env x_expr) (eval_num env y_expr)
  | VecVar name -> lookup_vec name env
  | VecCenter sk ->
      let ir = eval_sketch_basic env sk in
      compute_center ir
  | VecAdd (a, b) ->
      vec_add (eval_vec env flow_sources a) (eval_vec env flow_sources b)
  | VecSub (a, b) ->
      vec_sub (eval_vec env flow_sources a) (eval_vec env flow_sources b)
  | VecScale (v, n) -> vec_scale (eval_vec env flow_sources v) (eval_num env n)
  | VecFlow v ->
      (* Sample the flow field at the given point, returning direction vector *)
      let p = eval_vec env flow_sources v in
      sample_flow_field flow_sources p

(* Vector evaluation without flow (for bootstrapping / collecting flow sources) *)
and eval_vec_basic env expr = eval_vec env [] expr

and compute_center (ir : ir) : vec =
  if ir = [] then vec 0.0 0.0
  else
    let b = compute_bounds ir in
    vec ((b.min_x +. b.max_x) /. 2.0) ((b.min_y +. b.max_y) /. 2.0)

and compute_bounds (ir : ir) : bounds =
  let update b v =
    {
      min_x = Float.min b.min_x v.x;
      max_x = Float.max b.max_x v.x;
      min_y = Float.min b.min_y v.y;
      max_y = Float.max b.max_y v.y;
    }
  in
  let segment_bounds b = function
    | MoveTo v -> update b v
    | LineTo v -> update b v
  in
  let path_bounds b path =
    let b = update b path.start in
    List.fold_left segment_bounds b path.segments
  in
  let init =
    {
      min_x = Float.infinity;
      max_x = Float.neg_infinity;
      min_y = Float.infinity;
      max_y = Float.neg_infinity;
    }
  in
  List.fold_left path_bounds init ir

(* Basic sketch evaluation (no noise, no flow) - used for bounds, center, etc. *)
and eval_sketch_basic env (expr : sketch_expr) : ir =
  match expr with
  | Primitive prim -> eval_primitive_basic env prim
  | SketchVar name -> eval_sketch_basic env (lookup_sketch name env)
  | SketchList sks -> List.concat_map (eval_sketch_basic env) sks

and eval_primitive_basic env (prim : primitive) : ir =
  match prim with
  | Dot v ->
      let p = eval_vec_basic env v in
      let r = 0.5 in
      [
        {
          start = { x = p.x +. r; y = p.y };
          segments =
            [
              LineTo { x = p.x; y = p.y +. r };
              LineTo { x = p.x -. r; y = p.y };
              LineTo { x = p.x; y = p.y -. r };
              LineTo { x = p.x +. r; y = p.y };
            ];
        };
      ]
  | Dash v ->
      let p = eval_vec_basic env v in
      let half = 1.0 in
      [
        {
          start = { x = p.x -. half; y = p.y };
          segments = [ LineTo { x = p.x +. half; y = p.y } ];
        };
      ]
  | Stroke (v0, via, v1) ->
      let p0 = eval_vec_basic env v0 in
      let p1 = eval_vec_basic env v1 in
      let via_pts = List.map (eval_vec_basic env) via in
      let all_pts = [ p0 ] @ via_pts @ [ p1 ] in
      let segments = spline_to_segments all_pts in
      [ { start = p0; segments } ]

(*** Flow Source Collection ***)

(* Helper to create pairs from a list *)
let rec pairs = function
  | a :: (b :: _ as rest) -> (a, b) :: pairs rest
  | _ -> []

(* Collect all flow sources from strokes in a sketch expression
   This is done in the first pass before full evaluation *)
let rec collect_flow_sources env (expr : sketch_expr) : flow_source list =
  match expr with
  | Primitive (Stroke (v0, via, v1)) ->
      let p0 = eval_vec_basic env v0 in
      let p1 = eval_vec_basic env v1 in
      let via_pts = List.map (eval_vec_basic env) via in
      (* Create flow sources for each segment of the stroke *)
      let all_pts = [ p0 ] @ via_pts @ [ p1 ] in
      List.map (fun (a, b) -> make_flow_source a b) (pairs all_pts)
  | Primitive _ -> []
  | SketchVar name -> collect_flow_sources env (lookup_sketch name env)
  | SketchList sks -> List.concat_map (collect_flow_sources env) sks

(*** Full Evaluation with Noise and Flow ***)

let rec eval_sketch env noise flow_sources (expr : sketch_expr) : ir =
  match expr with
  | Primitive prim -> eval_primitive env noise flow_sources prim
  | SketchVar name ->
      eval_sketch env noise flow_sources (lookup_sketch name env)
  | SketchList sks -> List.concat_map (eval_sketch env noise flow_sources) sks

and eval_primitive env noise flow_sources (prim : primitive) : ir =
  match prim with
  | Dot v ->
      let p = jitter_vec noise (eval_vec env flow_sources v) in
      let r = 0.5 in
      [
        {
          start = { x = p.x +. r; y = p.y };
          segments =
            [
              LineTo { x = p.x; y = p.y +. r };
              LineTo { x = p.x -. r; y = p.y };
              LineTo { x = p.x; y = p.y -. r };
              LineTo { x = p.x +. r; y = p.y };
            ];
        };
      ]
  | Dash v ->
      let p = jitter_vec noise (eval_vec env flow_sources v) in
      let dir = sample_flow_field flow_sources p in
      let half = 1.0 in
      let p0 =
        jitter_vec noise
          { x = p.x -. (half *. dir.x); y = p.y -. (half *. dir.y) }
      in
      let p1 =
        jitter_vec noise
          { x = p.x +. (half *. dir.x); y = p.y +. (half *. dir.y) }
      in
      [ { start = p0; segments = [ LineTo p1 ] } ]
  | Stroke (v0, via, v1) ->
      let p0 = jitter_vec noise (eval_vec env flow_sources v0) in
      let p1 = jitter_vec noise (eval_vec env flow_sources v1) in
      let via_pts =
        List.map (fun v -> jitter_vec noise (eval_vec env flow_sources v)) via
      in
      let all_pts =
        match noise with
        | NoiseTrace -> [ p0 ] @ via_pts @ [ p1 ]
        | NoiseDraw | NoiseScribble ->
            let rec add_wobbles prev = function
              | [] -> [ prev ]
              | next :: rest ->
                  let wobbled = add_wobble_points noise prev next in
                  let trimmed =
                    match List.rev wobbled with
                    | _ :: t -> List.rev t
                    | [] -> []
                  in
                  trimmed @ add_wobbles next rest
            in
            add_wobbles p0 (via_pts @ [ p1 ])
      in
      let segments = spline_to_segments all_pts in
      [ { start = p0; segments } ]

(*** Statement Evaluation ***)

(* Evaluate a statement, threading flow_sources through the program
   Returns: updated env, updated flow_sources, optional IR output *)
let eval_statement env flow_sources (stmt : statement) :
    env * flow_source list * ir option =
  match stmt with
  | LetNum (name, expr) ->
      (bind name (VNum (eval_num env expr)) env, flow_sources, None)
  | LetVec (name, expr) ->
      (bind name (VVec (eval_vec_basic env expr)) env, flow_sources, None)
  | LetSketch (name, expr) -> (bind name (VSketch expr) env, flow_sources, None)
  | Scribble expr ->
      (* Collect any new flow sources from inline sketches *)
      let new_sources = collect_flow_sources env expr in
      let all_sources = flow_sources @ new_sources in
      (env, all_sources, Some (eval_sketch env NoiseScribble all_sources expr))
  | Draw expr ->
      let new_sources = collect_flow_sources env expr in
      let all_sources = flow_sources @ new_sources in
      (env, all_sources, Some (eval_sketch env NoiseDraw all_sources expr))
  | Trace expr ->
      let new_sources = collect_flow_sources env expr in
      let all_sources = flow_sources @ new_sources in
      (env, all_sources, Some (eval_sketch env NoiseTrace all_sources expr))

(*** Program Compilation ***)

let compile (program : program) : ir =
  let rec go env flow_sources acc = function
    | [] -> List.concat (List.rev acc)
    | stmt :: rest ->
        let env', flow_sources', ir_opt =
          eval_statement env flow_sources stmt
        in
        let acc' = match ir_opt with Some ir -> ir :: acc | None -> acc in
        go env' flow_sources' acc' rest
  in
  go empty_env [] [] program

let compile_safe (program : program) : (ir, compile_error) result =
  try Ok (compile program) with CompileError e -> Error e

(*** Pretty Printing ***)

let vec_to_string v = Printf.sprintf "(%.3f, %.3f)" v.x v.y

let segment_to_string = function
  | MoveTo v -> Printf.sprintf "M %s" (vec_to_string v)
  | LineTo v -> Printf.sprintf "L %s" (vec_to_string v)

let path_to_string path =
  let start_str = Printf.sprintf "M %s" (vec_to_string path.start) in
  let seg_strs = List.map segment_to_string path.segments in
  String.concat " " (start_str :: seg_strs)

let ir_to_string ir = String.concat "\n" (List.map path_to_string ir)

let bounds_to_string b =
  Printf.sprintf "x=[%.2f, %.2f] y=[%.2f, %.2f]" b.min_x b.max_x b.min_y b.max_y

let ir_stats ir =
  let num_paths = List.length ir in
  let num_segments =
    List.fold_left (fun acc p -> acc + List.length p.segments) 0 ir
  in
  Printf.sprintf "Paths: %d, Segments: %d, %s" num_paths num_segments
    (bounds_to_string (compute_bounds ir))
