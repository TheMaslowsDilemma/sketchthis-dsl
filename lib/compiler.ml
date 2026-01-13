(* Sketch DSL Compiler *)
open Ast

(*** Intermediate Representation ***)

type segment =
  | MoveTo of vec
  | LineTo of vec

type path = { start : vec; segments : segment list }
type ir = path list
type bounds = { min_x : float; max_x : float; min_y : float; max_y : float }

type noise_level =
  | NoiseScribble
  | NoiseDraw
  | NoiseTrace

(*** Compiler Errors ***)

type compile_error =
  | UndefinedVariable of string
  | TypeMismatch of string
  | InvalidOperation of string

exception CompileError of compile_error

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
  | v -> error (TypeMismatch (Printf.sprintf "%s is a '%s' not a 'number'" name (type_of_value v)))

let lookup_vec name env =
  match lookup name env with
  | VVec v -> v
  | v -> error (TypeMismatch (Printf.sprintf "%s is a '%s' not a 'vec'" name (type_of_value v)))

let lookup_sketch name env =
  match lookup name env with
  | VSketch sk -> sk
  | v -> error (TypeMismatch (Printf.sprintf "%s is a '%s' not a 'sketch'" name (type_of_value v)))

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

(*** Catmull-Rom Spline Evaluation ***)

(* Evaluate Catmull-Rom spline at parameter t given 4 control points *)
let catmull_rom_eval p0 p1 p2 p3 t =
  let t2 = t *. t in
  let t3 = t2 *. t in
  let x = 0.5 *. (
    (2.0 *. p1.x) +.
    (-.p0.x +. p2.x) *. t +.
    (2.0 *. p0.x -. 5.0 *. p1.x +. 4.0 *. p2.x -. p3.x) *. t2 +.
    (-.p0.x +. 3.0 *. p1.x -. 3.0 *. p2.x +. p3.x) *. t3
  ) in
  let y = 0.5 *. (
    (2.0 *. p1.y) +.
    (-.p0.y +. p2.y) *. t +.
    (2.0 *. p0.y -. 5.0 *. p1.y +. 4.0 *. p2.y -. p3.y) *. t2 +.
    (-.p0.y +. 3.0 *. p1.y -. 3.0 *. p2.y +. p3.y) *. t3
  ) in
  { x; y }

(* Flatten a Catmull-Rom segment to line segments *)
let flatten_catmull_rom_segment p0 p1 p2 p3 num_segments =
  let rec go i acc =
    if i > num_segments then acc
    else
      let t = float_of_int i /. float_of_int num_segments in
      let pt = catmull_rom_eval p0 p1 p2 p3 t in
      go (i + 1) (LineTo pt :: acc)
  in
  List.rev (go 1 [])

(* Convert a list of points to line segments via Catmull-Rom interpolation
   segments_per_span controls smoothness *)
let spline_to_segments ?(segments_per_span = 8) points =
  match points with
  | [] | [_] -> []
  | [_; p1] -> [LineTo p1]
  | _ ->
      let arr = Array.of_list points in
      let n = Array.length arr in
      (* Phantom endpoints for natural spline behavior *)
      let get i =
        if i < 0 then vec_sub arr.(0) (vec_sub arr.(1) arr.(0))
        else if i >= n then vec_add arr.(n-1) (vec_sub arr.(n-1) arr.(n-2))
        else arr.(i)
      in
      let segments = ref [] in
      for i = 0 to n - 2 do
        let segs = flatten_catmull_rom_segment
          (get (i-1)) (get i) (get (i+1)) (get (i+2))
          segments_per_span
        in
        segments := List.rev_append segs !segments
      done;
      List.rev !segments

(*** Noise: Wobble Points ***)

let add_wobble_points noise p0 p1 =
  match noise with
  | NoiseTrace -> [p0; p1]
  | NoiseDraw ->
      let mid = jitter_vec noise (vec_lerp p0 p1 0.5) in
      [p0; mid; p1]
  | NoiseScribble ->
      let t1 = jitter_vec noise (vec_lerp p0 p1 0.25) in
      let t2 = jitter_vec noise (vec_lerp p0 p1 0.5) in
      let t3 = jitter_vec noise (vec_lerp p0 p1 0.75) in
      [p0; t1; t2; t3; p1]

(*** Segment Transformations ***)

let transform_segment f = function
  | MoveTo v -> MoveTo (f v)
  | LineTo v -> LineTo (f v)

let transform_path f (path : path) : path =
  { start = f path.start;
    segments = List.map (transform_segment f) path.segments }

let transform_ir f (ir : ir) : ir = List.map (transform_path f) ir

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
      if Float.abs divisor < 1e-10 then error (InvalidOperation "Division by zero")
      else eval_num env a /. divisor

and eval_vec_basic env (expr : vec_expr) : vec =
  match expr with
  | VecLit (x, y) -> vec x y
  | VecConstruct (x_expr, y_expr) ->
      vec (eval_num env x_expr) (eval_num env y_expr)
  | VecVar name -> lookup_vec name env
  | VecCenter sk ->
      let ir = eval_sketch_basic env sk in
      compute_center ir
  | VecAdd (a, b) -> vec_add (eval_vec_basic env a) (eval_vec_basic env b)
  | VecSub (a, b) -> vec_sub (eval_vec_basic env a) (eval_vec_basic env b)
  | VecScale (v, n) -> vec_scale (eval_vec_basic env v) (eval_num env n)
  | VecFlow v -> eval_vec_basic env v

and compute_center (ir : ir) : vec =
  if ir = [] then vec 0.0 0.0
  else
    let b = compute_bounds ir in
    vec ((b.min_x +. b.max_x) /. 2.0) ((b.min_y +. b.max_y) /. 2.0)

and compute_bounds (ir : ir) : bounds =
  let update b v =
    { min_x = Float.min b.min_x v.x;
      max_x = Float.max b.max_x v.x;
      min_y = Float.min b.min_y v.y;
      max_y = Float.max b.max_y v.y }
  in
  let segment_bounds b = function
    | MoveTo v -> update b v
    | LineTo v -> update b v
  in
  let path_bounds b path =
    let b = update b path.start in
    List.fold_left segment_bounds b path.segments
  in
  let init = { min_x = Float.infinity; max_x = Float.neg_infinity;
               min_y = Float.infinity; max_y = Float.neg_infinity } in
  List.fold_left path_bounds init ir

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
      [{ start = { x = p.x +. r; y = p.y };
         segments = [
           LineTo { x = p.x; y = p.y +. r };
           LineTo { x = p.x -. r; y = p.y };
           LineTo { x = p.x; y = p.y -. r };
           LineTo { x = p.x +. r; y = p.y };
         ]}]
  | Dash v ->
      let p = eval_vec_basic env v in
      let half = 1.0 in
      [{ start = { x = p.x -. half; y = p.y };
         segments = [LineTo { x = p.x +. half; y = p.y }] }]
  | Stroke (v0, via, v1) ->
      let p0 = eval_vec_basic env v0 in
      let p1 = eval_vec_basic env v1 in
      let via_pts = List.map (eval_vec_basic env) via in
      let all_pts = [p0] @ via_pts @ [p1] in
      let segments = spline_to_segments all_pts in
      [{ start = p0; segments }]

(*** Flow Field ***)

type flow_source = { fs_p0 : vec; fs_p1 : vec; fs_dir : vec }

let rec collect_flow_sources env (expr : sketch_expr) : flow_source list =
  match expr with
  | Primitive (Stroke (v0, _, v1)) ->
      let p0 = eval_vec_basic env v0 in
      let p1 = eval_vec_basic env v1 in
      [{ fs_p0 = p0; fs_p1 = p1; fs_dir = vec_normalize (vec_sub p1 p0) }]
  | Primitive _ -> []
  | SketchVar name -> collect_flow_sources env (lookup_sketch name env)
  | SketchList sks -> List.concat_map (collect_flow_sources env) sks

let sample_flow_field sources p =
  if sources = [] then { x = 1.0; y = 0.0 }
  else
    let (sx, sy, sw) = List.fold_left (fun (ax, ay, aw) src ->
      let mid = vec_lerp src.fs_p0 src.fs_p1 0.5 in
      let dist = vec_distance p mid in
      let w = 1.0 /. (1.0 +. dist *. dist) in
      (ax +. src.fs_dir.x *. w, ay +. src.fs_dir.y *. w, aw +. w)
    ) (0.0, 0.0, 0.0) sources in
    if sw < 1e-10 then { x = 1.0; y = 0.0 }
    else vec_normalize { x = sx /. sw; y = sy /. sw }

(*** Full Evaluation with Noise ***)

let rec eval_sketch env noise flow_sources (expr : sketch_expr) : ir =
  match expr with
  | Primitive prim -> eval_primitive env noise flow_sources prim
  | SketchVar name -> eval_sketch env noise flow_sources (lookup_sketch name env)
  | SketchList sks -> List.concat_map (eval_sketch env noise flow_sources) sks

and eval_primitive env noise flow_sources (prim : primitive) : ir =
  match prim with
  | Dot v ->
      let p = jitter_vec noise (eval_vec_basic env v) in
      let r = 0.5 in
      [{ start = { x = p.x +. r; y = p.y };
         segments = [
           LineTo { x = p.x; y = p.y +. r };
           LineTo { x = p.x -. r; y = p.y };
           LineTo { x = p.x; y = p.y -. r };
           LineTo { x = p.x +. r; y = p.y };
         ]}]
  | Dash v ->
      let p = jitter_vec noise (eval_vec_basic env v) in
      let dir = sample_flow_field flow_sources p in
      let half = 1.0 in
      let p0 = jitter_vec noise { x = p.x -. half *. dir.x; y = p.y -. half *. dir.y } in
      let p1 = jitter_vec noise { x = p.x +. half *. dir.x; y = p.y +. half *. dir.y } in
      [{ start = p0; segments = [LineTo p1] }]
  | Stroke (v0, via, v1) ->
      let p0 = jitter_vec noise (eval_vec_basic env v0) in
      let p1 = jitter_vec noise (eval_vec_basic env v1) in
      let via_pts = List.map (fun v -> jitter_vec noise (eval_vec_basic env v)) via in
      let all_pts = match noise with
        | NoiseTrace -> [p0] @ via_pts @ [p1]
        | NoiseDraw | NoiseScribble ->
            let rec add_wobbles prev = function
              | [] -> [prev]
              | next :: rest ->
                  let wobbled = add_wobble_points noise prev next in
                  let trimmed = match List.rev wobbled with _ :: t -> List.rev t | [] -> [] in
                  trimmed @ add_wobbles next rest
            in
            add_wobbles p0 (via_pts @ [p1])
      in
      let segments = spline_to_segments all_pts in
      [{ start = p0; segments }]

let eval_sketch_full env noise expr =
  let flow_sources = collect_flow_sources env expr in
  eval_sketch env noise flow_sources expr

(*** Statement Evaluation ***)

let eval_statement env (stmt : statement) : env * ir option =
  match stmt with
  | LetNum (name, expr) -> (bind name (VNum (eval_num env expr)) env, None)
  | LetVec (name, expr) -> (bind name (VVec (eval_vec_basic env expr)) env, None)
  | LetSketch (name, expr) -> (bind name (VSketch expr) env, None)
  | Scribble expr -> (env, Some (eval_sketch_full env NoiseScribble expr))
  | Draw expr -> (env, Some (eval_sketch_full env NoiseDraw expr))
  | Trace expr -> (env, Some (eval_sketch_full env NoiseTrace expr))

let compile (program : program) : ir =
  let rec go env acc = function
    | [] -> List.concat (List.rev acc)
    | stmt :: rest ->
        let env', ir_opt = eval_statement env stmt in
        let acc' = match ir_opt with Some ir -> ir :: acc | None -> acc in
        go env' acc' rest
  in
  go empty_env [] program

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
  let num_segments = List.fold_left (fun acc p -> acc + List.length p.segments) 0 ir in
  Printf.sprintf "Paths: %d, Segments: %d, %s" num_paths num_segments (bounds_to_string (compute_bounds ir))