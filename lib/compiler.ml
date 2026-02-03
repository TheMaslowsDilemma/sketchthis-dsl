(*
-----------------------------------------------------------
compiler.ml
-----------------------------------------------------------
*)

open Ast
open Ir
open Vector
open Splines
open Environment

type bounds = { min_x : float; max_x : float; min_y : float; max_y : float }
type compile_error = { message : string; position : Lexer.position }

exception CompileError of compile_error

let error pos msg = raise (CompileError { message = msg; position = pos })

(* Type extractors *)

let type_name = function
  | VNum _ -> "number"
  | VVec _ -> "vec"
  | VSketch _ -> "sketch"

let as_num pos = function
  | VNum f -> f
  | v -> error pos (Printf.sprintf "expected number, got %s" (type_name v))

let as_vec pos = function
  | VVec v -> v
  | v -> error pos (Printf.sprintf "expected vec, got %s" (type_name v))

let as_ir pos = function
  | VSketch ir -> ir
  | v -> error pos (Printf.sprintf "expected sketch, got %s" (type_name v))

(* Flow field from accumulated IR *)

type flow_source = { origin : vec; target : vec; dir : vec }

let make_flow_source p0 p1 =
  let d = vec_sub p1 p0 in
  let len = vec_length d in
  let dir =
    if len < Globals.epsilon then vec 1.0 0.0 else vec_scale (1.0 /. len) d
  in
  { origin = p0; target = p1; dir }

let flow_sources_of_ir ir =
  List.concat_map
    (fun (p : path) ->
      List.map (fun (s : segment) -> make_flow_source s.p0 s.p1) p.segments)
    ir

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

(* Bounds and center *)

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

(* IR transformations *)

let transform_segment f s = { p0 = f s.p0; p1 = f s.p1 }

let transform_path f p =
  { start = f p.start; segments = List.map (transform_segment f) p.segments }

let transform_ir f ir = List.map (transform_path f) ir
let translate_ir d ir = transform_ir (vec_add d) ir

let transform_centered_ir f ir =
  let c = compute_center ir in
  ir |> translate_ir (vec_scale (-1.0) c) |> transform_ir f |> translate_ir c

let rotate_ir deg ir =
  let a = deg *. Float.pi /. 180.0 in
  let cos_a = Float.cos a and sin_a = Float.sin a in
  transform_centered_ir
    (fun v ->
      vec ((v.x *. cos_a) -. (v.y *. sin_a)) ((v.x *. sin_a) +. (v.y *. cos_a)))
    ir

(* Evaluation *)

let rec eval env noise acc (e : expr) =
  let pos = e.loc.start_loc in
  match e.txt with
  | Lit f -> VNum f
  | Var name -> (
      try lookup name env
      with UndefinedVariable n -> error pos (Printf.sprintf "undefined: %s" n))
  | Vec (a, b) ->
      VVec
        (vec
           (as_num a.loc.start_loc (eval env noise acc a))
           (as_num b.loc.start_loc (eval env noise acc b)))
  | Neg e1 -> (
      match eval env noise acc e1 with
      | VNum f -> VNum (-.f)
      | VVec v -> VVec (vec_scale (-1.0) v)
      | _ -> error pos "cannot negate a sketch")
  | Add (a, b) -> (
      match (eval env noise acc a, eval env noise acc b) with
      | VNum x, VNum y -> VNum (x +. y)
      | VVec u, VVec v -> VVec (vec_add u v)
      | _ -> error pos "type mismatch in +")
  | Sub (a, b) -> (
      match (eval env noise acc a, eval env noise acc b) with
      | VNum x, VNum y -> VNum (x -. y)
      | VVec u, VVec v -> VVec (vec_sub u v)
      | _ -> error pos "type mismatch in -")
  | Mul (a, b) -> (
      match (eval env noise acc a, eval env noise acc b) with
      | VNum x, VNum y -> VNum (x *. y)
      | VVec v, VNum s | VNum s, VVec v -> VVec (vec_scale s v)
      | _ -> error pos "type mismatch in *")
  | Div (a, b) ->
      let x = as_num a.loc.start_loc (eval env noise acc a) in
      let y = as_num b.loc.start_loc (eval env noise acc b) in
      if Float.abs y < Globals.epsilon then
        error b.loc.start_loc "division by zero"
      else VNum (x /. y)
  | CenterOf e1 ->
      VVec (compute_center (as_ir e1.loc.start_loc (eval env noise acc e1)))
  | Dot v ->
      let p =
        jitter_vec noise (as_vec v.loc.start_loc (eval env noise acc v))
      in
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
      VSketch [ { start = List.hd pts; segments = points_to_segments pts } ]
  | Dash v ->
      let p =
        jitter_vec noise (as_vec v.loc.start_loc (eval env noise acc v))
      in
      let dir = sample_flow_field (flow_sources_of_ir acc) p in
      let p0 = jitter_vec noise (vec (p.x -. dir.x) (p.y -. dir.y)) in
      let p1 = jitter_vec noise (vec (p.x +. dir.x) (p.y +. dir.y)) in
      VSketch [ { start = p0; segments = [ { p0; p1 } ] } ]
  | Segments pts ->
      if List.length pts < 2 then
        error pos "segments require more than one point";
      let vecmaker (e : expr) =
        jitter_vec noise (as_vec e.loc.start_loc (eval env noise acc e))
      in
      let vecs = List.map vecmaker pts in
      VSketch [ { start = List.hd vecs; segments = points_to_segments vecs } ]
  | Splines pts ->
      if List.length pts < 2 then
        error pos "splines require more than one point";
      let vecmaker (e : expr) =
        jitter_vec noise (as_vec e.loc.start_loc (eval env noise acc e))
      in
      let vecs = List.map vecmaker pts in
      let samples =
        match noise with
        | NoiseTrace -> 12
        | NoiseDraw -> 10
        | NoiseScribble -> 8
      in
      let segs = spline_to_segments ~samples_per_span:samples noise vecs in
      let start = match segs with [] -> List.hd vecs | s :: _ -> s.p0 in
      VSketch [ { start; segments = segs } ]
  | SketchList items ->
      let _, ir =
        List.fold_left
          (fun (cur_acc, result) (item : expr) ->
            let item_ir =
              as_ir item.loc.start_loc (eval env noise cur_acc item)
            in
            (cur_acc @ item_ir, result @ item_ir))
          (acc, []) items
      in
      VSketch ir
  | Mirror (sk, axis) ->
      let sk_ir = as_ir sk.loc.start_loc (eval env noise acc sk) in
      let axis_vec = as_vec axis.loc.start_loc (eval env noise acc axis) in
      VSketch (transform_centered_ir (vec_mirror axis_vec) sk_ir)
  | Rotate (sk, angle) ->
      let sk_ir = as_ir sk.loc.start_loc (eval env noise acc sk) in
      let deg = as_num angle.loc.start_loc (eval env noise acc angle) in
      VSketch (rotate_ir deg sk_ir)
  | Translate (sk, v) ->
      let sk_ir = as_ir sk.loc.start_loc (eval env noise acc sk) in
      let d = as_vec v.loc.start_loc (eval env noise acc v) in
      VSketch (translate_ir d sk_ir)
  | Scale (sk, n) ->
      let sk_ir = as_ir sk.loc.start_loc (eval env noise acc sk) in
      let s = as_num n.loc.start_loc (eval env noise acc n) in
      VSketch (transform_centered_ir (vec_scale s) sk_ir)
  | At (sk, target) ->
      let sk_ir = as_ir sk.loc.start_loc (eval env noise acc sk) in
      let t = as_vec target.loc.start_loc (eval env noise acc target) in
      VSketch (translate_ir (vec_sub t (compute_center sk_ir)) sk_ir)

(* Statement evaluation *)

let eval_stmt env acc (s : statement) =
  let pos = s.loc.start_loc in
  match s.txt with
  | Let (name, e) -> (bind name (eval env NoiseTrace acc e) env, None)
  | Draw e -> (env, Some (as_ir pos (eval env NoiseDraw acc e)))
  | Scribble e -> (env, Some (as_ir pos (eval env NoiseScribble acc e)))
  | Trace e -> (env, Some (as_ir pos (eval env NoiseTrace acc e)))

(* Compilation *)

let compile program =
  let rec go env acc = function
    | [] -> acc
    | stmt :: rest ->
        let env', ir = eval_stmt env acc stmt in
        go env' (match ir with Some i -> acc @ i | None -> acc) rest
  in
  go empty_env [] program

let compile_safe program =
  try Ok (compile program) with CompileError e -> Error e

let format_error e =
  Printf.sprintf "{ \"msg\": \"%s\", \"line\": %d, \"col\": %d }" e.message
    e.position.line e.position.column

(* Debug *)

let vec_str v = Printf.sprintf "(%.3f, %.3f)" v.x v.y
let seg_str s = Printf.sprintf "%s -> %s" (vec_str s.p0) (vec_str s.p1)

let path_str p =
  Printf.sprintf "start: %s\n  %s" (vec_str p.start)
    (String.concat "\n  " (List.map seg_str p.segments))

let ir_to_string ir = String.concat "\n" (List.map path_str ir)

let bounds_to_string b =
  Printf.sprintf "x=[%.2f,%.2f] y=[%.2f,%.2f]" b.min_x b.max_x b.min_y b.max_y

let ir_stats ir =
  let n = List.fold_left (fun a p -> a + List.length p.segments) 0 ir in
  Printf.sprintf "Paths: %d, Segments: %d, %s" (List.length ir) n
    (bounds_to_string (compute_bounds ir))
