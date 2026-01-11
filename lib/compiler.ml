(* Sketch DSL Compiler *)
(* Transforms AST into a flat intermediate representation for G-code generation *)

open Ast

(* ===== Intermediate Representation ===== *)

(** A concrete 2D point (fully evaluated) *)
type point = { x: float; y: float }

(** A path segment - the basic drawing unit *)
type segment =
  | MoveTo of point                           (* Pen up, move to point *)
  | LineTo of point                           (* Draw line to point *)
  | BezierTo of point * point * point         (* Cubic bezier: ctrl1, ctrl2, end *)
  | QuadraticTo of point * point              (* Quadratic bezier: ctrl, end *)
  | ArcTo of point * point * float * float    (* Arc: center, radius, start_angle, end_angle *)

(** A path is a sequence of segments starting from an implicit origin *)
type path = {
  start: point;
  segments: segment list;
}

(** The intermediate representation - a list of paths to draw *)
type ir = path list

(** Bounding box for a shape *)
type bounds = {
  min_x: float;
  max_x: float;
  min_y: float;
  max_y: float;
}

(* ===== Compiler Errors ===== *)

type compile_error =
  | UndefinedVariable of string
  | TypeMismatch of string
  | BoundsViolation of string
  | InvalidOperation of string

exception CompileError of compile_error

let error e = raise (CompileError e)

let format_error = function
  | UndefinedVariable name -> Printf.sprintf "Undefined variable: %s" name
  | TypeMismatch msg -> Printf.sprintf "Type mismatch: %s" msg
  | BoundsViolation msg -> Printf.sprintf "Bounds violation: %s" msg
  | InvalidOperation msg -> Printf.sprintf "Invalid operation: %s" msg

(* ===== Environment for Variable Bindings ===== *)

type value =
  | VNum of float
  | VVec of point
  | VSketch of ir

module Env = Map.Make(String)

type env = value Env.t

let empty_env : env = Env.empty

let bind name value env = Env.add name value env

let lookup name env =
  match Env.find_opt name env with
  | Some v -> v
  | None -> error (UndefinedVariable name)

let lookup_num name env =
  match lookup name env with
  | VNum f -> f
  | _ -> error (TypeMismatch (Printf.sprintf "%s is not a number" name))

let lookup_vec name env =
  match lookup name env with
  | VVec p -> p
  | _ -> error (TypeMismatch (Printf.sprintf "%s is not a vec2" name))

let lookup_sketch name env =
  match lookup name env with
  | VSketch ir -> ir
  | _ -> error (TypeMismatch (Printf.sprintf "%s is not a sketch" name))

(* ===== Point/Vector Operations ===== *)

let point x y : point = { x; y }
let point_of_vec2 (v : vec2) : point = { x = v.x; y = v.y }

let point_add p1 p2 = { x = p1.x +. p2.x; y = p1.y +. p2.y }
let point_sub p1 p2 = { x = p1.x -. p2.x; y = p1.y -. p2.y }
let point_scale p s = { x = p.x *. s; y = p.y *. s }
let point_neg p = { x = -. p.x; y = -. p.y }

let point_rotate p angle_deg =
  let angle_rad = angle_deg *. Float.pi /. 180.0 in
  let cos_a = Float.cos angle_rad in
  let sin_a = Float.sin angle_rad in
  {
    x = p.x *. cos_a -. p.y *. sin_a;
    y = p.x *. sin_a +. p.y *. cos_a;
  }

let point_rotate_around p center angle_deg =
  let translated = point_sub p center in
  let rotated = point_rotate translated angle_deg in
  point_add rotated center

let point_reflect_x p = { x = -. p.x; y = p.y }
let point_reflect_y p = { x = p.x; y = -. p.y }

(** Reflect point across a line defined by two points *)
let point_reflect_line p line_p1 line_p2 =
  let dx = line_p2.x -. line_p1.x in
  let dy = line_p2.y -. line_p1.y in
  let len_sq = dx *. dx +. dy *. dy in
  if len_sq < 1e-10 then p  (* Degenerate line *)
  else
    let t = ((p.x -. line_p1.x) *. dx +. (p.y -. line_p1.y) *. dy) /. len_sq in
    let proj_x = line_p1.x +. t *. dx in
    let proj_y = line_p1.y +. t *. dy in
    { x = 2.0 *. proj_x -. p.x; y = 2.0 *. proj_y -. p.y }

(* ===== Segment Transformations ===== *)

let transform_point f = function
  | MoveTo p -> MoveTo (f p)
  | LineTo p -> LineTo (f p)
  | BezierTo (c1, c2, p) -> BezierTo (f c1, f c2, f p)
  | QuadraticTo (c, p) -> QuadraticTo (f c, f p)
  | ArcTo (center, radius, a0, a1) -> 
    (* For arcs, we transform center but radius stays as-is for now *)
    (* This is a simplification - proper arc transformation is complex *)
    ArcTo (f center, f radius, a0, a1)

let transform_path f (path : path) : path =
  { start = f path.start; segments = List.map (transform_point f) path.segments }

let transform_ir f (ir : ir) : ir =
  List.map (transform_path f) ir

(* ===== Expression Evaluation ===== *)

(** Evaluate a numeric expression *)
let rec eval_num env (expr : num_expr) : float =
  match expr with
  | NumLit f -> f
  | NumVar name -> lookup_num name env
  | NumNeg e -> -. (eval_num env e)
  | NumAdd (a, b) -> eval_num env a +. eval_num env b
  | NumSub (a, b) -> eval_num env a -. eval_num env b
  | NumMul (a, b) -> eval_num env a *. eval_num env b
  | NumDiv (a, b) -> 
    let divisor = eval_num env b in
    if Float.abs divisor < 1e-10 then
      error (InvalidOperation "Division by zero")
    else
      eval_num env a /. divisor

(** Evaluate a vector expression *)
let rec eval_vec env (expr : vec_expr) : point =
  match expr with
  | VecLit (x, y) -> point x y
  | VecConstruct (x_expr, y_expr) ->
    let x = eval_num env x_expr in
    let y = eval_num env y_expr in
    point x y
  | VecVar name -> lookup_vec name env
  | VecCenter sk -> 
    let ir = eval_sketch env sk in
    compute_center ir
  | VecAdd (a, b) ->
    let pa = eval_vec env a in
    let pb = eval_vec env b in
    point_add pa pb
  | VecSub (a, b) ->
    let pa = eval_vec env a in
    let pb = eval_vec env b in
    point_sub pa pb
  | VecScale (v, n) ->
    let pv = eval_vec env v in
    let s = eval_num env n in
    point_scale pv s

(** Compute the center (centroid) of an IR *)
and compute_center (ir : ir) : point =
  if ir = [] then point 0.0 0.0
  else
    let bounds = compute_bounds ir in
    point 
      ((bounds.min_x +. bounds.max_x) /. 2.0)
      ((bounds.min_y +. bounds.max_y) /. 2.0)

(** Compute bounding box of IR *)
and compute_bounds (ir : ir) : bounds =
  let update_bounds b p =
    { min_x = Float.min b.min_x p.x;
      max_x = Float.max b.max_x p.x;
      min_y = Float.min b.min_y p.y;
      max_y = Float.max b.max_y p.y }
  in
  let segment_bounds b = function
    | MoveTo p -> update_bounds b p
    | LineTo p -> update_bounds b p
    | BezierTo (c1, c2, p) -> 
      update_bounds (update_bounds (update_bounds b c1) c2) p
    | QuadraticTo (c, p) ->
      update_bounds (update_bounds b c) p
    | ArcTo (center, radius, _, _) ->
      (* Approximate: use center +/- radius *)
      let b = update_bounds b { x = center.x -. radius.x; y = center.y -. radius.y } in
      update_bounds b { x = center.x +. radius.x; y = center.y +. radius.y }
  in
  let path_bounds b (path : path) =
    let b = update_bounds b path.start in
    List.fold_left segment_bounds b path.segments
  in
  let init = { min_x = Float.infinity; max_x = Float.neg_infinity;
               min_y = Float.infinity; max_y = Float.neg_infinity } in
  List.fold_left path_bounds init ir

(** Evaluate a sketch expression to IR *)
and eval_sketch env (expr : sketch_expr) : ir =
  match expr with
  | Primitive prim -> eval_primitive env prim
  | SketchVar name -> lookup_sketch name env
  | Scale (sk, n, along) -> eval_scale env sk n along
  | Rotate (sk, n) -> eval_rotate env sk n
  | Translate (sk, v) -> eval_translate env sk v
  | Repeat (sk, v, n) -> eval_repeat env sk v n
  | Symmetric (sk, ax) -> eval_symmetric env sk ax
  | RelativeTo (v, sk) -> eval_relative_to env v sk
  | Inside (sk, bounds_sk) -> eval_inside env sk bounds_sk
  | Compose sks -> List.concat_map (eval_sketch env) sks

(** Evaluate a primitive to IR *)
and eval_primitive env (prim : primitive) : ir =
  match prim with
  | Dot v ->
    let p = eval_vec env v in
    (* Dot becomes a small circle - approximate with short lines *)
    let r = 0.5 in  (* Small radius *)
    let segments = [
      LineTo { x = p.x +. r; y = p.y };
      LineTo { x = p.x; y = p.y +. r };
      LineTo { x = p.x -. r; y = p.y };
      LineTo { x = p.x; y = p.y -. r };
      LineTo { x = p.x +. r; y = p.y };
    ] in
    [{ start = { x = p.x +. r; y = p.y }; segments }]
  
  | HDash v ->
    let p = eval_vec env v in
    let len = 2.0 in  (* Dash length *)
    [{ start = { x = p.x -. len /. 2.0; y = p.y };
       segments = [LineTo { x = p.x +. len /. 2.0; y = p.y }] }]
  
  | VDash v ->
    let p = eval_vec env v in
    let len = 2.0 in
    [{ start = { x = p.x; y = p.y -. len /. 2.0 };
       segments = [LineTo { x = p.x; y = p.y +. len /. 2.0 }] }]
  
  | Line (v0, v1) ->
    let p0 = eval_vec env v0 in
    let p1 = eval_vec env v1 in
    [{ start = p0; segments = [LineTo p1] }]
  
  | Curve (v0, through, v1) ->
    let p0 = eval_vec env v0 in
    let p1 = eval_vec env v1 in
    let control_points = List.map (eval_vec env) through in
    (match control_points with
     | [c] -> 
       (* Quadratic bezier *)
       [{ start = p0; segments = [QuadraticTo (c, p1)] }]
     | [c1; c2] ->
       (* Cubic bezier *)
       [{ start = p0; segments = [BezierTo (c1, c2, p1)] }]
     | _ ->
       (* Multiple control points - chain quadratic beziers *)
       (* This is a simplification; could use Catmull-Rom or similar *)
       let rec make_segments prev = function
         | [] -> [LineTo p1]
         | [c] -> [QuadraticTo (c, p1)]
         | c :: rest ->
           let mid = point_scale (point_add prev c) 0.5 in
           QuadraticTo (prev, mid) :: make_segments c rest
       in
       match control_points with
       | [] -> [{ start = p0; segments = [LineTo p1] }]
       | c :: rest -> [{ start = p0; segments = make_segments c rest }])
  
  | Arc (center_v, radius_v, a0, a1) ->
    let center = eval_vec env center_v in
    let radius = eval_vec env radius_v in
    let angle0 = eval_num env a0 in
    let angle1 = eval_num env a1 in
    [{ start = point 
         (center.x +. radius.x *. Float.cos (angle0 *. Float.pi /. 180.0))
         (center.y +. radius.y *. Float.sin (angle0 *. Float.pi /. 180.0));
       segments = [ArcTo (center, radius, angle0, angle1)] }]

(** Scale transformation *)
and eval_scale env sk n along =
  let factor = eval_num env n in
  let ir = eval_sketch env sk in
  match along with
  | None ->
    (* Uniform scale around origin *)
    transform_ir (fun p -> point_scale p factor) ir
  | Some v ->
    (* Scale along a specific axis *)
    let axis = eval_vec env v in
    let len = Float.sqrt (axis.x *. axis.x +. axis.y *. axis.y) in
    if len < 1e-10 then ir
    else
      let nx, ny = axis.x /. len, axis.y /. len in
      transform_ir (fun p ->
        let proj = p.x *. nx +. p.y *. ny in
        let perp_x = p.x -. proj *. nx in
        let perp_y = p.y -. proj *. ny in
        { x = perp_x +. proj *. factor *. nx;
          y = perp_y +. proj *. factor *. ny }
      ) ir

(** Rotate transformation *)
and eval_rotate env sk n =
  let angle = eval_num env n in
  let ir = eval_sketch env sk in
  let center = compute_center ir in
  transform_ir (fun p -> point_rotate_around p center angle) ir

(** Translate transformation *)
and eval_translate env sk v =
  let offset = eval_vec env v in
  let ir = eval_sketch env sk in
  transform_ir (fun p -> point_add p offset) ir

(** Repeat transformation *)
and eval_repeat env sk v n =
  let offset = eval_vec env v in
  let count = int_of_float (eval_num env n) in
  let base_ir = eval_sketch env sk in
  let rec go i acc =
    if i >= count then acc
    else
      let translation = point_scale offset (float_of_int i) in
      let translated = transform_ir (fun p -> point_add p translation) base_ir in
      go (i + 1) (translated @ acc)
  in
  go 0 []

(** Symmetric transformation *)
and eval_symmetric env sk ax =
  let ir = eval_sketch env sk in
  let reflected = match ax with
    | XAxis -> 
      (* Reflect across x-axis (y=0): negate y coordinates *)
      transform_ir point_reflect_y ir
    | YAxis -> 
      (* Reflect across y-axis (x=0): negate x coordinates *)
      transform_ir point_reflect_x ir
    | XAxisAt pos_expr ->
      (* Reflect across horizontal line y=pos *)
      let pos = eval_num env pos_expr in
      transform_ir (fun p -> { x = p.x; y = 2.0 *. pos -. p.y }) ir
    | YAxisAt pos_expr ->
      (* Reflect across vertical line x=pos *)
      let pos = eval_num env pos_expr in
      transform_ir (fun p -> { x = 2.0 *. pos -. p.x; y = p.y }) ir
    | CustomAxis (v1, v2) ->
      let p1 = eval_vec env v1 in
      let p2 = eval_vec env v2 in
      transform_ir (fun p -> point_reflect_line p p1 p2) ir
  in
  ir @ reflected

(** Relative to transformation (coordinate frame shift) *)
and eval_relative_to env v sk =
  let origin = eval_vec env v in
  let ir = eval_sketch env sk in
  transform_ir (fun p -> point_add p origin) ir

(** Inside bounds check *)
and eval_inside env sk bounds_sk =
  let ir = eval_sketch env sk in
  let bounds_ir = eval_sketch env bounds_sk in
  let shape_bounds = compute_bounds ir in
  let container_bounds = compute_bounds bounds_ir in
  if shape_bounds.min_x < container_bounds.min_x ||
     shape_bounds.max_x > container_bounds.max_x ||
     shape_bounds.min_y < container_bounds.min_y ||
     shape_bounds.max_y > container_bounds.max_y then
    error (BoundsViolation "Shape exceeds bounding box")
  else
    ir

(* ===== Statement Evaluation ===== *)

(** Evaluate a statement, returning updated environment and optional IR to draw *)
let eval_statement env (stmt : statement) : env * ir option =
  match stmt with
  | LetNum (name, expr) ->
    let value = eval_num env expr in
    (bind name (VNum value) env, None)
  | LetVec (name, expr) ->
    let value = eval_vec env expr in
    (bind name (VVec value) env, None)
  | LetSketch (name, expr) ->
    let value = eval_sketch env expr in
    (bind name (VSketch value) env, None)
  | Draw expr ->
    let ir = eval_sketch env expr in
    (env, Some ir)

(** Compile a complete program *)
let compile (program : program) : ir =
  let rec go env acc = function
    | [] -> List.rev acc |> List.concat
    | stmt :: rest ->
      let env', ir_opt = eval_statement env stmt in
      let acc' = match ir_opt with
        | Some ir -> ir :: acc
        | None -> acc
      in
      go env' acc' rest
  in
  go empty_env [] program

(** Compile with error handling *)
let compile_safe (program : program) : (ir, compile_error) result =
  try Ok (compile program)
  with CompileError e -> Error e

(* ===== IR Pretty Printing ===== *)

let point_to_string p =
  Printf.sprintf "(%.2f, %.2f)" p.x p.y

let segment_to_string = function
  | MoveTo p -> Printf.sprintf "M %s" (point_to_string p)
  | LineTo p -> Printf.sprintf "L %s" (point_to_string p)
  | BezierTo (c1, c2, p) ->
    Printf.sprintf "C %s %s %s" (point_to_string c1) (point_to_string c2) (point_to_string p)
  | QuadraticTo (c, p) ->
    Printf.sprintf "Q %s %s" (point_to_string c) (point_to_string p)
  | ArcTo (center, radius, a0, a1) ->
    Printf.sprintf "A center=%s radius=%s %.1f° -> %.1f°" 
      (point_to_string center) (point_to_string radius) a0 a1

let path_to_string path =
  let start_str = Printf.sprintf "M %s" (point_to_string path.start) in
  let segment_strs = List.map segment_to_string path.segments in
  String.concat " " (start_str :: segment_strs)

let ir_to_string ir =
  ir |> List.map path_to_string |> String.concat "\n"

let bounds_to_string b =
  Printf.sprintf "Bounds: x=[%.2f, %.2f] y=[%.2f, %.2f]" 
    b.min_x b.max_x b.min_y b.max_y

(** Summary statistics for IR *)
let ir_stats ir =
  let num_paths = List.length ir in
  let num_segments = ir |> List.map (fun p -> List.length p.segments) |> List.fold_left (+) 0 in
  let bounds = compute_bounds ir in
  Printf.sprintf "Paths: %d, Segments: %d\n%s" num_paths num_segments (bounds_to_string bounds)
