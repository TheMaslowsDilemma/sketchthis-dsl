(* Sketch DSL Abstract Syntax Tree *)
(* Defines the AST types produced by the parser *)

(** A 2D vector - the fundamental coordinate type *)
type vec2 = { x: float; y: float }

(** Type annotations in the language *)
type type_annotation =
  | TNumber
  | TVec2
  | TSketch

(** Numeric expression - can be a literal or variable *)
type num_expr =
  | NumLit of float
  | NumVar of string
  | NumNeg of num_expr
  | NumAdd of num_expr * num_expr
  | NumSub of num_expr * num_expr
  | NumMul of num_expr * num_expr
  | NumDiv of num_expr * num_expr

(** Vector expression - evaluates to a vec2 *)
type vec_expr =
  | VecLit of float * float           (* Literal: (1.0, 2.0) *)
  | VecVar of string                  (* Variable reference *)
  | VecCenter of sketch_expr          (* "center of <sketch>" *)

(** Sketch primitives *)
and primitive =
  | Dot of vec_expr                               (* dot at point *)
  | HDash of vec_expr                             (* horizontal dash at point *)
  | VDash of vec_expr                             (* vertical dash at point *)
  | Line of vec_expr * vec_expr                   (* line from p0 to p1 *)
  | Curve of vec_expr * vec_expr list * vec_expr  (* curve from p0 through [...] to p1 *)
  | Arc of vec_expr * vec_expr * num_expr * num_expr  (* arc: center, radius, angle0, angle1 *)

(** Axis for symmetry operations *)
and axis =
  | XAxis
  | YAxis
  | CustomAxis of vec_expr * vec_expr  (* axis defined by two points *)

(** Sketch expression - the main drawing type *)
and sketch_expr =
  | Primitive of primitive
  | SketchVar of string
  | Scale of sketch_expr * num_expr * vec_expr option    (* scale <sketch> by <n> [along <vec>] *)
  | Rotate of sketch_expr * num_expr                      (* rotate <sketch> by <degrees> *)
  | Translate of sketch_expr * vec_expr                   (* translate <sketch> by <vec> *)
  | Repeat of sketch_expr * vec_expr * num_expr           (* repeat <sketch> along <vec> <n> times *)
  | Symmetric of sketch_expr * axis                       (* symmetric <sketch> along <axis> *)
  | RelativeTo of vec_expr * sketch_expr                  (* relative to <point> <sketch> *)
  | Inside of sketch_expr * sketch_expr                   (* <sketch> inside <bounding sketch> *)
  | Compose of sketch_expr list                           (* multiple sketches combined *)

(** Expression that can be any type - resolved during type checking *)
type expr =
  | ExprNum of num_expr
  | ExprVec of vec_expr
  | ExprSketch of sketch_expr

(** Top-level statements *)
type statement =
  | LetNum of string * num_expr                     (* let name : number = expr *)
  | LetVec of string * vec_expr                     (* let name : vec2 = expr *)
  | LetSketch of string * sketch_expr               (* let name : sketch = expr *)
  | Draw of sketch_expr                             (* draw <sketch> *)

(** A complete program *)
type program = statement list

(* ===== Pretty Printing ===== *)

let type_to_string = function
  | TNumber -> "number"
  | TVec2 -> "vec2"
  | TSketch -> "sketch"

let rec num_expr_to_string = function
  | NumLit f -> 
    if Float.is_integer f then Printf.sprintf "%.0f" f
    else Printf.sprintf "%g" f
  | NumVar s -> s
  | NumNeg e -> Printf.sprintf "(-%s)" (num_expr_to_string e)
  | NumAdd (a, b) -> Printf.sprintf "(%s + %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumSub (a, b) -> Printf.sprintf "(%s - %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumMul (a, b) -> Printf.sprintf "(%s * %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumDiv (a, b) -> Printf.sprintf "(%s / %s)" (num_expr_to_string a) (num_expr_to_string b)

let rec vec_expr_to_string = function
  | VecLit (x, y) -> 
    let fmt f = if Float.is_integer f then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f in
    Printf.sprintf "(%s, %s)" (fmt x) (fmt y)
  | VecVar s -> s
  | VecCenter sk -> Printf.sprintf "center of %s" (sketch_expr_to_string sk)

and axis_to_string = function
  | XAxis -> "x_axis"
  | YAxis -> "y_axis"
  | CustomAxis (p1, p2) -> 
    Printf.sprintf "axis(%s, %s)" (vec_expr_to_string p1) (vec_expr_to_string p2)

and primitive_to_string = function
  | Dot v -> Printf.sprintf "dot at %s" (vec_expr_to_string v)
  | HDash v -> Printf.sprintf "hdash at %s" (vec_expr_to_string v)
  | VDash v -> Printf.sprintf "vdash at %s" (vec_expr_to_string v)
  | Line (p0, p1) -> 
    Printf.sprintf "line from %s to %s" (vec_expr_to_string p0) (vec_expr_to_string p1)
  | Curve (p0, through, p1) ->
    let through_str = through |> List.map vec_expr_to_string |> String.concat " and " in
    Printf.sprintf "curve from %s to %s through %s" 
      (vec_expr_to_string p0) (vec_expr_to_string p1) through_str
  | Arc (center, radius, a0, a1) ->
    Printf.sprintf "arc center %s radius %s from %s to %s"
      (vec_expr_to_string center) (vec_expr_to_string radius)
      (num_expr_to_string a0) (num_expr_to_string a1)

and sketch_expr_to_string = function
  | Primitive p -> primitive_to_string p
  | SketchVar s -> s
  | Scale (sk, n, None) ->
    Printf.sprintf "scale %s by %s" (sketch_expr_to_string sk) (num_expr_to_string n)
  | Scale (sk, n, Some v) ->
    Printf.sprintf "scale %s by %s along %s" 
      (sketch_expr_to_string sk) (num_expr_to_string n) (vec_expr_to_string v)
  | Rotate (sk, n) ->
    Printf.sprintf "rotate %s by %s" (sketch_expr_to_string sk) (num_expr_to_string n)
  | Translate (sk, v) ->
    Printf.sprintf "translate %s by %s" (sketch_expr_to_string sk) (vec_expr_to_string v)
  | Repeat (sk, v, n) ->
    Printf.sprintf "repeat %s along %s %s times" 
      (sketch_expr_to_string sk) (vec_expr_to_string v) (num_expr_to_string n)
  | Symmetric (sk, ax) ->
    Printf.sprintf "symmetric %s along %s" (sketch_expr_to_string sk) (axis_to_string ax)
  | RelativeTo (v, sk) ->
    Printf.sprintf "relative to %s %s" (vec_expr_to_string v) (sketch_expr_to_string sk)
  | Inside (sk1, sk2) ->
    Printf.sprintf "%s inside %s" (sketch_expr_to_string sk1) (sketch_expr_to_string sk2)
  | Compose sks ->
    Printf.sprintf "[%s]" (sks |> List.map sketch_expr_to_string |> String.concat ", ")

let statement_to_string = function
  | LetNum (name, expr) ->
    Printf.sprintf "let %s : number = %s" name (num_expr_to_string expr)
  | LetVec (name, expr) ->
    Printf.sprintf "let %s : vec2 = %s" name (vec_expr_to_string expr)
  | LetSketch (name, expr) ->
    Printf.sprintf "let %s : sketch = %s" name (sketch_expr_to_string expr)
  | Draw sk ->
    Printf.sprintf "draw %s" (sketch_expr_to_string sk)

let program_to_string (stmts : program) =
  stmts |> List.map statement_to_string |> String.concat "\n"

(* ===== Helper Constructors ===== *)

let vec x y = { x; y }
let num f = NumLit f
let dot p = Primitive (Dot p)
let line p0 p1 = Primitive (Line (p0, p1))
let curve p0 through p1 = Primitive (Curve (p0, through, p1))
