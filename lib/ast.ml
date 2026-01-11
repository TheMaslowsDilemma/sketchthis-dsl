(* Sketch DSL Abstract Syntax Tree *)
(* This module will define the AST types after parsing *)

(** A 2D vector - the fundamental coordinate type *)
type vec2 = { x: float; y: float }

(** Numeric expression - can be a literal or computation *)
type num_expr =
  | NumLit of float
  | NumVar of string
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

(** Sketch expression - the main drawing type *)
and sketch_expr =
  | Primitive of primitive
  | SketchVar of string
  | Scale of sketch_expr * num_expr * vec_expr option    (* scale <sketch> by <n> [along <vec>] *)
  | Rotate of sketch_expr * num_expr                      (* rotate <sketch> by <degrees> *)
  | Translate of sketch_expr * vec_expr                   (* translate <sketch> by <vec> *)
  | Repeat of sketch_expr * vec_expr * int                (* repeat <sketch> along <vec> <n> times *)
  | Symmetric of sketch_expr * axis                       (* symmetric <sketch> along <axis> *)
  | RelativeTo of vec_expr * sketch_expr                  (* relative to <point> <sketch> *)
  | Inside of sketch_expr * sketch_expr                   (* <sketch> inside <bounding sketch> *)
  | Compose of sketch_expr list                           (* multiple sketches combined *)

(** Axis for symmetry operations *)
and axis =
  | XAxis
  | YAxis
  | CustomAxis of vec_expr * vec_expr  (* axis defined by two points *)

(** Top-level statements *)
type statement =
  | LetBinding of string * string * sketch_expr  (* let name : type = expr *)
  | Draw of sketch_expr                          (* draw <sketch> *)

(** A complete program *)
type program = statement list

(* Helper constructors for convenience *)
let vec x y = { x; y }
let dot p = Primitive (Dot p)
let line p0 p1 = Primitive (Line (p0, p1))
let curve p0 through p1 = Primitive (Curve (p0, through, p1))
