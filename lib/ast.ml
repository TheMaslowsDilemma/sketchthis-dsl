open Vector

type type_annotation = TNumber | TVec | TSketch

type num_expr =
  | NumLit of float
  | NumVar of string
  | NumNeg of num_expr
  | NumAdd of num_expr * num_expr
  | NumSub of num_expr * num_expr
  | NumMul of num_expr * num_expr
  | NumDiv of num_expr * num_expr

type vec_expr =
  | VecLit of float * float
  | VecConstruct of num_expr * num_expr
  | VecVar of string
  | VecCenter of sketch_expr
  | VecAdd of vec_expr * vec_expr
  | VecSub of vec_expr * vec_expr
  | VecScale of vec_expr * num_expr
  | VecFlow of vec_expr

and primitive =
  | Dot of vec_expr
  | Dash of vec_expr
  | Stroke of vec_expr * vec_expr list * vec_expr

and sketch_expr =
  | Primitive of primitive
  | SketchVar of string
  | SketchList of sketch_expr list

type expr =
  | ExprNum of num_expr
  | ExprVec of vec_expr
  | ExprSketch of sketch_expr

type statement =
  | LetNum of string * num_expr
  | LetVec of string * vec_expr
  | LetSketch of string * sketch_expr
  | Scribble of sketch_expr
  | Draw of sketch_expr
  | Trace of sketch_expr

type program = statement list

let type_to_string = function
  | TNumber -> "number"
  | TVec -> "vec"
  | TSketch -> "sketch"

let rec num_expr_to_string = function
  | NumLit f ->
      if Float.is_integer f then Printf.sprintf "%.0f" f
      else Printf.sprintf "%g" f
  | NumVar s -> s
  | NumNeg e -> Printf.sprintf "(-%s)" (num_expr_to_string e)
  | NumAdd (a, b) ->
      Printf.sprintf "(%s + %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumSub (a, b) ->
      Printf.sprintf "(%s - %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumMul (a, b) ->
      Printf.sprintf "(%s * %s)" (num_expr_to_string a) (num_expr_to_string b)
  | NumDiv (a, b) ->
      Printf.sprintf "(%s / %s)" (num_expr_to_string a) (num_expr_to_string b)

let rec vec_expr_to_string = function
  | VecLit (x, y) ->
      let fmt f =
        if Float.is_integer f then Printf.sprintf "%.0f" f
        else Printf.sprintf "%g" f
      in
      Printf.sprintf "(%s, %s)" (fmt x) (fmt y)
  | VecConstruct (x, y) ->
      Printf.sprintf "(%s, %s)" (num_expr_to_string x) (num_expr_to_string y)
  | VecVar s -> s
  | VecCenter sk -> Printf.sprintf "center of %s" (sketch_expr_to_string sk)
  | VecAdd (a, b) ->
      Printf.sprintf "(%s + %s)" (vec_expr_to_string a) (vec_expr_to_string b)
  | VecSub (a, b) ->
      Printf.sprintf "(%s - %s)" (vec_expr_to_string a) (vec_expr_to_string b)
  | VecScale (v, n) ->
      Printf.sprintf "(%s * %s)" (vec_expr_to_string v) (num_expr_to_string n)
  | VecFlow v -> Printf.sprintf "flow at %s" (vec_expr_to_string v)

and primitive_to_string = function
  | Dot v -> Printf.sprintf "dot at %s" (vec_expr_to_string v)
  | Dash v -> Printf.sprintf "dash at %s" (vec_expr_to_string v)
  | Stroke (p0, via, p1) ->
      if List.length via = 0 then
        Printf.sprintf "stroke from %s to %s" (vec_expr_to_string p0)
          (vec_expr_to_string p1)
      else
        let via_str =
          via |> List.map vec_expr_to_string |> String.concat ", "
        in
        Printf.sprintf "stroke from %s to %s via [%s]" (vec_expr_to_string p0)
          (vec_expr_to_string p1) via_str

and sketch_expr_to_string = function
  | Primitive p -> primitive_to_string p
  | SketchVar s -> s
  | SketchList sks ->
      Printf.sprintf "[%s]"
        (sks |> List.map sketch_expr_to_string |> String.concat ", ")

let statement_to_string = function
  | LetNum (name, expr) ->
      Printf.sprintf "let %s : number = %s" name (num_expr_to_string expr)
  | LetVec (name, expr) ->
      Printf.sprintf "let %s : vec = %s" name (vec_expr_to_string expr)
  | LetSketch (name, expr) ->
      Printf.sprintf "let %s : sketch = %s" name (sketch_expr_to_string expr)
  | Scribble sk -> Printf.sprintf "scribble %s" (sketch_expr_to_string sk)
  | Draw sk -> Printf.sprintf "draw %s" (sketch_expr_to_string sk)
  | Trace sk -> Printf.sprintf "trace %s" (sketch_expr_to_string sk)

let program_to_string (stmts : program) =
  stmts |> List.map statement_to_string |> String.concat "\n"

let vec x y = { x; y }
let num f = NumLit f
let dot p = Primitive (Dot p)
let dash p = Primitive (Dash p)
let stroke p0 p1 = Primitive (Stroke (p0, [], p1))
let stroke_via p0 via p1 = Primitive (Stroke (p0, via, p1))
