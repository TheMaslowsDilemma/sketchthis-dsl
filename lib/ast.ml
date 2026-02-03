(*
-----------------------------------------------------------
ast.ml
-----------------------------------------------------------
abstract syntax tree — unified expression type
*)

type expr = expr_desc Location.loc

and expr_desc =
  | Lit of float
  | Var of string
  | Vec of expr * expr
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | CenterOf of expr
  | Dot of expr
  | Dash of expr
  | Segments of expr list
  | Splines of expr list
  | SketchList of expr list
  | Mirror of expr * expr
  | Rotate of expr * expr
  | Translate of expr * expr
  | Scale of expr * expr
  | At of expr * expr

type statement = statement_desc Location.loc

and statement_desc =
  | Let of string * expr
  | Scribble of expr
  | Draw of expr
  | Trace of expr

type program = statement list
