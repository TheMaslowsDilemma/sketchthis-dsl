(*
----------------------------------------------------------- 
environment.ml
-----------------------------------------------------------
*)

open Vector

type value = VNum of float | VVec of vec | VSketch of Ast.sketch_expr
type env = (string * value) list

exception UndefinedVariable of string
exception TypeMismatch of string

let empty_env : env = []
let bind name value env = (name, value) :: env

let lookup_num name env =
  match List.assoc_opt name env with
  | Some (VNum f) -> f
  | Some _ -> raise (TypeMismatch ("expected number: " ^ name))
  | None -> raise (UndefinedVariable name)

let lookup_vec name env =
  match List.assoc_opt name env with
  | Some (VVec v) -> v
  | Some _ -> raise (TypeMismatch ("expected vector: " ^ name))
  | None -> raise (UndefinedVariable name)

let lookup_sketch name env =
  match List.assoc_opt name env with
  | Some (VSketch s) -> s
  | Some _ -> raise (TypeMismatch ("expected sketch: " ^ name))
  | None -> raise (UndefinedVariable name)
