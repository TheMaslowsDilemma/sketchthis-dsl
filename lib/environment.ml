(*
-----------------------------------------------------------
environment.ml
-----------------------------------------------------------
*)

open Vector

type value = VNum of float | VVec of vec | VRegion of vec list | VSketch of Ir.ir
type env = (string * value) list

exception UndefinedVariable of string

let empty_env : env = []
let bind name value env = (name, value) :: env

let lookup name env =
  match List.assoc_opt name env with
  | Some v -> v
  | None -> raise (UndefinedVariable name)
