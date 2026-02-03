(*
-----------------------------------------------------------
environment.mli
-----------------------------------------------------------
variable bindings during compilation.
*)

open Vector

type value = VNum of float | VVec of vec | VSketch of Ir.ir
type env

exception UndefinedVariable of string

val empty_env : env
val bind : string -> value -> env -> env
val lookup : string -> env -> value
