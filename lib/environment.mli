(***
----------------------------------------------------------- 
environment.mli
----------------------------------------------------------- 
variable bindings during compilation.
***)

open Vector

type value = VNum of float | VVec of vec | VSketch of Ast.sketch_expr
type env

exception UndefinedVariable of string
exception TypeMismatch of string

val empty_env : env
val bind : string -> value -> env -> env
val lookup_num : string -> env -> float
val lookup_vec : string -> env -> vec
val lookup_sketch : string -> env -> Ast.sketch_expr
