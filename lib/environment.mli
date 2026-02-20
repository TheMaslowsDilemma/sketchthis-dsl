(*---------------------------------------------------------
environment.mli - variable organization and lookup logic
note: there is no mutability.
---------------------------------------------------------*)

open Vector

type value =
  | VNum of float
  | VVec of vec
  | VRegion of vec list
  | VSketch of Ir.ir

type scope
type env

exception UndefinedVariable of string
exception UndefinedScope of string

val init_scope : env -> string -> env
(* adds an empty scope to the environment *)

val empty_env : env
(* an empty hash table of scope name to scope *)

val bind : env -> string -> string -> value -> env
(* add new value to a variable ~ no mutability ~
environment, section name, variable name, new value *)

val lookup : env -> string -> string -> value
(* returns the value from the given params
environment, section name, variable name*)
