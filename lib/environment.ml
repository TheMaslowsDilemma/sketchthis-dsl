(*---------------------------------------------------------
environment.ml - variable organization and lookup logic
we use maps (BST) to perform var and scope lookups
---------------------------------------------------------*)

open Vector
module StringMap = Map.Make (String)

type value =
  | VNum of float
  | VVec of vec
  | VRegion of vec list
  | VSketch of Ir.ir

type scope = value StringMap.t
type env = scope StringMap.t

exception UndefinedVariable of string
exception UndefinedScope of string

let empty_env : env = StringMap.empty
let find_scope_opt e sname = StringMap.find_opt sname e

let find_scope e sname =
  match StringMap.find_opt sname e with
  | None -> raise (UndefinedScope sname)
  | Some s -> s

let find_var s vname =
  match StringMap.find_opt vname s with
  | None -> raise (UndefinedVariable vname)
  | Some v -> v

let bind env sname vname value =
  let s =
    match find_scope_opt env sname with None -> StringMap.empty | Some s -> s
  in
  let snew = StringMap.add vname value s in
  StringMap.add sname snew env

let lookup sname vname env =
  let s = find_scope env sname in
  find_var s vname
