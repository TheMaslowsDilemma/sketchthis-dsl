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

let init_scope env sname =
  match find_scope_opt env sname with
  | Some _ -> env  (* already exists *)
  | None -> StringMap.add sname StringMap.empty env

let bind env sname vname value =
  let s =
    match find_scope_opt env sname with None -> StringMap.empty | Some s -> s
  in
  let snew = StringMap.add vname value s in
  StringMap.add sname snew env

let rec lookup env sname vname =
  match find_scope_opt env sname with
  | None -> raise (UndefinedScope sname)
  | Some s -> (
      match StringMap.find_opt vname s with
      | Some v -> v
      | None ->
          if sname = "default" then raise (UndefinedVariable vname)
          else lookup env "default" vname)
