(*---------------------------------------------------------
compiler.mli -
sketch dsl compiler. evaluates programs to intermediate
representation (paths with line segments).
---------------------------------------------------------*)

open Ir
open Vector
open Sections

type bounds = { min_x : float; max_x : float; min_y : float; max_y : float }
type compile_error

exception CompileError of compile_error

val compile : Ast.program -> ir * overlap_warning option
val compile_safe : Ast.program -> (ir * overlap_warning option, compile_error) result

val compute_bounds : ir -> bounds
val compute_center : ir -> vec
val transform_ir : (vec -> vec) -> ir -> ir

val ir_to_string : ir -> string
val ir_stats : ir -> string
val bounds_to_string : bounds -> string
val format_error : compile_error -> string