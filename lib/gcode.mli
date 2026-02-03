(***
----------------------------------------------------------- 
gcode.mli
----------------------------------------------------------- 
g-code generation for pen plotters. includes path optimization,
deduplication, and SVG preview generation.
***)

open Compiler
open Ir

type gcode_config = {
  travel_speed : float;
  draw_speed : float;
  pen_up_command : string;
  pen_down_command : string;
  scale : float;
  x_offset : float;
  y_offset : float;
  decimal_places : int;
  include_comments : bool;
}

type placement = { pos_x : float; pos_y : float; width : float; height : float }

val default_config : gcode_config
val machine_config : gcode_config
val machine_bounds : bounds

(* path operations *)
val reverse_path : path -> path
val optimize_paths : path list -> path list
val deduplicate_paths : ?eps:float -> path list -> path list

(* bounds and fitting *)
val check_machine_bounds : ir -> bool
val fit_to_machine : ?margin:float -> ir -> ir
val fit_to_placement : placement -> ir -> ir

val fit_to_machine_with_placement :
  ?margin:float -> ?placement:placement -> ir -> ir

(* g-code generation *)
val generate : ?config:gcode_config -> ir -> string
val generate_with_stats : ?config:gcode_config -> ir -> string * string

val generate_machine :
  ?fit:bool -> ?margin:float -> ?placement:placement -> ir -> string

val generate_machine_with_stats :
  ?fit:bool -> ?margin:float -> ?placement:placement -> ir -> string * string

(* svg preview *)
val generate_svg : ?width:int -> ?height:int -> ir -> string
