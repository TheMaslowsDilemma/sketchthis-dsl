(*---------------------------------------------------------
sections.mli - section overlap detection
---------------------------------------------------------*)

open Ir
open Vector

type render_info = {
  section : string;
  method_ : string;
  line : int;
  column : int;
}

type rendered_sketch = {
  ir : ir;
  info : render_info;
}

type section_data = {
  name : string;
  sketches : rendered_sketch list;
}

type sketch_intersection = {
  point : vec;
  sketch1 : render_info;
  sketch2 : render_info;
}

type overlap_warning = {
  msg : string;
  intersections : sketch_intersection list;
}

val empty_section : string -> section_data

val add_sketch : section_data -> render_info -> ir -> section_data

val check_overlaps : section_data list -> overlap_warning option

val format_warning : overlap_warning -> string