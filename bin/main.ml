(* Sketch DSL - Main Entry Point *)

open Sketch_dsl

let usage =
  {|Sketch DSL Compiler

Usage: sketch_dsl <input-file> [options]

Options:
  -o <name>         Output base name (without extension)
  -scale <n>        Scale the drawing by factor n (e.g., 0.5 for half size)
  -pos <x,y>        Position the drawing at (x,y) in mm (e.g., -pos 50,100)
  -size <w,h>       Scale drawing to fit within width x height in mm (e.g., -size 200,150)
  --gcode           Generate G-code output (.txt)
  --svg             Generate SVG preview (.svg)
  --stats           Print compilation statistics to stdout
  --no-fit          Disable auto-fitting to machine bounds (use with -pos/-size)

If neither --gcode nor --svg is specified, defaults to --gcode.
If -o is not specified, uses input filename as base.

When -pos and -size are both specified, the drawing is scaled to fit within
the specified size and positioned at the given coordinates (no centering).
When only -scale is used, the drawing is centered on the machine bed.

Examples:
  sketch_dsl fun.sketch                       # outputs fun.txt (centered)
  sketch_dsl fun.sketch -o out                # outputs out.txt
  sketch_dsl fun.sketch -scale 0.5            # half size, centered
  sketch_dsl fun.sketch -pos 10,10 -size 100,100  # placed at (10,10), max 100x100mm
  sketch_dsl fun.sketch -o out --svg          # outputs out.svg only
  sketch_dsl fun.sketch -o out --gcode --svg  # outputs out.txt and out.svg
|}

type config = {
  input_file : string option;
  output_base : string option;
  scale : float;
  position : (float * float) option;
  size : (float * float) option;
  emit_gcode : bool;
  emit_svg : bool;
  show_stats : bool;
  no_fit : bool;
}

let default_config = {
  input_file = None;
  output_base = None;
  scale = 1.0;
  position = None;
  size = None;
  emit_gcode = false;
  emit_svg = false;
  show_stats = false;
  no_fit = false;
}

let parse_pair s =
  match String.split_on_char ',' s with
  | [a; b] ->
      (match float_of_string_opt a, float_of_string_opt b with
       | Some x, Some y -> Some (x, y)
       | _ -> None)
  | _ -> None

let parse_args args =
  let rec loop cfg = function
    | [] -> cfg
    | "-o" :: name :: rest -> loop { cfg with output_base = Some name } rest
    | "-scale" :: n :: rest ->
        (match float_of_string_opt n with
        | Some f when f > 0.0 -> loop { cfg with scale = f } rest
        | _ -> Printf.eprintf "Error: -scale requires a positive number\n%s" usage; exit 1)
    | "-pos" :: coords :: rest ->
        (match parse_pair coords with
        | Some (x, y) -> loop { cfg with position = Some (x, y) } rest
        | None -> Printf.eprintf "Error: -pos requires coordinates as x,y\n%s" usage; exit 1)
    | "-size" :: dims :: rest ->
        (match parse_pair dims with
        | Some (w, h) when w > 0.0 && h > 0.0 -> loop { cfg with size = Some (w, h) } rest
        | _ -> Printf.eprintf "Error: -size requires positive dimensions as w,h\n%s" usage; exit 1)
    | "--gcode" :: rest -> loop { cfg with emit_gcode = true } rest
    | "--svg" :: rest -> loop { cfg with emit_svg = true } rest
    | "--stats" :: rest -> loop { cfg with show_stats = true } rest
    | "--no-fit" :: rest -> loop { cfg with no_fit = true } rest
    | "-h" :: _ | "--help" :: _ -> print_endline usage; exit 0
    | arg :: rest when String.length arg > 0 && arg.[0] <> '-' ->
        loop { cfg with input_file = Some arg } rest
    | arg :: _ -> Printf.eprintf "Unknown option: %s\n%s" arg usage; exit 1
  in
  let cfg = loop default_config args in
  if not cfg.emit_gcode && not cfg.emit_svg then { cfg with emit_gcode = true } else cfg

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | Some i -> String.sub filename 0 i
  | None -> filename

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let compile_input input ~scale ~position ~size ~no_fit ~emit_gcode ~emit_svg ~output_base ~show_stats =
  let ast = Parser.parse input in
  let ir = Compiler.compile ast in
  
  let ir =
    if Float.abs (scale -. 1.0) < Globals.precision then ir
    else Compiler.transform_ir (fun v -> Vector.vec (v.Vector.x *. scale) (v.Vector.y *. scale)) ir
  in
  
  let placement =
    match position, size with
    | Some (x, y), Some (w, h) ->
        Some Gcode.{ pos_x = x; pos_y = y; width = w; height = h }
    | Some (x, y), None ->
        let b = Compiler.compute_bounds ir in
        Some Gcode.{ pos_x = x; pos_y = y; width = b.max_x -. b.min_x; height = b.max_y -. b.min_y }
    | None, Some (w, h) ->
        Some Gcode.{ pos_x = 0.0; pos_y = 0.0; width = w; height = h }
    | None, None -> None
  in
  
  if emit_gcode then begin
    let gcode_file = output_base ^ ".txt" in
    if show_stats then begin
      let gcode, stats = Gcode.generate_machine_with_stats ~fit:(not no_fit) ?placement ir in
      write_file gcode_file gcode;
      Printf.printf "%s\n%!" stats
    end else begin
      let gcode = Gcode.generate_machine ~fit:(not no_fit) ?placement ir in
      write_file gcode_file gcode
    end
  end;
  
  if emit_svg then begin
    let svg_file = output_base ^ ".svg" in
    let ir_for_svg = if no_fit then ir else Gcode.fit_to_machine_with_placement ?placement ir in
    write_file svg_file (Gcode.generate_svg ir_for_svg)
  end

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let cfg = parse_args args in
  match cfg.input_file with
  | None -> Printf.eprintf "Error: No input file specified\n%s%!" usage; exit 1
  | Some input_file ->
      let output_base = match cfg.output_base with Some b -> b | None -> remove_extension input_file in
      try
        compile_input (read_file input_file)
          ~scale:cfg.scale ~position:cfg.position ~size:cfg.size ~no_fit:cfg.no_fit
          ~emit_gcode:cfg.emit_gcode ~emit_svg:cfg.emit_svg ~output_base ~show_stats:cfg.show_stats
      with
      | Lexer.LexerError e ->
          Printf.eprintf "%s\n%!" (Lexer.format_error e); exit 1
      | Parser.ParseError e ->
          Printf.eprintf "%s\n%!" (Parser.format_error e); exit 1
      | Compiler.CompileError e ->
          Printf.eprintf "%s\n%!" (Compiler.format_error e); exit 1
      | Sys_error msg ->
          Printf.eprintf "Error: %s\n%!" msg; exit 1