(* Sketch DSL - Main Entry Point *)
(* Full compiler pipeline: lexer -> parser -> compiler -> gcode *)

open Sketch_dsl

let usage =
  {|Sketch DSL Compiler

Usage: sketch_dsl <input-file> [options]

Options:
  -o <name>     Output base name (without extension)
  --gcode       Generate G-code output (.txt)
  --svg         Generate SVG preview (.svg)
  --stats       Print compilation statistics to stdio

If neither --gcode nor --svg is specified, defaults to --gcode.
If -o is not specified, uses input filename as base.

Examples:
  sketch_dsl fun.sketch                     # outputs fun.txt
  sketch_dsl fun.sketch -o out              # outputs out.txt
  sketch_dsl fun.sketch -o out --svg        # outputs out.svg only
  sketch_dsl fun.sketch -o out --gcode --svg  # outputs out.txt and out.svg
|}

type config = {
  input_file : string option;
  output_base : string option;
  emit_gcode : bool;
  emit_svg : bool;
  show_stats : bool;
}

let default_config = {
  input_file = None;
  output_base = None;
  emit_gcode = false;
  emit_svg = false;
  show_stats = false;
}

let parse_args args =
  let rec loop cfg = function
    | [] -> cfg
    | "-o" :: name :: rest -> loop { cfg with output_base = Some name } rest
    | "--gcode" :: rest -> loop { cfg with emit_gcode = true } rest
    | "--svg" :: rest -> loop { cfg with emit_svg = true } rest
    | "--stats" :: rest -> loop { cfg with show_stats = true } rest
    | "-h" :: _ | "--help" :: _ -> print_endline usage; exit 0
    | arg :: rest when String.length arg > 0 && arg.[0] <> '-' ->
        loop { cfg with input_file = Some arg } rest
    | arg :: _ ->
        Printf.eprintf "Unknown option: %s\n%s" arg usage;
        exit 1
  in
  let cfg = loop default_config args in
  (* Default to gcode if neither specified *)
  if not cfg.emit_gcode && not cfg.emit_svg then
    { cfg with emit_gcode = true }
  else
    cfg

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | Some i -> String.sub filename 0 i
  | None -> filename

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let compile_input input ~emit_gcode ~emit_svg ~output_base ~show_stats =
  let ast = Parser.parse input in
  let ir = Compiler.compile ast in
  
  if emit_gcode then begin
    let gcode_file = output_base ^ ".txt" in
    if show_stats then begin
      let gcode, stats = Gcode.generate_machine_with_stats ir in
      write_file gcode_file gcode;
      Printf.printf "Compilation Stats: %s\n" stats
    end else begin
      let gcode = Gcode.generate_machine ir in
      write_file gcode_file gcode
    end
  end;
  
  if emit_svg then begin
    let svg_file = output_base ^ ".svg" in
    let svg = Gcode.generate_svg ir in
    write_file svg_file svg
  end

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let cfg = parse_args args in
  
  match cfg.input_file with
  | None ->
      Printf.eprintf "Error: No input file specified\n%s" usage;
      exit 1
  | Some input_file ->
      let output_base = match cfg.output_base with
        | Some base -> base
        | None -> remove_extension input_file
      in
      try
        let input = read_file input_file in
        compile_input input
          ~emit_gcode:cfg.emit_gcode
          ~emit_svg:cfg.emit_svg
          ~output_base
          ~show_stats:cfg.show_stats
      with
      | Lexer.LexerError err ->
          Printf.eprintf "Lexer error at line %d, column %d: %s\n"
            err.position.line err.position.column err.message;
          exit 1
      | Parser.ParseError err ->
          Printf.eprintf "%s\n" (Parser.format_error err);
          exit 1
      | Compiler.CompileError err ->
          Printf.eprintf "Compile error: %s\n" (Compiler.format_error err);
          exit 1
      | Sys_error msg ->
          Printf.eprintf "Error: %s\n" msg;
          exit 1