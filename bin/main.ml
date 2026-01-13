(* Sketch DSL - Main Entry Point *)
(* Full compiler pipeline: lexer -> parser -> compiler -> gcode *)

open Sketch_dsl

let usage =
  {|
Sketch DSL Compiler

Usage: sketch_dsl [options] [file]

Options:
  --lex         Tokenize input and print tokens
  --parse       Parse input and print AST
  --compile     Compile input and print IR
  --gcode       Generate generic G-code
  --machine     Generate G-code for Uunatek plotter (default)
  --svg         Generate SVG preview
  --stats       Show compilation and optimization statistics
  --help        Show this help message

If no file is provided, reads from stdin.
|}

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_stdin () =
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    ""
  with End_of_file -> Buffer.contents buf

let run_lexer input =
  try
    let tokens = Lexer.tokenize input in
    print_endline "=== Tokens ===";
    print_endline (Lexer.located_tokens_to_string tokens);
    print_endline "";
    print_endline "=== Compact ===";
    print_endline
      (Lexer.tokens_to_string (List.map (fun t -> t.Lexer.token) tokens))
  with Lexer.LexerError err ->
    Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
      err.position.column err.message;
    exit 1

let run_parser input =
  try
    let ast = Parser.parse input in
    print_endline "=== AST ===";
    print_endline (Ast.program_to_string ast);
    print_endline "";
    print_endline
      (Printf.sprintf "=== Parsed %d statement(s) ===" (List.length ast))
  with
  | Lexer.LexerError err ->
      Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
        err.position.column err.message;
      exit 1
  | Parser.ParseError err ->
      Printf.eprintf "%s\n" (Parser.format_error err);
      exit 1

let run_compiler ?(show_stats = false) input =
  try
    let ast = Parser.parse input in
    let ir = Compiler.compile ast in
    print_endline "=== Intermediate Representation ===";
    print_endline (Compiler.ir_to_string ir);
    if show_stats then begin
      print_endline "";
      print_endline "=== Statistics ===";
      print_endline (Compiler.ir_stats ir)
    end
  with
  | Lexer.LexerError err ->
      Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
        err.position.column err.message;
      exit 1
  | Parser.ParseError err ->
      Printf.eprintf "%s\n" (Parser.format_error err);
      exit 1
  | Compiler.CompileError err ->
      Printf.eprintf "Compile error: %s\n" (Compiler.format_error err);
      exit 1

let run_gcode ?(show_stats = false) input =
  try
    let ast = Parser.parse input in
    let ir = Compiler.compile ast in
    if show_stats then begin
      let gcode, stats = Gcode.generate_with_stats ir in
      print_endline gcode;
      print_endline "";
      print_endline "=== Optimization Statistics ===";
      print_endline stats
    end
    else begin
      let gcode = Gcode.generate ir in
      print_endline gcode
    end
  with
  | Lexer.LexerError err ->
      Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
        err.position.column err.message;
      exit 1
  | Parser.ParseError err ->
      Printf.eprintf "%s\n" (Parser.format_error err);
      exit 1
  | Compiler.CompileError err ->
      Printf.eprintf "Compile error: %s\n" (Compiler.format_error err);
      exit 1

let run_svg input output_filepath =
  try
    let ast = Parser.parse input in
    let ir = Compiler.compile ast in
    let svg = Gcode.generate_svg ir in
    let oc = open_out output_filepath in
    Printf.fprintf oc "%s" svg;
    close_out oc
  with
  | Lexer.LexerError err ->
      Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
        err.position.column err.message;
      exit 1
  | Parser.ParseError err ->
      Printf.eprintf "%s\n" (Parser.format_error err);
      exit 1
  | Compiler.CompileError err ->
      Printf.eprintf "Compile error: %s\n" (Compiler.format_error err);
      exit 1

let run_machine ?(show_stats = false) input =
  try
    let ast = Parser.parse input in
    let ir = Compiler.compile ast in
    if show_stats then begin
      let gcode, stats = Gcode.generate_machine_with_stats ir in
      print_endline gcode;
      print_endline "";
      print_endline "=== Uunatek Statistics ===";
      print_endline stats
    end
    else begin
      let gcode = Gcode.generate_machine ir in
      print_endline gcode
    end
  with
  | Lexer.LexerError err ->
      Printf.eprintf "Lexer error at line %d, column %d: %s\n" err.position.line
        err.position.column err.message;
      exit 1
  | Parser.ParseError err ->
      Printf.eprintf "%s\n" (Parser.format_error err);
      exit 1
  | Compiler.CompileError err ->
      Printf.eprintf "Compile error: %s\n" (Compiler.format_error err);
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in

  match args with
  | [ "--help" ] | [ "-h" ] -> print_endline usage
  | [ "--lex" ] ->
      let input = read_stdin () in
      run_lexer input
  | [ "--lex"; filename ] ->
      let input = read_file filename in
      run_lexer input
  | [ "--parse" ] ->
      let input = read_stdin () in
      run_parser input
  | [ "--parse"; filename ] ->
      let input = read_file filename in
      run_parser input
  | [ "--compile" ] ->
      let input = read_stdin () in
      run_compiler input
  | [ "--compile"; filename ] ->
      let input = read_file filename in
      run_compiler ~show_stats:true input
  | [ "--gcode" ] ->
      let input = read_stdin () in
      run_gcode input
  | [ "--gcode"; filename ] ->
      let input = read_file filename in
      run_gcode input
  | [ "--svg"; "-o"; output_path ] ->
      let input = read_stdin () in
      run_svg input output_path
  | [ "--svg"; "-i"; filename; "-o"; output_path ] ->
      let input = read_file filename in
      run_svg input output_path
  | [ "--machine" ] ->
      let input = read_stdin () in
      run_machine input
  | [ "--machine"; filename ] ->
      let input = read_file filename in
      run_machine input
  | [ "--stats" ] ->
      let input = read_stdin () in
      run_machine ~show_stats:true input
  | [ "--stats"; filename ] ->
      let input = read_file filename in
      run_machine ~show_stats:true input
  | [ filename ] ->
      (* Default: generate Uunatek G-code with stats *)
      let input = read_file filename in
      run_machine ~show_stats:true input
  | [] ->
      (* Interactive mode - read from stdin *)
      print_endline "Sketch DSL - Enter your program (Ctrl+D to finish):";
      let input = read_stdin () in
      run_machine ~show_stats:true input
  | _ ->
      prerr_endline usage;
      exit 1
