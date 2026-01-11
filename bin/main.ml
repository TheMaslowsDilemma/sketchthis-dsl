(* Sketch DSL - Main Entry Point *)
(* Currently demonstrates the lexer *)

open Sketch_dsl

let usage = {|
Sketch DSL Compiler

Usage: sketch_dsl [options] [file]

Options:
  --lex         Tokenize input and print tokens
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
  with End_of_file ->
    Buffer.contents buf

let run_lexer input =
  try
    let tokens = Lexer.tokenize input in
    print_endline "=== Tokens ===";
    print_endline (Lexer.located_tokens_to_string tokens);
    print_endline "";
    print_endline "=== Compact ===";
    print_endline (Lexer.tokens_to_string (List.map (fun t -> t.Lexer.token) tokens))
  with Lexer.LexerError err ->
    Printf.eprintf "Lexer error at line %d, column %d: %s\n"
      err.position.line
      err.position.column
      err.message;
    exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  
  match args with
  | ["--help"] | ["-h"] ->
    print_endline usage
  
  | ["--lex"] ->
    (* Read from stdin *)
    let input = read_stdin () in
    run_lexer input
  
  | ["--lex"; filename] ->
    let input = read_file filename in
    run_lexer input
  
  | [filename] ->
    (* Default: just lex for now *)
    let input = read_file filename in
    run_lexer input
  
  | [] ->
    (* Interactive mode - read from stdin *)
    print_endline "Sketch DSL - Enter your program (Ctrl+D to finish):";
    let input = read_stdin () in
    run_lexer input
  
  | _ ->
    prerr_endline usage;
    exit 1
