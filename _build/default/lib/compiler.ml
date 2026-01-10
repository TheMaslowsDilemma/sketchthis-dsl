(* (* compiler.ml - Main compiler driver for Sketch DSL *)

open Ast

(* Compilation result *)
type compile_result =
  | Success of string
  | Error of string

(* Compile source code to G-code *)
let compile ?(config=Gcode.default_config) source =
  try
    (* Lexical analysis *)
    let tokens = Lexer.tokenize source in
    
    (* Parsing *)
    let ast = Parser.parse tokens in
    
    (* TODO: Type checking and semantic analysis *)
    
    (* TODO: Bounds checking *)
    
    (* TODO: Optimization *)
    
    (* G-code generation *)
    let gcode = Gcode.generate ~config ast in
    
    Success gcode
  with
  | Lexer.ParseError msg -> Error (Printf.sprintf "Lexer error: %s" msg)
  | Parser.ParseError msg -> Error (Printf.sprintf "Parse error: %s" msg)
  | Failure msg -> Error (Printf.sprintf "Compilation error: %s" msg)
  | e -> Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(* Compile file *)
let compile_file ?(config=Gcode.default_config) input_file output_file =
  try
    let source = 
      let ic = open_in input_file in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    in
    
    match compile ~config source with
    | Success gcode ->
        let oc = open_out output_file in
        output_string oc gcode;
        close_out oc;
        Printf.printf "Successfully compiled %s to %s\n" input_file output_file
    | Error msg ->
        Printf.eprintf "Compilation failed: %s\n" msg;
        exit 1
  with
  | Sys_error msg ->
      Printf.eprintf "File error: %s\n" msg;
      exit 1

(* Interactive REPL *)
let repl () =
  let config = Gcode.default_config in
  
  Printf.printf "Sketch DSL REPL (type 'exit' to quit)\n";
  Printf.printf "Enter Sketch code and press Ctrl+D to compile\n\n";
  
  let rec loop () =
    Printf.printf "sketch> ";
    flush stdout;
    
    try
      (* Read multiline input *)
      let lines = ref [] in
      (try
        while true do
          lines := input_line stdin :: !lines
        done
      with finof_file -> ());
      
      let source = String.concat "\n" (List.rev !lines) in
      
      if String.trim source = "exit" then
        Printf.printf "Goodbye!\n"
      else if String.trim source = "" then
        loop ()
      else begin
        match compile ~config source with
        | Success gcode ->
            Printf.printf "\n--- Generated G-code ---\n%s\n" gcode;
            loop ()
        | Error msg ->
            Printf.eprintf "Error: %s\n\n" msg;
            loop ()
      end
    with
    | finof_file -> Printf.printf "\nGoodbye!\n"
  in
  
  loop ()
 *)