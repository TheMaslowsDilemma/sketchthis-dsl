(* Sketch DSL Tests *)

open Sketch_dsl

(* Simple test framework *)
let tests_run = ref 0
let tests_passed = ref 0

let test name f =
  incr tests_run;
  try
    f ();
    incr tests_passed;
    Printf.printf "✓ %s\n" name
  with e ->
    Printf.printf "✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Expected %s but got %s" 
                (Lexer.token_to_string expected) 
                (Lexer.token_to_string actual))

let assert_tokens expected input =
  let actual = Lexer.tokenize_simple input in
  (* Filter out newlines and EOF for simpler comparison *)
  let filter_trivial = List.filter (function 
    | Lexer.NEWLINE | Lexer.EOF -> false 
    | _ -> true) in
  let expected' = filter_trivial expected in
  let actual' = filter_trivial actual in
  if expected' <> actual' then begin
    Printf.printf "\n  Input: %S\n" input;
    Printf.printf "  Expected: %s\n" (Lexer.tokens_to_string expected');
    Printf.printf "  Actual:   %s\n" (Lexer.tokens_to_string actual');
    failwith "Token mismatch"
  end

(* === Lexer Tests === *)

let test_numbers () =
  test "lex integer" (fun () ->
    assert_tokens [Lexer.NUMBER 42.0] "42");
  
  test "lex float" (fun () ->
    assert_tokens [Lexer.NUMBER 3.14] "3.14");
  
  test "lex negative number" (fun () ->
    assert_tokens [Lexer.NUMBER (-2.5)] "-2.5")

let test_identifiers () =
  test "lex simple identifier" (fun () ->
    assert_tokens [Lexer.IDENT "foo"] "foo");
  
  test "lex identifier with underscore" (fun () ->
    assert_tokens [Lexer.IDENT "my_curve"] "my_curve");
  
  test "lex identifier with numbers" (fun () ->
    assert_tokens [Lexer.IDENT "p0"] "p0")

let test_keywords () =
  test "lex primitive keywords" (fun () ->
    assert_tokens [Lexer.DOT] "dot";
    assert_tokens [Lexer.LINE] "line";
    assert_tokens [Lexer.CURVE] "curve";
    assert_tokens [Lexer.ARC] "arc");
  
  test "lex type keywords" (fun () ->
    assert_tokens [Lexer.NUMBER_TYPE] "number";
    assert_tokens [Lexer.VEC2_TYPE] "vec2";
    assert_tokens [Lexer.SKETCH_TYPE] "sketch");
  
  test "lex transformation keywords" (fun () ->
    assert_tokens [Lexer.SCALE] "scale";
    assert_tokens [Lexer.ROTATE] "rotate";
    assert_tokens [Lexer.REPEAT] "repeat";
    assert_tokens [Lexer.SYMMETRIC] "symmetric");
  
  test "lex spatial keywords" (fun () ->
    assert_tokens [Lexer.FROM] "from";
    assert_tokens [Lexer.TO] "to";
    assert_tokens [Lexer.THROUGH] "through";
    assert_tokens [Lexer.ALONG] "along";
    assert_tokens [Lexer.RELATIVE] "relative";
    assert_tokens [Lexer.CENTER] "center";
    assert_tokens [Lexer.OF] "of")

let test_punctuation () =
  test "lex punctuation" (fun () ->
    assert_tokens [Lexer.COLON] ":";
    assert_tokens [Lexer.EQUALS] "=";
    assert_tokens [Lexer.LPAREN] "(";
    assert_tokens [Lexer.RPAREN] ")";
    assert_tokens [Lexer.COMMA] ",";
    assert_tokens [Lexer.LBRACKET] "[";
    assert_tokens [Lexer.RBRACKET] "]")

let test_expressions () =
  test "lex let binding" (fun () ->
    assert_tokens 
      [Lexer.LET; Lexer.IDENT "my_line"; Lexer.COLON; Lexer.SKETCH_TYPE; 
       Lexer.EQUALS; Lexer.LINE; Lexer.FROM; Lexer.IDENT "p0"; 
       Lexer.TO; Lexer.IDENT "p1"]
      "let my_line : sketch = line from p0 to p1");
  
  test "lex curve expression" (fun () ->
    assert_tokens
      [Lexer.CURVE; Lexer.FROM; Lexer.IDENT "A"; Lexer.TO; Lexer.IDENT "B";
       Lexer.THROUGH; Lexer.IDENT "C"; Lexer.AND; Lexer.IDENT "D"]
      "curve from A to B through C and D");
  
  test "lex relative expression" (fun () ->
    assert_tokens
      [Lexer.RELATIVE; Lexer.TO; Lexer.CENTER; Lexer.OF; Lexer.IDENT "D";
       Lexer.CURVE; Lexer.FROM; Lexer.IDENT "A"; Lexer.TO; Lexer.IDENT "B"]
      "relative to center of D curve from A to B");
  
  test "lex repeat expression" (fun () ->
    assert_tokens
      [Lexer.REPEAT; Lexer.IDENT "A"; Lexer.ALONG; Lexer.IDENT "C";
       Lexer.NUMBER 10.0; Lexer.TIMES]
      "repeat A along C 10 times");
  
  test "lex scale expression" (fun () ->
    assert_tokens
      [Lexer.SCALE; Lexer.IDENT "A"; Lexer.BY; Lexer.NUMBER 2.5;
       Lexer.ALONG; Lexer.IDENT "C"]
      "scale A by 2.5 along C");
  
  test "lex symmetric expression" (fun () ->
    assert_tokens
      [Lexer.SYMMETRIC; Lexer.ALONG; Lexer.Y_AXIS; Lexer.IDENT "half_face"]
      "symmetric along y_axis half_face")

let test_draw_command () =
  test "lex draw command" (fun () ->
    assert_tokens
      [Lexer.DRAW; Lexer.IDENT "my_sketch"]
      "draw my_sketch")

let test_comments () =
  test "lex with hash comment" (fun () ->
    assert_tokens
      [Lexer.DRAW; Lexer.IDENT "x"]
      "draw x # this is a comment");
  
  test "lex with dash comment" (fun () ->
    assert_tokens
      [Lexer.DRAW; Lexer.IDENT "x"]
      "draw x -- this is also a comment")

let test_multiline () =
  test "lex multiline program" (fun () ->
    let tokens = Lexer.tokenize_simple {|
let face : sketch = symmetric along y_axis half_face
draw face
|} in
    (* Should contain NEWLINE tokens *)
    assert (List.mem Lexer.NEWLINE tokens))

let () =
  print_endline "=== Sketch DSL Lexer Tests ===\n";
  
  test_numbers ();
  test_identifiers ();
  test_keywords ();
  test_punctuation ();
  test_expressions ();
  test_draw_command ();
  test_comments ();
  test_multiline ();
  
  Printf.printf "\n=== Results: %d/%d tests passed ===\n" !tests_passed !tests_run;
  if !tests_passed < !tests_run then exit 1
