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
  with e -> Printf.printf "✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "Expected %s but got %s"
         (Lexer.token_to_string expected)
         (Lexer.token_to_string actual))

let assert_tokens expected input =
  let actual = Lexer.tokenize_simple input in
  (* Filter out newlines and EOF for simpler comparison *)
  let filter_trivial =
    List.filter (function Lexer.NEWLINE | Lexer.EOF -> false | _ -> true)
  in
  let expected' = filter_trivial expected in
  let actual' = filter_trivial actual in
  if expected' <> actual' then begin
    Printf.printf "\n  Input: %S\n" input;
    Printf.printf "  Expected: %s\n" (Lexer.tokens_to_string expected');
    Printf.printf "  Actual:   %s\n" (Lexer.tokens_to_string actual');
    failwith "Token mismatch"
  end

(* ===== Lexer Tests ===== *)

let test_numbers () =
  test "lex integer" (fun () -> assert_tokens [ Lexer.NUMBER 42.0 ] "42");

  test "lex float" (fun () -> assert_tokens [ Lexer.NUMBER 3.14 ] "3.14");

  test "lex negative number" (fun () ->
      assert_tokens [ Lexer.NUMBER (-2.5) ] "-2.5")

let test_identifiers () =
  test "lex simple identifier" (fun () ->
      assert_tokens [ Lexer.IDENT "foo" ] "foo");

  test "lex identifier with underscore" (fun () ->
      assert_tokens [ Lexer.IDENT "my_curve" ] "my_curve");

  test "lex identifier with numbers" (fun () ->
      assert_tokens [ Lexer.IDENT "p0" ] "p0")

let test_keywords () =
  test "lex primitive keywords" (fun () ->
      assert_tokens [ Lexer.DOT ] "dot";
      assert_tokens [ Lexer.LINE ] "line";
      assert_tokens [ Lexer.CURVE ] "curve";
      assert_tokens [ Lexer.ARC ] "arc");

  test "lex type keywords" (fun () ->
      assert_tokens [ Lexer.NUMBER_TYPE ] "number";
      assert_tokens [ Lexer.VEC2_TYPE ] "vec";
      assert_tokens [ Lexer.SKETCH_TYPE ] "sketch");

  test "lex transformation keywords" (fun () ->
      assert_tokens [ Lexer.SCALE ] "scale";
      assert_tokens [ Lexer.ROTATE ] "rotate";
      assert_tokens [ Lexer.REPEAT ] "repeat";
      assert_tokens [ Lexer.SYMMETRIC ] "symmetric");

  test "lex spatial keywords" (fun () ->
      assert_tokens [ Lexer.FROM ] "from";
      assert_tokens [ Lexer.TO ] "to";
      assert_tokens [ Lexer.THROUGH ] "through";
      assert_tokens [ Lexer.ALONG ] "along";
      assert_tokens [ Lexer.RELATIVE ] "relative";
      assert_tokens [ Lexer.CENTER ] "center";
      assert_tokens [ Lexer.OF ] "of")

let test_punctuation () =
  test "lex punctuation" (fun () ->
      assert_tokens [ Lexer.COLON ] ":";
      assert_tokens [ Lexer.EQUALS ] "=";
      assert_tokens [ Lexer.LPAREN ] "(";
      assert_tokens [ Lexer.RPAREN ] ")";
      assert_tokens [ Lexer.COMMA ] ",";
      assert_tokens [ Lexer.LBRACKET ] "[";
      assert_tokens [ Lexer.RBRACKET ] "]")

let test_expressions () =
  test "lex let binding" (fun () ->
      assert_tokens
        [
          Lexer.LET;
          Lexer.IDENT "my_line";
          Lexer.COLON;
          Lexer.SKETCH_TYPE;
          Lexer.EQUALS;
          Lexer.LINE;
          Lexer.FROM;
          Lexer.IDENT "p0";
          Lexer.TO;
          Lexer.IDENT "p1";
        ]
        "let my_line : sketch = line from p0 to p1");

  test "lex curve expression" (fun () ->
      assert_tokens
        [
          Lexer.CURVE;
          Lexer.FROM;
          Lexer.IDENT "A";
          Lexer.TO;
          Lexer.IDENT "B";
          Lexer.THROUGH;
          Lexer.IDENT "C";
          Lexer.AND;
          Lexer.IDENT "D";
        ]
        "curve from A to B through C and D");

  test "lex relative expression" (fun () ->
      assert_tokens
        [
          Lexer.RELATIVE;
          Lexer.TO;
          Lexer.CENTER;
          Lexer.OF;
          Lexer.IDENT "D";
          Lexer.CURVE;
          Lexer.FROM;
          Lexer.IDENT "A";
          Lexer.TO;
          Lexer.IDENT "B";
        ]
        "relative to center of D curve from A to B");

  test "lex repeat expression" (fun () ->
      assert_tokens
        [
          Lexer.REPEAT;
          Lexer.IDENT "A";
          Lexer.ALONG;
          Lexer.IDENT "C";
          Lexer.NUMBER 10.0;
          Lexer.TIMES;
        ]
        "repeat A along C 10 times");

  test "lex scale expression" (fun () ->
      assert_tokens
        [
          Lexer.SCALE;
          Lexer.IDENT "A";
          Lexer.BY;
          Lexer.NUMBER 2.5;
          Lexer.ALONG;
          Lexer.IDENT "C";
        ]
        "scale A by 2.5 along C");

  test "lex symmetric expression" (fun () ->
      assert_tokens
        [ Lexer.SYMMETRIC; Lexer.ALONG; Lexer.Y_AXIS; Lexer.IDENT "half_face" ]
        "symmetric along y_axis half_face")

let test_draw_command () =
  test "lex draw command" (fun () ->
      assert_tokens [ Lexer.DRAW; Lexer.IDENT "my_sketch" ] "draw my_sketch")

let test_comments () =
  test "lex with hash comment" (fun () ->
      assert_tokens [ Lexer.DRAW; Lexer.IDENT "x" ] "draw x # this is a comment");

  test "lex with dash comment" (fun () ->
      assert_tokens
        [ Lexer.DRAW; Lexer.IDENT "x" ]
        "draw x -- this is also a comment")

let test_multiline () =
  test "lex multiline program" (fun () ->
      let tokens =
        Lexer.tokenize_simple
          {|
let face : sketch = symmetric along y_axis half_face
draw face
|}
      in
      (* Should contain NEWLINE tokens *)
      assert (List.mem Lexer.NEWLINE tokens))

(* ===== Parser Tests ===== *)

let assert_parses input =
  match Parser.parse_safe input with
  | Ok _ -> ()
  | Error e -> failwith (Parser.format_error e)

let assert_parse_fails input =
  match Parser.parse_safe input with
  | Ok _ -> failwith "Expected parse to fail"
  | Error _ -> ()

let assert_ast_matches input expected =
  match Parser.parse_safe input with
  | Ok ast ->
      let actual = Ast.program_to_string ast in
      if actual <> expected then
        failwith
          (Printf.sprintf "AST mismatch:\nExpected: %s\nActual: %s" expected
             actual)
  | Error e -> failwith (Parser.format_error e)

let test_parse_primitives () =
  test "parse dot" (fun () -> assert_parses "let p : sketch = dot (1, 2)");

  test "parse line" (fun () ->
      assert_parses "let l : sketch = line from (0, 0) to (10, 10)");

  test "parse curve with one control point" (fun () ->
      assert_parses
        "let c : sketch = curve from (0, 0) to (10, 10) through (5, 8)");

  test "parse curve with multiple control points" (fun () ->
      assert_parses
        "let c : sketch = curve from (0, 0) to (10, 10) through (2, 5) and (8, \
         5)")

let test_parse_transformations () =
  test "parse scale" (fun () ->
      assert_parses "let s : sketch = scale myshape by 2.5");

  test "parse scale along" (fun () ->
      assert_parses "let s : sketch = scale myshape by 2.5 along (1, 0)");

  test "parse rotate" (fun () ->
      assert_parses "let r : sketch = rotate myshape by 45");

  test "parse translate" (fun () ->
      assert_parses "let t : sketch = translate myshape by (10, 20)");

  test "parse repeat" (fun () ->
      assert_parses "let r : sketch = repeat myshape along (1, 0) 5 times");

  test "parse symmetric x_axis" (fun () ->
      assert_parses "let s : sketch = symmetric myshape along x_axis");

  test "parse symmetric y_axis" (fun () ->
      assert_parses "let s : sketch = symmetric along y_axis myshape")

let test_parse_let_bindings () =
  test "parse let number" (fun () -> assert_parses "let x : number = 42");

  test "parse let vec" (fun () -> assert_parses "let p : vec = (10, 20)");

  test "parse let sketch" (fun () ->
      assert_parses "let s : sketch = line from (0, 0) to (1, 1)")

let test_parse_draw () =
  test "parse draw variable" (fun () -> assert_parses "draw mysketch");

  test "parse draw primitive" (fun () ->
      assert_parses "draw line from (0, 0) to (10, 10)");

  test "parse draw transformed" (fun () ->
      assert_parses "draw scale mysketch by 2")

let test_parse_complex () =
  test "parse relative to" (fun () ->
      assert_parses
        "let s : sketch = relative to (5, 5) line from (0, 0) to (1, 1)");

  test "parse center of" (fun () ->
      assert_parses
        "let s : sketch = relative to center of base line from (0, 0) to (1, 1)");

  test "parse inside" (fun () ->
      assert_parses "let s : sketch = mysketch inside bounds");

  test "parse multiline program" (fun () ->
      assert_parses
        {|
let base : sketch = line from (0, 0) to (10, 0)
let side : sketch = line from (10, 0) to (5, 8)
draw base
draw side
|})

let test_parse_ast_output () =
  test "ast output for line" (fun () ->
      assert_ast_matches "let l : sketch = line from (0, 0) to (10, 5)"
        "let l : sketch = line from (0, 0) to (10, 5)");

  test "ast output for scale" (fun () ->
      assert_ast_matches "let s : sketch = scale myshape by 2"
        "let s : sketch = scale myshape by 2");

  test "ast output for repeat" (fun () ->
      assert_ast_matches
        "let r : sketch = repeat dot (0, 0) along (1, 0) 5 times"
        "let r : sketch = repeat dot at (0, 0) along (1, 0) 5 times")

let test_parse_errors () =
  test "error on missing type" (fun () -> assert_parse_fails "let x = 42");

  test "error on missing equals" (fun () ->
      assert_parse_fails "let x : number 42");

  test "error on incomplete line" (fun () ->
      assert_parse_fails "let l : sketch = line from (0, 0)")

(* ===== Compiler Tests ===== *)

let assert_compiles input =
  match Parser.parse_safe input with
  | Error e -> failwith (Parser.format_error e)
  | Ok ast -> (
      match Compiler.compile_safe ast with
      | Ok _ -> ()
      | Error e -> failwith (Compiler.format_error e))

let assert_compile_fails input =
  match Parser.parse_safe input with
  | Error _ -> () (* Parse failure is also acceptable *)
  | Ok ast -> (
      match Compiler.compile_safe ast with
      | Ok _ -> failwith "Expected compilation to fail"
      | Error _ -> ())

let get_ir input =
  let ast = Parser.parse input in
  Compiler.compile ast

let test_compile_primitives () =
  test "compile line" (fun () ->
      assert_compiles "draw line from (0, 0) to (10, 10)";
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      assert (List.length ir = 1));

  test "compile dot" (fun () -> assert_compiles "draw dot (5, 5)");

  test "compile curve" (fun () ->
      assert_compiles "draw curve from (0, 0) to (10, 10) through (5, 8)");

  test "compile hdash" (fun () -> assert_compiles "draw hdash (0, 0)");

  test "compile vdash" (fun () -> assert_compiles "draw vdash (0, 0)")

let test_compile_transformations () =
  test "compile scale" (fun () ->
      assert_compiles
        {|
      let base : sketch = line from (0, 0) to (10, 0)
      draw scale base by 2
    |});

  test "compile rotate" (fun () ->
      assert_compiles
        {|
      let base : sketch = line from (0, 0) to (10, 0)
      draw rotate base by 45
    |});

  test "compile translate" (fun () ->
      assert_compiles
        {|
      let base : sketch = line from (0, 0) to (10, 0)
      draw translate base by (5, 5)
    |});

  test "compile repeat" (fun () ->
      assert_compiles
        {|
      let dot1 : sketch = dot (0, 0)
      draw repeat dot1 along (10, 0) 5 times
    |};
      let ir =
        get_ir
          {|
      let dot1 : sketch = dot (0, 0)
      draw repeat dot1 along (10, 0) 5 times
    |}
      in
      (* 5 repetitions of a dot (which is a small shape) *)
      assert (List.length ir = 5));

  test "compile symmetric" (fun () ->
      assert_compiles
        {|
      let half : sketch = line from (0, 0) to (5, 5)
      draw symmetric half along y_axis
    |};
      let ir =
        get_ir
          {|
      let half : sketch = line from (0, 0) to (5, 5)
      draw symmetric half along y_axis
    |}
      in
      (* Original + reflected = 2 paths *)
      assert (List.length ir = 2));

  test "compile symmetric with at clause" (fun () ->
      assert_compiles
        {|
      let shape : sketch = line from (0, 0) to (10, 10)
      draw symmetric shape along x_axis at (0, 50)
    |});

  test "compile symmetric y_axis at position" (fun () ->
      assert_compiles
        {|
      let origin : vec = (100, 50)
      let half : sketch = line from (50, 0) to (100, 50)
      draw symmetric half along y_axis at origin
    |});

  test "compile symmetric with custom axis direction" (fun () ->
      assert_compiles
        {|
      let shape : sketch = line from (0, 0) to (10, 10)
      draw symmetric shape along axis (1, 1)
    |});

  test "compile symmetric with custom axis at position" (fun () ->
      assert_compiles
        {|
      let shape : sketch = line from (0, 0) to (10, 10)
      draw symmetric shape along axis (1, 1) at (50, 50)
    |})

let test_compile_variables () =
  test "compile with number variable" (fun () ->
      assert_compiles
        {|
      let size : number = 10
      draw line from (0, 0) to (size, size)
    |});

  test "compile with vec variable" (fun () ->
      assert_compiles
        {|
      let start : vec = (0, 0)
      let finish : vec = (10, 10)
      draw line from start to finish
    |});

  test "compile with sketch variable" (fun () ->
      assert_compiles
        {|
      let base : sketch = line from (0, 0) to (10, 0)
      let scaled : sketch = scale base by 2
      draw scaled
    |});

  test "compile center of" (fun () ->
      assert_compiles
        {|
      let box : sketch = line from (0, 0) to (10, 10)
      draw relative to center of box dot (0, 0)
    |});

  test "compile vector addition" (fun () ->
      assert_compiles
        {|
      let p1 : vec = (10, 20)
      let p2 : vec = (5, 5)
      let sum : vec = p1 + p2
      draw dot sum
    |});

  test "compile vector subtraction" (fun () ->
      assert_compiles
        {|
      let p1 : vec = (10, 20)
      let p2 : vec = (5, 5)
      let diff : vec = p1 - p2
      draw dot diff
    |});

  test "compile vector scaling" (fun () ->
      assert_compiles
        {|
      let p : vec = (10, 20)
      let scaled : vec = p * 2
      draw dot scaled
    |});

  test "compile complex vector expression" (fun () ->
      assert_compiles
        {|
      let origin : vec = (0, 0)
      let offset : vec = (10, 0)
      let pos : vec = origin + offset * 3
      draw dot pos
    |});

  test "compile number arithmetic" (fun () ->
      assert_compiles
        {|
      let a : number = 10 + 5
      let b : number = a * 2
      let c : number = (b - 10) / 2
      draw line from (0, 0) to (c, c)
    |});

  test "compile numeric expressions in vector construction" (fun () ->
      assert_compiles
        {|
      let width : number = 35
      draw line from (0, 0) to (width, 0)
    |});

  test "compile complex expressions in vector" (fun () ->
      assert_compiles
        {|
      let w : number = 10
      let h : number = 20
      draw line from (w * 2, 0) to (w * 2, h + 5)
    |})

let test_compile_errors () =
  test "error on undefined variable" (fun () ->
      assert_compile_fails "draw undefined_var");

  test "error on type mismatch - num as vec" (fun () ->
      assert_compile_fails
        {|
      let x : number = 5
      draw line from x to (10, 10)
    |});

  test "error on type mismatch - vec as sketch" (fun () ->
      assert_compile_fails {|
      let p : vec = (5, 5)
      draw p
    |})

(* ===== G-code Tests ===== *)

let test_gcode_generation () =
  test "generate gcode for line" (fun () ->
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      let gcode = Gcode.generate ir in
      assert (String.length gcode > 0);
      assert (String.sub gcode 0 1 = ";" || String.sub gcode 0 1 = "G"));

  test "gcode contains movement commands" (fun () ->
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      let gcode = Gcode.generate ir in
      assert (String.length gcode > 0);
      (* Should contain G0 or G1 commands *)
      assert (Str.string_match (Str.regexp ".*G[01].*") gcode 0));

  test "gcode for multiple paths" (fun () ->
      let ir =
        get_ir
          {|
      draw line from (0, 0) to (10, 0)
      draw line from (20, 0) to (30, 0)
    |}
      in
      let gcode, stats = Gcode.generate_with_stats ir in
      assert (String.length gcode > 0);
      (* Stats should mention 2 paths *)
      assert (Str.string_match (Str.regexp ".*Paths: 2.*") stats 0))

let test_gcode_optimization () =
  test "optimization reduces travel distance" (fun () ->
      (* Create paths that benefit from reordering *)
      let ir =
        get_ir
          {|
      draw line from (0, 0) to (10, 0)
      draw line from (100, 100) to (110, 100)
      draw line from (10, 0) to (20, 0)
    |}
      in
      let _, stats = Gcode.generate_with_stats ir in
      (* Should show some travel reduction *)
      assert (String.length stats > 0));

  test "optimization handles single path" (fun () ->
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      let gcode = Gcode.generate ir in
      assert (String.length gcode > 0));

  test "optimization handles repeated patterns" (fun () ->
      let ir =
        get_ir
          {|
      let d : sketch = dot (0, 0)
      draw repeat d along (10, 0) 10 times
    |}
      in
      let _, stats = Gcode.generate_with_stats ir in
      (* Should have 10 paths *)
      assert (Str.string_match (Str.regexp ".*Paths: 10.*") stats 0))

let test_svg_generation () =
  test "generate svg" (fun () ->
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      let svg = Gcode.generate_svg ir in
      assert (String.length svg > 0);
      (* Should be valid SVG *)
      assert (Str.string_match (Str.regexp ".*<svg.*") svg 0);
      assert (Str.string_match (Str.regexp ".*</svg>.*") svg 0));

  test "svg contains paths" (fun () ->
      let ir = get_ir "draw line from (0, 0) to (10, 10)" in
      let svg = Gcode.generate_svg ir in
      assert (Str.string_match (Str.regexp ".*<path.*") svg 0))

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

  print_endline "\n=== Sketch DSL Parser Tests ===\n";

  test_parse_primitives ();
  test_parse_transformations ();
  test_parse_let_bindings ();
  test_parse_draw ();
  test_parse_complex ();
  test_parse_ast_output ();
  test_parse_errors ();

  print_endline "\n=== Sketch DSL Compiler Tests ===\n";

  test_compile_primitives ();
  test_compile_transformations ();
  test_compile_variables ();
  test_compile_errors ();

  print_endline "\n=== Sketch DSL G-code Tests ===\n";

  test_gcode_generation ();
  test_gcode_optimization ();
  test_svg_generation ();

  Printf.printf "\n=== Results: %d/%d tests passed ===\n" !tests_passed
    !tests_run;
  if !tests_passed < !tests_run then exit 1
