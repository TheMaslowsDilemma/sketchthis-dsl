open Lexer
open Ast

(*** Parser Error Handling ***)

type parse_error = { message : string; position : Lexer.position }

exception ParseError of parse_error

let error pos msg = raise (ParseError { message = msg; position = pos })
let error_at tok msg = error tok.start_pos msg

let expected_error tok expected =
  error tok.start_pos
    (Printf.sprintf "Expected %s but got %s" expected
       (token_to_string tok.token))

(*** Parser State ***)

type parser = { tokens : located_token array; pos : int }

let create tokens = { tokens = Array.of_list tokens; pos = 0 }

(*** Token Navigation ***)

let current p =
  if p.pos >= Array.length p.tokens then p.tokens.(Array.length p.tokens - 1)
  else p.tokens.(p.pos)

let peek p = (current p).token
let is_at_end p = match peek p with EOF -> true | _ -> false
let advance p = if is_at_end p then p else { p with pos = p.pos + 1 }
let check p tok = peek p = tok
let check_ident p = match peek p with IDENT _ -> true | _ -> false
let match_token p tok = if check p tok then (advance p, true) else (p, false)

let expect p tok msg =
  if check p tok then advance p else expected_error (current p) msg

let rec skip_newlines p =
  if check p NEWLINE then skip_newlines (advance p) else p

let parse_number p =
  match peek p with
  | NUMBER f -> (advance p, f)
  | _ -> expected_error (current p) "number"

let parse_ident p =
  match peek p with
  | IDENT s -> (advance p, s)
  | _ -> expected_error (current p) "identifier"

let parse_type_annotation p =
  match peek p with
  | NUMBER_TYPE -> (advance p, TNumber)
  | VEC2_TYPE -> (advance p, TVec)
  | SKETCH_TYPE -> (advance p, TSketch)
  | _ -> expected_error (current p) "type (number, vec, or sketch)"

let rec parse_num_expr p : parser * num_expr = parse_num_additive p

and parse_num_additive p : parser * num_expr =
  let p, left = parse_num_multiplicative p in
  parse_num_additive_rest p left

and parse_num_additive_rest p left : parser * num_expr =
  match peek p with
  | PLUS ->
      let p = advance p in
      let p, right = parse_num_multiplicative p in
      parse_num_additive_rest p (NumAdd (left, right))
  | MINUS ->
      let p = advance p in
      let p, right = parse_num_multiplicative p in
      parse_num_additive_rest p (NumSub (left, right))
  | _ -> (p, left)

and parse_num_multiplicative p : parser * num_expr =
  let p, left = parse_num_atom p in
  parse_num_multiplicative_rest p left

and parse_num_multiplicative_rest p left : parser * num_expr =
  match peek p with
  | STAR ->
      let p = advance p in
      let p, right = parse_num_atom p in
      parse_num_multiplicative_rest p (NumMul (left, right))
  | SLASH ->
      let p = advance p in
      let p, right = parse_num_atom p in
      parse_num_multiplicative_rest p (NumDiv (left, right))
  | _ -> (p, left)

and parse_num_atom p : parser * num_expr =
  match peek p with
  | NUMBER f -> (advance p, NumLit f)
  | IDENT s -> (advance p, NumVar s)
  | MINUS ->
      let p = advance p in
      let p, e = parse_num_atom p in
      (p, NumNeg e)
  | LPAREN ->
      let p = advance p in
      let p, expr = parse_num_expr p in
      let p = expect p RPAREN ")" in
      (p, expr)
  | _ -> expected_error (current p) "numeric expression"

and parse_vec_expr p : parser * vec_expr = parse_vec_additive p

and parse_vec_additive p : parser * vec_expr =
  let p, left = parse_vec_multiplicative p in
  parse_vec_additive_rest p left

and parse_vec_additive_rest p left : parser * vec_expr =
  match peek p with
  | PLUS ->
      let p = advance p in
      let p, right = parse_vec_multiplicative p in
      parse_vec_additive_rest p (VecAdd (left, right))
  | MINUS ->
      let p = advance p in
      let p, right = parse_vec_multiplicative p in
      parse_vec_additive_rest p (VecSub (left, right))
  | _ -> (p, left)

and parse_vec_multiplicative p : parser * vec_expr =
  let p, left = parse_vec_atom p in
  parse_vec_multiplicative_rest p left

and parse_vec_multiplicative_rest p left : parser * vec_expr =
  match peek p with
  | STAR ->
      let p = advance p in
      let p, right = parse_num_atom p in
      parse_vec_multiplicative_rest p (VecScale (left, right))
  | _ -> (p, left)

and parse_vec_atom p : parser * vec_expr =
  match peek p with
  (* Vector construction: (expr, expr) *)
  | LPAREN ->
      let p = advance p in
      let p, x = parse_num_expr p in
      let p = expect p COMMA "," in
      let p, y = parse_num_expr p in
      let p = expect p RPAREN ")" in
      let expr =
        match (x, y) with
        | NumLit fx, NumLit fy -> VecLit (fx, fy)
        | _ -> VecConstruct (x, y)
      in
      (p, expr)
  | CENTER ->
      let p = advance p in
      let p = expect p OF "of" in
      let p, sk = parse_sketch_atom p in
      (p, VecCenter sk)
  | FLOW ->
      let p = advance p in
      let p = expect p AT "at" in
      let p, v = parse_vec_atom p in
      (p, VecFlow v)
  | ORIGIN -> (advance p, VecLit (0.0, 0.0))
  | X_MAX -> (advance p, VecVar "x_max")
  | Y_MAX -> (advance p, VecVar "y_max")
  | IDENT s -> (advance p, VecVar s)
  | _ -> expected_error (current p) "vector expression"

and parse_sketch_atom p : parser * sketch_expr =
  match peek p with
  | DOT ->
      let p = advance p in
      let p = expect p AT "at" in
      let p, v = parse_vec_expr p in
      (p, Primitive (Dot v))
  | DASH ->
      let p = advance p in
      let p = expect p AT "at" in
      let p, v = parse_vec_expr p in
      (p, Primitive (Dash v))
  | STROKE ->
      let p = advance p in
      let p = expect p FROM "from" in
      let p, p0 = parse_vec_expr p in
      let p = expect p TO "to" in
      let p, p1 = parse_vec_expr p in
      (* Optional via clause *)
      let p, matched = match_token p VIA in
      let p, via = if matched then parse_vec_list_bracket p else (p, []) in
      (p, Primitive (Stroke (p0, via, p1)))
  | LBRACKET ->
      let p = advance p in
      let p = skip_newlines p in
      let p, sketches = parse_sketch_list p in
      let p = skip_newlines p in
      let p = expect p RBRACKET "]" in
      (p, SketchList sketches)
  | IDENT s -> (advance p, SketchVar s)
  | _ -> expected_error (current p) "sketch expression"

and parse_vec_list_bracket p : parser * vec_expr list =
  let p = expect p LBRACKET "[" in
  let p = skip_newlines p in
  if check p RBRACKET then
    let p = advance p in
    (p, [])
  else
    let p, first = parse_vec_expr p in
    let rec go p acc =
      let p, matched = match_token p COMMA in
      if matched then
        let p = skip_newlines p in
        let p, v = parse_vec_expr p in
        go p (v :: acc)
      else (p, List.rev acc)
    in
    let p, rest = go p [ first ] in
    let p = skip_newlines p in
    let p = expect p RBRACKET "]" in
    (p, rest)

and parse_sketch_list p : parser * sketch_expr list =
  let p = skip_newlines p in
  if check p RBRACKET then (p, [])
  else
    let p, first = parse_sketch_expr p in
    let rec go p acc =
      let p, matched = match_token p COMMA in
      if matched then
        let p = skip_newlines p in
        let p, sk = parse_sketch_expr p in
        go p (sk :: acc)
      else (p, List.rev acc)
    in
    go p [ first ]

and parse_sketch_expr p : parser * sketch_expr = parse_sketch_atom p

let parse_let_binding p : parser * statement =
  let p = expect p LET "let" in
  let p, name = parse_ident p in
  let p = expect p COLON ":" in
  let p, typ = parse_type_annotation p in
  let p = expect p EQUALS "=" in
  match typ with
  | TNumber ->
      let p, expr = parse_num_expr p in
      (p, LetNum (name, expr))
  | TVec ->
      let p, expr = parse_vec_expr p in
      (p, LetVec (name, expr))
  | TSketch ->
      let p, expr = parse_sketch_expr p in
      (p, LetSketch (name, expr))

let parse_draw p : parser * statement =
  let p = expect p DRAW "draw" in
  let p, sk = parse_sketch_expr p in
  (p, Draw sk)

let parse_scribble p : parser * statement =
  let p = expect p SCRIBBLE "scribble" in
  let p, sk = parse_sketch_expr p in
  (p, Scribble sk)

let parse_trace p : parser * statement =
  let p = expect p TRACE "trace" in
  let p, sk = parse_sketch_expr p in
  (p, Trace sk)

let parse_statement p : parser * statement =
  let p = skip_newlines p in
  match peek p with
  | LET -> parse_let_binding p
  | DRAW -> parse_draw p
  | SCRIBBLE -> parse_scribble p
  | TRACE -> parse_trace p
  | _ -> expected_error (current p) "statement (let, draw, scribble, or trace)"

(** Parse a complete program *)
let parse_program p : program =
  let rec go p acc =
    let p = skip_newlines p in
    if is_at_end p then List.rev acc
    else
      let p, stmt = parse_statement p in
      let p = skip_newlines p in
      go p (stmt :: acc)
  in
  go p []

(*** Public Interface ***)

let parse input : program =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  parse_program p

let parse_safe input : (program, parse_error) result =
  try Ok (parse input) with ParseError e -> Error e

let parse_sketch_expr_string input : sketch_expr =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  let _, expr = parse_sketch_expr p in
  expr

let parse_vec_expr_string input : vec_expr =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  let _, expr = parse_vec_expr p in
  expr

let format_error (e : parse_error) : string =
  Printf.sprintf "Parse error at line %d, column %d: %s" e.position.line
    e.position.column e.message
