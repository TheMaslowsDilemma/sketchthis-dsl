(*
----------------------------------------------------------- 
parser.ml
----------------------------------------------------------- 
*)

open Lexer
open Ast

type parse_error = { message : string; position : Lexer.position }

exception ParseError of parse_error

type parser = { tokens : located_token array; mutable pos : int }

let error pos msg = raise (ParseError { message = msg; position = pos })

let expected tok exp =
  error tok.start_pos
    (Printf.sprintf "Expected %s but got %s" exp (token_to_string tok.token))

let create toks = { tokens = Array.of_list toks; pos = 0 }
let save p = p.pos
let restore p s = p.pos <- s

let current p =
  if p.pos >= Array.length p.tokens then p.tokens.(Array.length p.tokens - 1)
  else p.tokens.(p.pos)

let peek p = (current p).token
let at_end p = peek p = EOF
let advance p = if not (at_end p) then p.pos <- p.pos + 1
let check p t = peek p = t

let match_tok p t =
  if check p t then (
    advance p;
    true)
  else false

let expect p t =
  if check p t then advance p else expected (current p) (token_to_string t)

let rec skip_nl p =
  if check p NEWLINE then (
    advance p;
    skip_nl p)

let parse_ident p =
  match peek p with
  | IDENT s ->
      advance p;
      s
  | _ -> expected (current p) "identifier"

let parse_type p =
  match peek p with
  | NUMBER_TYPE ->
      advance p;
      TNumber
  | VEC_TYPE ->
      advance p;
      TVec
  | SKETCH_TYPE ->
      advance p;
      TSketch
  | _ -> expected (current p) "type (number, vec, or sketch)"

let rec parse_num_expr p = parse_num_add p

and parse_num_add p =
  let rec go left =
    match peek p with
    | PLUS ->
        advance p;
        go (NumAdd (left, parse_num_mul p))
    | MINUS ->
        advance p;
        go (NumSub (left, parse_num_mul p))
    | _ -> left
  in
  go (parse_num_mul p)

and parse_num_mul p =
  let rec go left =
    match peek p with
    | STAR ->
        advance p;
        go (NumMul (left, parse_num_atom p))
    | SLASH ->
        advance p;
        go (NumDiv (left, parse_num_atom p))
    | _ -> left
  in
  go (parse_num_atom p)

and parse_num_atom p =
  match peek p with
  | NUMBER f ->
      advance p;
      NumLit f
  | IDENT s ->
      advance p;
      NumVar s
  | MINUS ->
      advance p;
      NumNeg (parse_num_atom p)
  | LPAREN ->
      advance p;
      let e = parse_num_expr p in
      expect p RPAREN;
      e
  | _ -> expected (current p) "numeric expression"

and parse_vec_expr p = parse_vec_add p

and parse_vec_add p =
  let rec go left =
    match peek p with
    | PLUS ->
        advance p;
        go (VecAdd (left, parse_vec_mul p))
    | MINUS ->
        advance p;
        go (VecSub (left, parse_vec_mul p))
    | _ -> left
  in
  go (parse_vec_mul p)

and parse_vec_mul p =
  let rec go_mul left =
    match peek p with
    | STAR ->
        advance p;
        go_mul (VecScale (left, parse_num_atom p))
    | _ -> left
  in
  match peek p with
  | NUMBER _ | MINUS -> (
      let saved = save p in
      try
        let n = parse_num_atom p in
        if check p STAR then (
          advance p;
          go_mul (VecScale (parse_vec_mul p, n)))
        else (
          restore p saved;
          expected (current p) "* after number in vector context")
      with ParseError _ ->
        restore p saved;
        expected (current p) "vector expression")
  | _ -> go_mul (parse_vec_atom p)

and parse_vec_atom p =
  match peek p with
  | LPAREN -> (
      advance p;
      let saved = save p in
      try
        let first = parse_num_expr p in
        match peek p with
        | COMMA -> (
            advance p;
            let second = parse_num_expr p in
            expect p RPAREN;
            match (first, second) with
            | NumLit x, NumLit y -> VecLit (x, y)
            | _ -> VecConstruct (first, second))
        | RPAREN | PLUS | MINUS ->
            restore p saved;
            let inner = parse_vec_expr p in
            expect p RPAREN;
            inner
        | _ -> expected (current p) ", or ) or vector operator"
      with ParseError _ ->
        restore p saved;
        let inner = parse_vec_expr p in
        expect p RPAREN;
        inner)
  | CENTER ->
      advance p;
      expect p OF;
      VecCenter (parse_sketch_atom p)
  | ORIGIN ->
      advance p;
      VecLit (0.0, 0.0)
  | X_MAX ->
      advance p;
      VecVar "x_max"
  | Y_MAX ->
      advance p;
      VecVar "y_max"
  | X_AXIS ->
      advance p;
      VecLit (1.0, 0.0)
  | Y_AXIS ->
      advance p;
      VecLit (0.0, 1.0)
  | IDENT s ->
      advance p;
      VecVar s
  | _ -> expected (current p) "vector expression"

and parse_sketch_atom p =
  match peek p with
  | DOT ->
      advance p;
      expect p AT;
      Primitive (Dot (parse_vec_expr p))
  | DASH ->
      advance p;
      expect p AT;
      Primitive (Dash (parse_vec_expr p))
  | STROKE ->
      advance p;
      expect p FROM;
      let p0 = parse_vec_expr p in
      expect p TO;
      let p1 = parse_vec_expr p in
      let via = if match_tok p VIA then parse_vec_list_bracket p else [] in
      Primitive (Stroke (p0, via, p1))
  | LBRACKET ->
      advance p;
      skip_nl p;
      let sks = parse_sketch_list p in
      skip_nl p;
      expect p RBRACKET;
      SketchList sks
  | IDENT s ->
      advance p;
      SketchVar s
  | MIRROR ->
      advance p;
      let sk = parse_sketch_atom p in
      expect p ABOUT;
      let axis = parse_vec_atom p in
      MirrorSketch (sk, axis)
  | _ -> expected (current p) "sketch expression"

and parse_vec_list_bracket p =
  expect p LBRACKET;
  skip_nl p;
  if check p RBRACKET then (
    advance p;
    [])
  else
    let rec go acc =
      if match_tok p COMMA then (
        skip_nl p;
        go (parse_vec_expr p :: acc))
      else List.rev acc
    in
    let result = go [ parse_vec_expr p ] in
    skip_nl p;
    expect p RBRACKET;
    result

and parse_sketch_list p =
  skip_nl p;
  if check p RBRACKET then []
  else
    let rec go acc =
      if match_tok p COMMA then (
        skip_nl p;
        go (parse_sketch_expr p :: acc))
      else List.rev acc
    in
    go [ parse_sketch_expr p ]

and parse_sketch_expr p = parse_sketch_atom p

let parse_let p =
  expect p LET;
  let name = parse_ident p in
  expect p COLON;
  let typ = parse_type p in
  expect p EQUALS;
  match typ with
  | TNumber -> LetNum (name, parse_num_expr p)
  | TVec -> LetVec (name, parse_vec_expr p)
  | TSketch -> LetSketch (name, parse_sketch_expr p)

let parse_stmt p =
  skip_nl p;
  match peek p with
  | LET -> parse_let p
  | DRAW ->
      advance p;
      Draw (parse_sketch_expr p)
  | SCRIBBLE ->
      advance p;
      Scribble (parse_sketch_expr p)
  | TRACE ->
      advance p;
      Trace (parse_sketch_expr p)
  | _ -> expected (current p) "statement (let, draw, scribble, or trace)"

let parse_program p =
  let rec go acc =
    skip_nl p;
    if at_end p then List.rev acc
    else
      let s = parse_stmt p in
      skip_nl p;
      go (s :: acc)
  in
  go []

let parse input =
  let p = create (Lexer.tokenize input) in
  parse_program p

let parse_safe input = try Ok (parse input) with ParseError e -> Error e

let parse_sketch_expr_string input =
  let p = create (Lexer.tokenize input) in
  parse_sketch_expr p

let parse_vec_expr_string input =
  let p = create (Lexer.tokenize input) in
  parse_vec_expr p

let format_error e =
  Printf.sprintf "Parse error at line %d, column %d: %s" e.position.line
    e.position.column e.message
