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

(* Using mutable state to enable backtracking *)
type parser = { tokens : located_token array; mutable pos : int }

let create tokens = { tokens = Array.of_list tokens; pos = 0 }
let save p = p.pos
let restore p saved = p.pos <- saved

(*** Token Navigation ***)

let current p =
  if p.pos >= Array.length p.tokens then p.tokens.(Array.length p.tokens - 1)
  else p.tokens.(p.pos)

let peek p = (current p).token
let is_at_end p = match peek p with EOF -> true | _ -> false
let advance p = if not (is_at_end p) then p.pos <- p.pos + 1
let check p tok = peek p = tok
let check_ident p = match peek p with IDENT _ -> true | _ -> false

let match_token p tok =
  if check p tok then (
    advance p;
    true)
  else false

let expect p tok msg =
  if check p tok then advance p else expected_error (current p) msg

let rec skip_newlines p =
  if check p NEWLINE then (
    advance p;
    skip_newlines p)

let parse_number p =
  match peek p with
  | NUMBER f ->
      advance p;
      f
  | _ -> expected_error (current p) "number"

let parse_ident p =
  match peek p with
  | IDENT s ->
      advance p;
      s
  | _ -> expected_error (current p) "identifier"

let parse_type_annotation p =
  match peek p with
  | NUMBER_TYPE ->
      advance p;
      TNumber
  | VEC2_TYPE ->
      advance p;
      TVec
  | SKETCH_TYPE ->
      advance p;
      TSketch
  | _ -> expected_error (current p) "type (number, vec, or sketch)"

(* Forward declarations via mutual recursion *)
let rec parse_num_expr p : num_expr = parse_num_additive p

and parse_num_additive p : num_expr =
  let left = parse_num_multiplicative p in
  parse_num_additive_rest p left

and parse_num_additive_rest p left : num_expr =
  match peek p with
  | PLUS ->
      advance p;
      let right = parse_num_multiplicative p in
      parse_num_additive_rest p (NumAdd (left, right))
  | MINUS ->
      advance p;
      let right = parse_num_multiplicative p in
      parse_num_additive_rest p (NumSub (left, right))
  | _ -> left

and parse_num_multiplicative p : num_expr =
  let left = parse_num_atom p in
  parse_num_multiplicative_rest p left

and parse_num_multiplicative_rest p left : num_expr =
  match peek p with
  | STAR ->
      advance p;
      let right = parse_num_atom p in
      parse_num_multiplicative_rest p (NumMul (left, right))
  | SLASH ->
      advance p;
      let right = parse_num_atom p in
      parse_num_multiplicative_rest p (NumDiv (left, right))
  | _ -> left

and parse_num_atom p : num_expr =
  match peek p with
  | NUMBER f ->
      advance p;
      NumLit f
  | IDENT s ->
      advance p;
      NumVar s
  | MINUS ->
      advance p;
      let e = parse_num_atom p in
      NumNeg e
  | LPAREN ->
      advance p;
      let expr = parse_num_expr p in
      expect p RPAREN ")";
      expr
  | _ -> expected_error (current p) "numeric expression"

and parse_vec_expr p : vec_expr = parse_vec_additive p

and parse_vec_additive p : vec_expr =
  let left = parse_vec_multiplicative p in
  parse_vec_additive_rest p left

and parse_vec_additive_rest p left : vec_expr =
  match peek p with
  | PLUS ->
      advance p;
      let right = parse_vec_multiplicative p in
      parse_vec_additive_rest p (VecAdd (left, right))
  | MINUS ->
      advance p;
      let right = parse_vec_multiplicative p in
      parse_vec_additive_rest p (VecSub (left, right))
  | _ -> left

and parse_vec_multiplicative p : vec_expr =
  (* Handle both: num * vec  AND  vec * num *)
  match peek p with
  | NUMBER _ ->
      (* Scalar first: num * vec *)
      let num = parse_num_atom p in
      if check p STAR then begin
        advance p;
        let vec = parse_vec_multiplicative p in
        parse_vec_multiplicative_rest p (VecScale (vec, num))
      end
      else expected_error (current p) "* after number in vector context"
  | MINUS -> (
      (* Could be negative number: -num * vec, or start of something else *)
      let saved = save p in
      try
        let num = parse_num_atom p in
        if check p STAR then begin
          advance p;
          let vec = parse_vec_multiplicative p in
          parse_vec_multiplicative_rest p (VecScale (vec, num))
        end
        else begin
          restore p saved;
          expected_error (current p) "vector expression"
        end
      with ParseError _ ->
        restore p saved;
        expected_error (current p) "vector expression")
  | _ ->
      (* Normal: vec possibly followed by * num *)
      let left = parse_vec_atom p in
      parse_vec_multiplicative_rest p left

and parse_vec_multiplicative_rest p left : vec_expr =
  match peek p with
  | STAR ->
      advance p;
      let right = parse_num_atom p in
      parse_vec_multiplicative_rest p (VecScale (left, right))
  | _ -> left

and parse_vec_atom p : vec_expr =
  match peek p with
  | LPAREN -> (
      advance p;
      let saved = save p in
      try
        let first_num = parse_num_expr p in
        match peek p with
        | COMMA -> (
            (* Vector construction: (num, num) *)
            advance p;
            let second_num = parse_num_expr p in
            expect p RPAREN ")";
            match (first_num, second_num) with
            | NumLit fx, NumLit fy -> VecLit (fx, fy)
            | _ -> VecConstruct (first_num, second_num))
        | RPAREN ->
            (* Just (num_expr) - not valid as vec_atom, so backtrack and try vec_expr *)
            restore p saved;
            let inner = parse_vec_expr p in
            expect p RPAREN ")";
            inner
        | PLUS | MINUS ->
            (* After num_expr we see + or -, this must be vec arithmetic.
               Backtrack and parse as vec_expr *)
            restore p saved;
            let inner = parse_vec_expr p in
            expect p RPAREN ")";
            inner
        | _ -> expected_error (current p) ", or ) or vector operator"
      with ParseError _ ->
        (* Parsing as num_expr failed, try as vec_expr *)
        restore p saved;
        let inner = parse_vec_expr p in
        expect p RPAREN ")";
        inner)
  | CENTER ->
      advance p;
      expect p OF "of";
      let sk = parse_sketch_atom p in
      VecCenter sk
  | FLOW ->
      advance p;
      expect p AT "at";
      let v = parse_vec_atom p in
      VecFlow v
  | ORIGIN ->
      advance p;
      VecLit (0.0, 0.0)
  | X_MAX ->
      advance p;
      VecVar "x_max"
  | Y_MAX ->
      advance p;
      VecVar "y_max"
  | IDENT s ->
      advance p;
      VecVar s
  | _ -> expected_error (current p) "vector expression"

and parse_sketch_atom p : sketch_expr =
  match peek p with
  | DOT ->
      advance p;
      expect p AT "at";
      let v = parse_vec_expr p in
      Primitive (Dot v)
  | DASH ->
      advance p;
      expect p AT "at";
      let v = parse_vec_expr p in
      Primitive (Dash v)
  | STROKE ->
      advance p;
      expect p FROM "from";
      let p0 = parse_vec_expr p in
      expect p TO "to";
      let p1 = parse_vec_expr p in
      let via = if match_token p VIA then parse_vec_list_bracket p else [] in
      Primitive (Stroke (p0, via, p1))
  | LBRACKET ->
      advance p;
      skip_newlines p;
      let sketches = parse_sketch_list p in
      skip_newlines p;
      expect p RBRACKET "]";
      SketchList sketches
  | IDENT s ->
      advance p;
      SketchVar s
  | _ -> expected_error (current p) "sketch expression"

and parse_vec_list_bracket p : vec_expr list =
  expect p LBRACKET "[";
  skip_newlines p;
  if check p RBRACKET then begin
    advance p;
    []
  end
  else begin
    let first = parse_vec_expr p in
    let rec go acc =
      if match_token p COMMA then begin
        skip_newlines p;
        let v = parse_vec_expr p in
        go (v :: acc)
      end
      else List.rev acc
    in
    let result = go [ first ] in
    skip_newlines p;
    expect p RBRACKET "]";
    result
  end

and parse_sketch_list p : sketch_expr list =
  skip_newlines p;
  if check p RBRACKET then []
  else begin
    let first = parse_sketch_expr p in
    let rec go acc =
      if match_token p COMMA then begin
        skip_newlines p;
        let sk = parse_sketch_expr p in
        go (sk :: acc)
      end
      else List.rev acc
    in
    go [ first ]
  end

and parse_sketch_expr p : sketch_expr = parse_sketch_atom p

let parse_let_binding p : statement =
  expect p LET "let";
  let name = parse_ident p in
  expect p COLON ":";
  let typ = parse_type_annotation p in
  expect p EQUALS "=";
  match typ with
  | TNumber ->
      let expr = parse_num_expr p in
      LetNum (name, expr)
  | TVec ->
      let expr = parse_vec_expr p in
      LetVec (name, expr)
  | TSketch ->
      let expr = parse_sketch_expr p in
      LetSketch (name, expr)

let parse_draw p : statement =
  expect p DRAW "draw";
  let sk = parse_sketch_expr p in
  Draw sk

let parse_scribble p : statement =
  expect p SCRIBBLE "scribble";
  let sk = parse_sketch_expr p in
  Scribble sk

let parse_trace p : statement =
  expect p TRACE "trace";
  let sk = parse_sketch_expr p in
  Trace sk

let parse_statement p : statement =
  skip_newlines p;
  match peek p with
  | LET -> parse_let_binding p
  | DRAW -> parse_draw p
  | SCRIBBLE -> parse_scribble p
  | TRACE -> parse_trace p
  | _ -> expected_error (current p) "statement (let, draw, scribble, or trace)"

(** Parse a complete program *)
let parse_program p : program =
  let rec go acc =
    skip_newlines p;
    if is_at_end p then List.rev acc
    else begin
      let stmt = parse_statement p in
      skip_newlines p;
      go (stmt :: acc)
    end
  in
  go []

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
  parse_sketch_expr p

let parse_vec_expr_string input : vec_expr =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  parse_vec_expr p

let format_error (e : parse_error) : string =
  Printf.sprintf "Parse error at line %d, column %d: %s" e.position.line
    e.position.column e.message
