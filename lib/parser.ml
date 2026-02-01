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

(* Location helpers *)
let start_pos p = (current p).start_pos

let end_pos p =
  if p.pos > 0 then p.tokens.(p.pos - 1).end_pos else (current p).start_pos

let loc start_loc end_loc txt : 'a Location.loc =
  { txt; loc = { start_loc; end_loc } }

let loc_to p start txt = loc start (end_pos p) txt

let with_span (a : _ Location.loc) (b : _ Location.loc) txt : _ Location.loc =
  loc a.loc.start_loc b.loc.end_loc txt

(* Parsing helpers *)
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

(* Expression parsing *)
let rec parse_num_expr p = parse_num_add p

and parse_num_add p =
  let rec go (left : num_expr) =
    match peek p with
    | PLUS ->
        advance p;
        let right = parse_num_mul p in
        go (with_span left right (NumAdd (left, right)))
    | MINUS ->
        advance p;
        let right = parse_num_mul p in
        go (with_span left right (NumSub (left, right)))
    | _ -> left
  in
  go (parse_num_mul p)

and parse_num_mul p =
  let rec go (left : num_expr) =
    match peek p with
    | STAR ->
        advance p;
        let right = parse_num_atom p in
        go (with_span left right (NumMul (left, right)))
    | SLASH ->
        advance p;
        let right = parse_num_atom p in
        go (with_span left right (NumDiv (left, right)))
    | _ -> left
  in
  go (parse_num_atom p)

and parse_num_atom p =
  let start = start_pos p in
  match peek p with
  | NUMBER f ->
      advance p;
      loc_to p start (NumLit f)
  | IDENT s ->
      advance p;
      loc_to p start (NumVar s)
  | MINUS ->
      advance p;
      let e = parse_num_atom p in
      loc start e.loc.end_loc (NumNeg e)
  | LPAREN ->
      advance p;
      let e = parse_num_expr p in
      expect p RPAREN;
      loc_to p start e.txt
  | _ -> expected (current p) "numeric expression"

and parse_vec_expr p = parse_vec_add p

and parse_vec_add p =
  let rec go (left : vec_expr) =
    match peek p with
    | PLUS ->
        advance p;
        let right = parse_vec_mul p in
        go (with_span left right (VecAdd (left, right)))
    | MINUS ->
        advance p;
        let right = parse_vec_mul p in
        go (with_span left right (VecSub (left, right)))
    | _ -> left
  in
  go (parse_vec_mul p)

and parse_vec_mul p =
  let start = start_pos p in
  let rec go (left : vec_expr) =
    match peek p with
    | STAR ->
        advance p;
        let right = parse_num_atom p in
        go (with_span left right (VecScale (left, right)))
    | _ -> left
  in
  match peek p with
  | NUMBER _ | MINUS -> (
      let saved = save p in
      try
        let n = parse_num_atom p in
        if check p STAR then (
          advance p;
          let v = parse_vec_mul p in
          go (loc start v.loc.end_loc (VecScale (v, n))))
        else (
          restore p saved;
          expected (current p) "* after number in vector context")
      with ParseError _ ->
        restore p saved;
        expected (current p) "vector expression")
  | _ -> go (parse_vec_atom p)

and parse_vec_atom p =
  let start = start_pos p in
  match peek p with
  | LPAREN -> (
      advance p;
      let saved = save p in
      try
        let first = parse_num_expr p in
        match peek p with
        | COMMA ->
            advance p;
            let second = parse_num_expr p in
            expect p RPAREN;
            let desc =
              match (first.txt, second.txt) with
              | NumLit x, NumLit y -> VecLit (x, y)
              | _ -> VecConstruct (first, second)
            in
            loc_to p start desc
        | RPAREN | PLUS | MINUS ->
            restore p saved;
            let inner = parse_vec_expr p in
            expect p RPAREN;
            loc_to p start inner.txt
        | _ -> expected (current p) ", or ) or vector operator"
      with ParseError _ ->
        restore p saved;
        let inner = parse_vec_expr p in
        expect p RPAREN;
        loc_to p start inner.txt)
  | CENTER ->
      advance p;
      expect p OF;
      let (sk : sketch_expr) = parse_sketch_atom p in
      loc start sk.loc.end_loc (VecCenter sk)
  | ORIGIN ->
      advance p;
      loc_to p start (VecLit (0.0, 0.0))
  | X_MAX ->
      advance p;
      loc_to p start (VecVar "x_max")
  | Y_MAX ->
      advance p;
      loc_to p start (VecVar "y_max")
  | X_AXIS ->
      advance p;
      loc_to p start (VecLit (1.0, 0.0))
  | Y_AXIS ->
      advance p;
      loc_to p start (VecLit (0.0, 1.0))
  | IDENT s ->
      advance p;
      loc_to p start (VecVar s)
  | _ -> expected (current p) "vector expression"

and parse_sketch_atom p =
  let start = start_pos p in
  match peek p with
  | DOT ->
      advance p;
      let v = parse_vec_expr p in
      loc start v.loc.end_loc (Primitive (Dot v))
  | DASH ->
      advance p;
      let v = parse_vec_expr p in
      loc start v.loc.end_loc (Primitive (Dash v))
  | STROKE ->
      advance p;
      let p0 = parse_vec_expr p in
      expect p TO;
      let p1 = parse_vec_expr p in
      if match_tok p VIA then
        let via = parse_vec_list_bracket p in
        loc_to p start (Primitive (Stroke (p0, via, p1)))
      else loc start p1.loc.end_loc (Primitive (Stroke (p0, [], p1)))
  | LBRACKET ->
      advance p;
      skip_nl p;
      let sks = parse_sketch_list p in
      skip_nl p;
      expect p RBRACKET;
      loc_to p start (SketchList sks)
  | IDENT s ->
      advance p;
      loc_to p start (SketchVar s)
  | MIRROR ->
      advance p;
      let (sk : sketch_expr) = parse_sketch_atom p in
      expect p ABOUT;
      let axis = parse_vec_atom p in
      loc start axis.loc.end_loc (MirrorSketch (sk, axis))
  | ROTATE ->
    advance p;
    let (sk : sketch_expr) = parse_sketch_atom p in
    expect p BY;
    let degree = parse_num_atom p in
    loc start degree.loc.end_loc (RotateSketch (sk, degree))
  | TRANSLATE ->
      advance p;
      let (sk : sketch_expr) = parse_sketch_atom p in
      expect p BY;
      let v = parse_vec_atom p in
      loc start v.loc.end_loc (TranslateSketch (sk, v))
  | SCALE ->
      advance p;
      let (sk : sketch_expr) = parse_sketch_atom p in
      expect p BY;
      let n = parse_num_atom p in
      loc start n.loc.end_loc (ScaleSketch (sk, n))
  | LPAREN ->
      advance p;
      let (sk : sketch_expr) = parse_sketch_expr p in
      expect p RPAREN;
      loc_to p start sk.txt
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
  let start = start_pos p in
  expect p LET;
  let name = parse_ident p in
  expect p COLON;
  let typ = parse_type p in
  expect p EQUALS;
  let desc =
    match typ with
    | TNumber -> LetNum (name, parse_num_expr p)
    | TVec -> LetVec (name, parse_vec_expr p)
    | TSketch -> LetSketch (name, parse_sketch_expr p)
  in
  loc_to p start desc

let parse_stmt p =
  skip_nl p;
  let start = start_pos p in
  match peek p with
  | LET -> parse_let p
  | DRAW ->
      advance p;
      let sk = parse_sketch_expr p in
      loc start sk.loc.end_loc (Draw sk)
  | SCRIBBLE ->
      advance p;
      let sk = parse_sketch_expr p in
      loc start sk.loc.end_loc (Scribble sk)
  | TRACE ->
      advance p;
      let sk = parse_sketch_expr p in
      loc start sk.loc.end_loc (Trace sk)
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

let parse input = parse_program (create (Lexer.tokenize input))
let parse_safe input = try Ok (parse input) with ParseError e -> Error e

let parse_sketch_expr_string input =
  parse_sketch_expr (create (Lexer.tokenize input))

let parse_vec_expr_string input = parse_vec_expr (create (Lexer.tokenize input))

let format_error e =
  Printf.sprintf "{ \"msg\": \"%s\", \"line\": %d, \"col\": %d }" e.message
    e.position.line e.position.column
