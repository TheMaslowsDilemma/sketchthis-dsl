(*
-----------------------------------------------------------
parser.ml
-----------------------------------------------------------
precedence levels (lowest to highest):
  pipe       expr ("|>" transform)*
  add        mul  (("+"|"-") mul)*
  mul        unary (("*"|"/") unary)*
  unary      "-" unary | atom
  atom       literals, parens, primitives, prefix transforms
*)

open Lexer
open Ast

type parse_error = { message : string; position : Lexer.position }

exception ParseError of parse_error

type parser = { tokens : located_token array; mutable pos : int }

let error pos msg = raise (ParseError { message = msg; position = pos })

let expected tok exp =
  error tok.start_pos
    (Printf.sprintf "Expected '%s' but got '%s'" exp
       (token_to_string tok.token))

let create toks = { tokens = Array.of_list toks; pos = 0 }

let current p =
  if p.pos >= Array.length p.tokens then p.tokens.(Array.length p.tokens - 1)
  else p.tokens.(p.pos)

let peek p = (current p).token
let end_of_program p = peek p = EOF
let end_of_section p = (peek p = AT_SYM || end_of_program p)

let advance p = if not (end_of_program p) then p.pos <- p.pos + 1
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

let start_pos p = (current p).start_pos

let end_pos p =
  if p.pos > 0 then p.tokens.(p.pos - 1).end_pos else (current p).start_pos

let loc s e txt : 'a Location.loc =
  { txt; loc = { start_loc = s; end_loc = e } }

let loc_to p start txt = loc start (end_pos p) txt

let with_span (a : _ Location.loc) (b : _ Location.loc) txt : _ Location.loc =
  loc a.loc.start_loc b.loc.end_loc txt

let parse_ident p =
  match peek p with
  | IDENT s ->
      advance p;
      s
  | _ -> expected (current p) "identifier"

(* Expression parsing *)

let rec parse_expr p = parse_pipe p

and parse_pipe p =
  let rec go (left : expr) =
    if match_tok p PIPE then go (parse_pipe_transform p left) else left
  in
  go (parse_add p)

and parse_pipe_transform p (left : expr) =
  let start = left.loc.start_loc in
  match peek p with
  | TRANSLATE ->
      advance p;
      let (a : expr) = parse_atom p in
      loc start a.loc.end_loc (Translate (left, a))
  | SCALE ->
      advance p;
      let a = parse_atom p in
      loc start a.loc.end_loc (Scale (left, a))
  | ROTATE ->
      advance p;
      let a = parse_atom p in
      loc start a.loc.end_loc (Rotate (left, a))
  | MIRROR ->
      advance p;
      let a = parse_atom p in
      loc start a.loc.end_loc (Mirror (left, a))
  | AT ->
      advance p;
      let a = parse_atom p in
      loc start a.loc.end_loc (At (left, a))
  | NEWLINE ->
      advance p;
      parse_pipe_transform p left
  | _ -> expected (current p) "transform (translate, scale, rotate, mirror, at)"

and parse_add p =
  let rec go (left : expr) =
    match peek p with
    | PLUS ->
        advance p;
        let right = parse_mul p in
        go (with_span left right (Add (left, right)))
    | MINUS ->
        advance p;
        let right = parse_mul p in
        go (with_span left right (Sub (left, right)))
    | _ -> left
  in
  go (parse_mul p)

and parse_mul p =
  let rec go (left : expr) =
    match peek p with
    | STAR ->
        advance p;
        let right = parse_unary p in
        go (with_span left right (Mul (left, right)))
    | SLASH ->
        advance p;
        let right = parse_unary p in
        go (with_span left right (Div (left, right)))
    | _ -> left
  in
  go (parse_unary p)

and parse_unary p =
  let start = start_pos p in
  match peek p with
  | MINUS ->
      advance p;
      let e = parse_unary p in
      loc start e.loc.end_loc (Neg e)
  | _ -> parse_atom p

and parse_atom p =
  let start = start_pos p in
  match peek p with
  | MINUS -> parse_unary p
  | NUMBER f ->
      advance p;
      loc_to p start (Lit f)
  | IDENT s ->
      advance p;
      loc_to p start (Var s)
  | LPAREN ->
      advance p;
      let first = parse_expr p in
      if match_tok p COMMA then (
        let second = parse_expr p in
        expect p RPAREN;
        loc_to p start (Vec (first, second)))
      else (
        expect p RPAREN;
        loc_to p start first.txt)
  | ORIGIN ->
      advance p;
      let z = loc_to p start (Lit 0.0) in
      loc_to p start (Vec (z, z))
  | X_AXIS ->
      advance p;
      let one = loc_to p start (Lit 1.0) in
      let zero = loc_to p start (Lit 0.0) in
      loc_to p start (Vec (one, zero))
  | Y_AXIS ->
      advance p;
      let zero = loc_to p start (Lit 0.0) in
      let one = loc_to p start (Lit 1.0) in
      loc_to p start (Vec (zero, one))
  | X_MAX ->
      advance p;
      loc_to p start (Var "x_max")
  | Y_MAX ->
      advance p;
      loc_to p start (Var "y_max")
  | CENTEROF ->
      advance p;
      let e = parse_atom p in
      loc start e.loc.end_loc (CenterOf e)
  | REGIONOF ->
      advance p;
      let e = parse_atom p in
      loc start e.loc.end_loc (RegionOf e)
  | SHADE ->
      advance p;
      let e = parse_atom p in
      loc start e.loc.end_loc (Shade e)
  | DOT -> (
      advance p;
      match peek p with
      | LBRACKET ->
          let pts = parse_bracket_list p in
          let dotmaker (v : expr) = loc v.loc.start_loc v.loc.end_loc (Dot v) in
          loc_to p start (SketchList (List.map dotmaker pts))
      | _ ->
          let v = parse_atom p in
          loc start v.loc.end_loc (Dot v))
  | DASH -> (
      advance p;
      match peek p with
      | LBRACKET ->
          let pts = parse_bracket_list p in
          let dashmaker (v : expr) =
            loc v.loc.start_loc v.loc.end_loc (Dash v)
          in
          loc_to p start (SketchList (List.map dashmaker pts))
      | _ ->
          let v = parse_atom p in
          loc start v.loc.end_loc (Dash v))
  | STROKE -> (
      advance p;
      match peek p with
      | ARROW ->
          advance p;
          let pts = parse_bracket_list p in
          loc_to p start (Segments pts)
      | TILDE_ARROW ->
          advance p;
          let pts = parse_bracket_list p in
          loc_to p start (Splines pts)
      | _ -> expected (current p) "'~>' or '->'")
  | LBRACKET ->
      advance p;
      skip_nl p;
      let items = parse_comma_list p RBRACKET in
      skip_nl p;
      expect p RBRACKET;
      loc_to p start (SketchList items)
  | MIRROR ->
      advance p;
      let sk = parse_atom p in
      let axis = parse_atom p in
      loc start axis.loc.end_loc (Mirror (sk, axis))
  | ROTATE ->
      advance p;
      let sk = parse_atom p in
      let angle = parse_atom p in
      loc start angle.loc.end_loc (Rotate (sk, angle))
  | TRANSLATE ->
      advance p;
      let sk = parse_atom p in
      let v = parse_atom p in
      loc start v.loc.end_loc (Translate (sk, v))
  | SCALE ->
      advance p;
      let sk = parse_atom p in
      let n = parse_atom p in
      loc start n.loc.end_loc (Scale (sk, n))
  | AT ->
      advance p;
      let sk = parse_atom p in
      let v = parse_atom p in
      loc start v.loc.end_loc (At (sk, v))
  | NEWLINE ->
      advance p;
      parse_atom p
  | _ -> expected (current p) "expression"

and parse_bracket_list p =
  expect p LBRACKET;
  skip_nl p;
  if check p RBRACKET then (
    advance p;
    [])
  else
    let items = parse_comma_list p RBRACKET in
    skip_nl p;
    expect p RBRACKET;
    items

and parse_comma_list p closing =
  skip_nl p;
  if check p closing then []
  else
    let rec go acc =
      if match_tok p COMMA then (
        skip_nl p;
        go (parse_expr p :: acc))
      else List.rev acc
    in
    go [ parse_expr p ]

(* Statements *)

let parse_let p =
  let start = start_pos p in
  expect p LET;
  let name = parse_ident p in
  expect p EQUALS;
  let e = parse_expr p in
  loc_to p start (Let (name, e))

let parse_stmt p =
  skip_nl p;
  let start = start_pos p in
  match peek p with
  | LET -> parse_let p
  | DRAW ->
      advance p;
      let e = parse_expr p in
      loc start e.loc.end_loc (Draw e)
  | SCRIBBLE ->
      advance p;
      let e = parse_expr p in
      loc start e.loc.end_loc (Scribble e)
  | TRACE ->
      advance p;
      let e = parse_expr p in
      loc start e.loc.end_loc (Trace e)
  | _ -> expected (current p) "statement (let, draw, scribble, or trace)"

let parse_section p nm =
  let rec parse_body acc =
    skip_nl p;
    if end_of_section p then List.rev acc
    else
      let s = parse_stmt p in
      skip_nl p;
      go (s :: acc)
  in
  let start = start_pos p in
  let name =
    if nm == "" then begin
      expect p AT_SYM; advance p;
      parse_ident p
    end else nm
  in
  let body = parse_body [] in
  let psection = { name; body } in
  loc_to p start psection

let parse_program p =
  let rec go acc =
    skip_nl p;
    if end_of_program p then List.rev acc
    else
      let start = 
      let s = parse_section p "" in
      go (s :: acc)
  in
  let dflts = parse_section "default" in
  go [ dflts ]

(* Public API *)

let parse input = parse_program (create (Lexer.tokenize input))
let parse_safe input = try Ok (parse input) with ParseError e -> Error e
let parse_expr_string input = parse_expr (create (Lexer.tokenize input))

let format_error e =
  Printf.sprintf "{ \"msg\": \"%s\", \"line\": %d, \"col\": %d }" e.message
    e.position.line e.position.column
