(*
----------------------------------------------------------- 
lexer.ml
----------------------------------------------------------- 
*)

type token =
  | NUMBER of float
  | IDENT of string
  | NUMBER_TYPE
  | VEC2_TYPE
  | SKETCH_TYPE
  | DOT
  | DASH
  | STROKE
  | FROM
  | TO
  | VIA
  | CENTER
  | OF
  | FLOW
  | AT
  | SCRIBBLE
  | DRAW
  | TRACE
  | LET
  | X_AXIS
  | Y_AXIS
  | X_MAX
  | Y_MAX
  | ORIGIN
  | COLON
  | EQUALS
  | LPAREN
  | RPAREN
  | COMMA
  | LBRACKET
  | RBRACKET
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | NEWLINE
  | EOF

type position = { line : int; column : int; offset : int }
type located_token = { token : token; start_pos : position; end_pos : position }
type lexer_error = { message : string; position : position }

exception LexerError of lexer_error

type lexer = { input : string; pos : int; line : int; col : int }

let token_to_string = function
  | NUMBER f -> Printf.sprintf "NUMBER(%g)" f
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | NUMBER_TYPE -> "NUMBER_TYPE"
  | VEC2_TYPE -> "VEC2_TYPE"
  | SKETCH_TYPE -> "SKETCH_TYPE"
  | DOT -> "DOT"
  | DASH -> "DASH"
  | STROKE -> "STROKE"
  | FROM -> "FROM"
  | TO -> "TO"
  | VIA -> "VIA"
  | CENTER -> "CENTER"
  | OF -> "OF"
  | FLOW -> "FLOW"
  | AT -> "AT"
  | SCRIBBLE -> "SCRIBBLE"
  | DRAW -> "DRAW"
  | TRACE -> "TRACE"
  | LET -> "LET"
  | X_AXIS -> "X_AXIS"
  | Y_AXIS -> "Y_AXIS"
  | X_MAX -> "X_MAX"
  | Y_MAX -> "Y_MAX"
  | ORIGIN -> "ORIGIN"
  | COLON -> "COLON"
  | EQUALS -> "EQUALS"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | COMMA -> "COMMA"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | NEWLINE -> "NEWLINE"
  | EOF -> "EOF"

let keywords =
  [
    ("number", NUMBER_TYPE);
    ("vec", VEC2_TYPE);
    ("sketch", SKETCH_TYPE);
    ("dot", DOT);
    ("dash", DASH);
    ("stroke", STROKE);
    ("from", FROM);
    ("to", TO);
    ("via", VIA);
    ("center", CENTER);
    ("of", OF);
    ("flow", FLOW);
    ("at", AT);
    ("scribble", SCRIBBLE);
    ("draw", DRAW);
    ("trace", TRACE);
    ("let", LET);
    ("x_axis", X_AXIS);
    ("y_axis", Y_AXIS);
    ("x_max", X_MAX);
    ("y_max", Y_MAX);
    ("origin", ORIGIN);
  ]

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_ident_start c = is_alpha c || c = '_'
let is_ident_char c = is_alpha c || is_digit c || c = '_'
let pos l = { line = l.line; column = l.col; offset = l.pos }
let eof l = l.pos >= String.length l.input
let peek l = if eof l then None else Some l.input.[l.pos]

let peek_n l n =
  let i = l.pos + n in
  if i >= String.length l.input then None else Some l.input.[i]

let advance l =
  if eof l then l
  else
    match l.input.[l.pos] with
    | '\n' -> { l with pos = l.pos + 1; line = l.line + 1; col = 1 }
    | _ -> { l with pos = l.pos + 1; col = l.col + 1 }

let rec skip_ws l =
  match peek l with Some (' ' | '\t' | '\r') -> skip_ws (advance l) | _ -> l

let rec skip_line l =
  match peek l with None | Some '\n' -> l | _ -> skip_line (advance l)

let rec skip_comments l =
  match peek l with
  | Some '#' -> skip_comments (skip_ws (skip_line (advance l)))
  | Some '-' when peek_n l 1 = Some '-' ->
      skip_comments (skip_ws (skip_line (advance (advance l))))
  | _ -> l

let rec skip l =
  let l' = skip_comments (skip_ws l) in
  if l'.pos > l.pos then skip l' else l'

let rec read_digits l acc =
  match peek l with
  | Some c when is_digit c -> read_digits (advance l) (acc ^ String.make 1 c)
  | _ -> (l, acc)

let read_number l =
  let start = pos l in
  let l, acc =
    match peek l with Some '-' -> (advance l, "-") | _ -> (l, "")
  in
  let l, acc = read_digits l acc in
  let l, acc =
    if peek l = Some '.' && Option.map is_digit (peek_n l 1) = Some true then
      let l = advance l in
      read_digits l (acc ^ ".")
    else (l, acc)
  in
  if String.length acc = 0 || acc = "-" then
    raise (LexerError { message = "Invalid number"; position = start });
  (l, NUMBER (float_of_string acc))

let rec read_ident l acc =
  match peek l with
  | Some c when is_ident_char c -> read_ident (advance l) (acc ^ String.make 1 c)
  | _ -> (l, acc)

let lookup_keyword w =
  match List.assoc_opt (String.lowercase_ascii w) keywords with
  | Some t -> t
  | None -> IDENT w

let read_identifier l =
  let l, w = read_ident l "" in
  (l, lookup_keyword w)

let next_token l =
  let l = skip l in
  let start = pos l in
  if eof l then (l, { token = EOF; start_pos = start; end_pos = start })
  else
    let c = Option.get (peek l) in
    let l, tok =
      match c with
      | '\n' -> (advance l, NEWLINE)
      | ':' -> (advance l, COLON)
      | '=' -> (advance l, EQUALS)
      | '(' -> (advance l, LPAREN)
      | ')' -> (advance l, RPAREN)
      | ',' -> (advance l, COMMA)
      | '[' -> (advance l, LBRACKET)
      | ']' -> (advance l, RBRACKET)
      | '+' -> (advance l, PLUS)
      | '*' -> (advance l, STAR)
      | '/' -> (advance l, SLASH)
      | '-' when Option.map is_digit (peek_n l 1) = Some true -> read_number l
      | '-' -> (advance l, MINUS)
      | '0' .. '9' -> read_number l
      | c when is_ident_start c -> read_identifier l
      | c ->
          raise
            (LexerError
               {
                 message = Printf.sprintf "Unexpected character: '%c'" c;
                 position = start;
               })
    in
    (l, { token = tok; start_pos = start; end_pos = pos l })

let tokenize input =
  let rec go l acc =
    let l, tok = next_token l in
    match tok.token with EOF -> List.rev (tok :: acc) | _ -> go l (tok :: acc)
  in
  go { input; pos = 0; line = 1; col = 1 } []

let tokenize_simple input = tokenize input |> List.map (fun lt -> lt.token)

let tokens_to_string toks =
  toks |> List.map token_to_string |> String.concat " "

let located_tokens_to_string toks =
  toks
  |> List.map (fun lt ->
      Printf.sprintf "%s @ %d:%d" (token_to_string lt.token) lt.start_pos.line
        lt.start_pos.column)
  |> String.concat "\n"
