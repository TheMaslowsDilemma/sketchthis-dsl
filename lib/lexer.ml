(*
----------------------------------------------------------- 
lexer.ml
----------------------------------------------------------- 
*)

type token =
  | NUMBER of float
  | IDENT of string
  | NUMBER_TYPE
  | VEC_TYPE
  | SKETCH_TYPE
  | DOT
  | DASH
  | STROKE
  | SHADE
  | ARROW
  | TILDE_ARROW
  | TILDE
  | CENTEROF
  | REGIONOF
  | AT
  | ROTATE
  | MIRROR
  | TRANSLATE
  | SCALE
  | SCRIBBLE
  | DRAW
  | TRACE
  | PIPE
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

let format_error e =
  Printf.sprintf "{ \"msg\": \"%s\", \"line\": %d, \"col\": %d }" e.message
    e.position.line e.position.column

type lexer = { input : string; len : int; pos : position }

let token_to_string = function
  | NUMBER f -> Printf.sprintf "number(%g)" f
  | IDENT s -> Printf.sprintf "ident(%s)" s
  | NUMBER_TYPE -> "number_type"
  | VEC_TYPE -> "vec"
  | SKETCH_TYPE -> "sketch"
  | DOT -> "dot"
  | DASH -> "dash"
  | STROKE -> "stroke"
  | SHADE -> "shade"
  | ARROW -> "->"
  | TILDE_ARROW -> "~>"
  | TILDE -> "~"
  | CENTEROF -> "centerof"
  | REGIONOF -> "regionof"
  | AT -> "at"
  | ROTATE -> "rotate"
  | MIRROR -> "mirror"
  | TRANSLATE -> "translate"
  | SCALE -> "scale"
  | SCRIBBLE -> "scribble"
  | DRAW -> "draw"
  | TRACE -> "trace"
  | PIPE -> "|>"
  | LET -> "let"
  | X_AXIS -> "x_axis"
  | Y_AXIS -> "y_axis"
  | X_MAX -> "x_max"
  | Y_MAX -> "y_max"
  | ORIGIN -> "origin"
  | COLON -> ":" (* remove ? or use in sgstns *)
  | EQUALS -> "="
  | LPAREN -> "("
  | RPAREN -> ")"
  | COMMA -> ","
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | NEWLINE -> "NEWLINE"
  | EOF -> "EOF"

let keywords =
  [
    ("number", NUMBER_TYPE);
    ("vec", VEC_TYPE);
    ("sketch", SKETCH_TYPE);
    ("dot", DOT);
    ("dash", DASH);
    ("stroke", STROKE);
    ("shade", SHADE);
    ("centerof", CENTEROF);
    ("regionof", REGIONOF);
    ("at", AT);
    ("rotate", ROTATE);
    ("mirror", MIRROR);
    ("translate", TRANSLATE);
    ("scale", SCALE);
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
let eof l = l.pos.offset >= l.len
let peek l = if eof l then None else Some l.input.[l.pos.offset]

let peek_n l n =
  let i = l.pos.offset + n in
  if i >= l.len then None else Some l.input.[i]

let advance l =
  if eof l then l
  else
    match l.input.[l.pos.offset] with
    | '\n' ->
        {
          l with
          pos = { offset = l.pos.offset + 1; line = l.pos.line + 1; column = 1 };
        }
    | _ ->
        {
          l with
          pos =
            { l.pos with offset = l.pos.offset + 1; column = l.pos.column + 1 };
        }

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
  if l'.pos.offset > l.pos.offset then skip l' else l'

let rec read_digits l acc =
  match peek l with
  | Some c when is_digit c -> read_digits (advance l) (acc ^ String.make 1 c)
  | _ -> (l, acc)

let read_number l =
  let start = l.pos in
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
  let start = l.pos in
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
      | '|' ->
          if peek_n l 1 = Some '>' then (advance (advance l), PIPE)
          else begin
            let message = Printf.sprintf "Expected '>' after '|'" in
            let position = start in
            let err = LexerError { message; position } in
            raise err
          end
      | '~' ->
          if peek_n l 1 = Some '>' then (advance (advance l), TILDE_ARROW)
          else (advance l, TILDE)
      | '-' ->
          let nextch = peek_n l 1 in
          if nextch = Some '>' then (advance (advance l), ARROW)
          else (advance l, MINUS)
      | '0' .. '9' -> read_number l
      | c when is_ident_start c -> read_identifier l
      | c ->
          let message = Printf.sprintf "Unexpected character: '%c'" c in
          let position = start in
          let err = LexerError { message; position } in
          raise err
    in
    (l, { token = tok; start_pos = start; end_pos = l.pos })

let tokenize input =
  let rec go l acc =
    let l, tok = next_token l in
    match tok.token with EOF -> List.rev (tok :: acc) | _ -> go l (tok :: acc)
  in
  let pos = { offset = 0; line = 1; column = 1 } in
  go { input; len = String.length input; pos } []

let tokenize_simple input = tokenize input |> List.map (fun lt -> lt.token)

let tokens_to_string toks =
  toks |> List.map token_to_string |> String.concat " "

let located_tokens_to_string toks =
  toks
  |> List.map (fun lt ->
      Printf.sprintf "%s at %d:%d" (token_to_string lt.token) lt.start_pos.line
        lt.start_pos.column)
  |> String.concat "\n"
