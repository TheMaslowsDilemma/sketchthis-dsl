(* Sketch DSL Lexer *)
(* Tokenizes the Sketch DSL language for art description *)

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
  | SCRIBBLE (* most noise added *)
  | DRAW (* some noise added *)
  | TRACE (* no noise added *)
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

type position = { line : int; column : int; offset : int }
type located_token = { token : token; start_pos : position; end_pos : position }
type lexer_error = { message : string; position : position }

exception LexerError of lexer_error

type lexer = { input : string; pos : int; line : int; column : int }

let create input = { input; pos = 0; line = 1; column = 1 }

let current_position lexer =
  { line = lexer.line; column = lexer.column; offset = lexer.pos }

let is_eof lexer = lexer.pos >= String.length lexer.input

let peek lexer =
  if is_eof lexer then None else Some (String.get lexer.input lexer.pos)

let peek_ahead lexer n =
  let idx = lexer.pos + n in
  if idx >= String.length lexer.input then None
  else Some (String.get lexer.input idx)

let advance lexer =
  if is_eof lexer then lexer
  else
    let c = String.get lexer.input lexer.pos in
    let lexer = { lexer with pos = lexer.pos + 1 } in
    if c = '\n' then { lexer with line = lexer.line + 1; column = 1 }
    else { lexer with column = lexer.column + 1 }

let rec skip_whitespace lexer =
  match peek lexer with
  | Some ' ' | Some '\t' | Some '\r' -> skip_whitespace (advance lexer)
  | _ -> lexer

let rec skip_comment lexer =
  match peek lexer with
  | Some '#' -> skip_until_newline (advance lexer)
  | Some '-' when peek_ahead lexer 1 = Some '-' ->
      skip_until_newline (advance (advance lexer))
  | _ -> lexer

and skip_until_newline lexer =
  match peek lexer with
  | None -> lexer
  | Some '\n' -> lexer
  | _ -> skip_until_newline (advance lexer)

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_ident_start c = is_alpha c || c = '_'
let is_ident_char c = is_alpha c || is_digit c || c = '_'

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

let lookup_keyword word =
  match List.assoc_opt (String.lowercase_ascii word) keywords with
  | Some tok -> tok
  | None -> IDENT word

let rec read_digits lexer acc =
  match peek lexer with
  | Some c when is_digit c -> read_digits (advance lexer) (acc ^ String.make 1 c)
  | _ -> (lexer, acc)

let read_number lexer =
  let start_pos = current_position lexer in

  let lexer, acc =
    match peek lexer with Some '-' -> (advance lexer, "-") | _ -> (lexer, "")
  in
  let lexer, acc = read_digits lexer acc in
  let lexer, acc =
    if
      peek lexer = Some '.'
      && Option.map is_digit (peek_ahead lexer 1) = Some true
    then
      let lexer = advance lexer in
      let acc = acc ^ "." in
      read_digits lexer acc
    else (lexer, acc)
  in

  if String.length acc = 0 || acc = "-" then
    raise (LexerError { message = "Invalid number"; position = start_pos });

  (lexer, NUMBER (float_of_string acc))

let rec read_ident_chars lexer acc =
  match peek lexer with
  | Some c when is_ident_char c ->
      read_ident_chars (advance lexer) (acc ^ String.make 1 c)
  | _ -> (lexer, acc)

let read_identifier lexer =
  let lexer, word = read_ident_chars lexer "" in
  (lexer, lookup_keyword word)

let rec skip_whitespace_and_comments lexer =
  let lexer = skip_whitespace lexer in
  let lexer' = skip_comment lexer in
  let lexer' = skip_whitespace lexer' in
  if lexer'.pos > lexer.pos then skip_whitespace_and_comments lexer' else lexer'

let next_token lexer : lexer * located_token =
  let lexer = skip_whitespace_and_comments lexer in
  let start_pos = current_position lexer in

  if is_eof lexer then (lexer, { token = EOF; start_pos; end_pos = start_pos })
  else
    let c = Option.get (peek lexer) in
    let lexer, token =
      match c with
      | '\n' -> (advance lexer, NEWLINE)
      | ':' -> (advance lexer, COLON)
      | '=' -> (advance lexer, EQUALS)
      | '(' -> (advance lexer, LPAREN)
      | ')' -> (advance lexer, RPAREN)
      | ',' -> (advance lexer, COMMA)
      | '[' -> (advance lexer, LBRACKET)
      | ']' -> (advance lexer, RBRACKET)
      | '+' -> (advance lexer, PLUS)
      | '*' -> (advance lexer, STAR)
      | '/' -> (advance lexer, SLASH)
      | '-' when Option.map is_digit (peek_ahead lexer 1) = Some true ->
          read_number lexer
      | '-' -> (advance lexer, MINUS)
      | '0' .. '9' -> read_number lexer
      | c when is_ident_start c -> read_identifier lexer
      | c ->
          raise
            (LexerError
               {
                 message = Printf.sprintf "Unexpected character: '%c'" c;
                 position = start_pos;
               })
    in
    let end_pos = current_position lexer in
    (lexer, { token; start_pos; end_pos })

let tokenize input : located_token list =
  let rec go lexer acc =
    let lexer, tok = next_token lexer in
    match tok.token with
    | EOF -> List.rev (tok :: acc)
    | _ -> go lexer (tok :: acc)
  in
  go (create input) []

let tokenize_simple input : token list =
  tokenize input |> List.map (fun lt -> lt.token)

let tokens_to_string tokens =
  tokens |> List.map token_to_string |> String.concat " "

let located_tokens_to_string tokens =
  tokens
  |> List.map (fun lt ->
      Printf.sprintf "%s @ %d:%d" (token_to_string lt.token) lt.start_pos.line
        lt.start_pos.column)
  |> String.concat "\n"
