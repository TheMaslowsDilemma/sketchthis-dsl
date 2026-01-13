(* Sketch DSL Lexer *)
(* Tokenizes the Sketch DSL language for art description *)

(** Token types for the Sketch DSL *)
type token =
  (* Literals *)
  | NUMBER of float (* Numeric literals: 1.0, 2.5, -3.14 *)
  | IDENT of string (* Identifiers: my_curve, face, p0 *)
  (* Keywords - Types *)
  | NUMBER_TYPE
  | VEC2_TYPE
  | SKETCH_TYPE
  (* Keywords - Primitives *)
  | DOT
  | DASH
  | STROKE
  (* Keywords - Spatial Relations *)
  | FROM (* stroke FROM ... *)
  | TO (* stroke from <vec2> TO ... *)
  | VIA (* stroke from <vec2> to <vec2> VIA <list vev2> *)
  | CENTER (* center of <sketch> *)
  | OF (* center *)
  | FLOW (* flow at <vec2> *)
  | AT (* "at" *)
  (* Keywords - Commands *)
  | SCRIBBLE
    (* imprecise: most noise added to underlying sketch points and additional points added to strokes *)
  | DRAW
    (* normal precision: some noise added to underlying sketch points and minimal additional points added to strokes *)
  | TRACE (* most precise: drawn exactly as sketch is *)
  | LET
  (* Keywords - Global Constants *)
  | X_AXIS
  | Y_AXIS
  | X_MAX
  | Y_MAX
  | ORIGIN
  (* Punctuation *)
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
  (* Special *)
  | NEWLINE (* End of statement *)
  | EOF (* End of file *)

(** Convert a token to a human-readable string *)
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
(** Lexer position tracking *)

type located_token = { token : token; start_pos : position; end_pos : position }
(** Token with position information *)

type lexer_error = { message : string; position : position }
(** Lexer error type *)

exception LexerError of lexer_error

type lexer = { input : string; pos : int; line : int; column : int }
(** Lexer state *)

(** Create a new lexer from input string *)
let create input = { input; pos = 0; line = 1; column = 1 }

(** Get current position *)
let current_position lexer =
  { line = lexer.line; column = lexer.column; offset = lexer.pos }

(** Check if at end of input *)
let is_eof lexer = lexer.pos >= String.length lexer.input

(** Peek at current character without consuming *)
let peek lexer =
  if is_eof lexer then None else Some (String.get lexer.input lexer.pos)

(** Peek at character n positions ahead *)
let peek_ahead lexer n =
  let idx = lexer.pos + n in
  if idx >= String.length lexer.input then None
  else Some (String.get lexer.input idx)

(** Advance the lexer by one character, returning new lexer state *)
let advance lexer =
  if is_eof lexer then lexer
  else
    let c = String.get lexer.input lexer.pos in
    let lexer = { lexer with pos = lexer.pos + 1 } in
    if c = '\n' then { lexer with line = lexer.line + 1; column = 1 }
    else { lexer with column = lexer.column + 1 }

(** Skip whitespace (but not newlines - those are tokens) *)
let rec skip_whitespace lexer =
  match peek lexer with
  | Some ' ' | Some '\t' | Some '\r' -> skip_whitespace (advance lexer)
  | _ -> lexer

(** Skip line comments starting with # *)
let rec skip_comment lexer =
  match peek lexer with
  | Some '#' -> skip_until_newline (advance lexer)
  | Some '-' when peek_ahead lexer 1 = Some '-' ->
      skip_until_newline (advance (advance lexer))
  | _ -> lexer

and skip_until_newline lexer =
  match peek lexer with
  | None -> lexer
  | Some '\n' -> lexer (* Don't consume the newline - it's a token *)
  | _ -> skip_until_newline (advance lexer)

(** Check if character is a digit *)
let is_digit = function '0' .. '9' -> true | _ -> false

(** Check if character is alphabetic *)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

(** Check if character can start an identifier *)
let is_ident_start c = is_alpha c || c = '_'

(** Check if character can be part of an identifier *)
let is_ident_char c = is_alpha c || is_digit c || c = '_'

(** Keyword lookup table *)
let keywords =
  [
    (* Types *)
    ("number", NUMBER_TYPE);
    ("vec2", VEC2_TYPE);
    ("sketch", SKETCH_TYPE);
    (* Primitives *)
    ("dot", DOT);
    ("dash", DASH);
    ("stroke", STROKE);
    (* Spatial relations *)
    ("from", FROM);
    ("to", TO);
    ("via", VIA);
    ("center", CENTER);
    ("of", OF);
    ("flow", FLOW);
    ("at", AT);
    (* Commands *)
    ("scribble", SCRIBBLE);
    ("draw", DRAW);
    ("trace", TRACE);
    ("let", LET);
    (* Global constants *)
    ("x_axis", X_AXIS);
    ("y_axis", Y_AXIS);
    ("x_max", X_MAX);
    ("y_max", Y_MAX);
    ("origin", ORIGIN);
  ]

(** Look up a word to see if it's a keyword *)
let lookup_keyword word =
  match List.assoc_opt (String.lowercase_ascii word) keywords with
  | Some tok -> tok
  | None -> IDENT word

(** Read digits into accumulator, returning (updated lexer, accumulated string)
*)
let rec read_digits lexer acc =
  match peek lexer with
  | Some c when is_digit c -> read_digits (advance lexer) (acc ^ String.make 1 c)
  | _ -> (lexer, acc)

(** Read a number (integer or float), returning (updated lexer, token) *)
let read_number lexer =
  let start_pos = current_position lexer in

  (* Optional negative sign *)
  let lexer, acc =
    match peek lexer with Some '-' -> (advance lexer, "-") | _ -> (lexer, "")
  in

  (* Integer part *)
  let lexer, acc = read_digits lexer acc in

  (* Decimal part *)
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

(** Read identifier characters into accumulator *)
let rec read_ident_chars lexer acc =
  match peek lexer with
  | Some c when is_ident_char c ->
      read_ident_chars (advance lexer) (acc ^ String.make 1 c)
  | _ -> (lexer, acc)

(** Read an identifier or keyword, returning (updated lexer, token) *)
let read_identifier lexer =
  let lexer, word = read_ident_chars lexer "" in
  (lexer, lookup_keyword word)

(** Skip whitespace and comments *)
let rec skip_whitespace_and_comments lexer =
  let lexer = skip_whitespace lexer in
  let lexer' = skip_comment lexer in
  let lexer' = skip_whitespace lexer' in
  (* Keep going if we skipped something *)
  if lexer'.pos > lexer.pos then skip_whitespace_and_comments lexer' else lexer'

(** Get the next token, returning (updated lexer, located token) *)
let next_token lexer : lexer * located_token =
  let lexer = skip_whitespace_and_comments lexer in
  let start_pos = current_position lexer in

  if is_eof lexer then (lexer, { token = EOF; start_pos; end_pos = start_pos })
  else
    let c = Option.get (peek lexer) in
    let lexer, token =
      match c with
      (* Newline *)
      | '\n' -> (advance lexer, NEWLINE)
      (* Single-character tokens *)
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
      (* Minus can be operator or start of negative number *)
      | '-' when Option.map is_digit (peek_ahead lexer 1) = Some true ->
          read_number lexer
      | '-' -> (advance lexer, MINUS)
      (* Numbers *)
      | '0' .. '9' -> read_number lexer
      (* Identifiers and keywords *)
      | c when is_ident_start c -> read_identifier lexer
      (* Unknown character *)
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

(** Tokenize entire input, returning list of located tokens *)
let tokenize input : located_token list =
  let rec go lexer acc =
    let lexer, tok = next_token lexer in
    match tok.token with
    | EOF -> List.rev (tok :: acc)
    | _ -> go lexer (tok :: acc)
  in
  go (create input) []

(** Tokenize and return just the tokens (without positions) *)
let tokenize_simple input : token list =
  tokenize input |> List.map (fun lt -> lt.token)

(** Pretty-print a list of tokens *)
let tokens_to_string tokens =
  tokens |> List.map token_to_string |> String.concat " "

(** Pretty-print located tokens with positions *)
let located_tokens_to_string tokens =
  tokens
  |> List.map (fun lt ->
      Printf.sprintf "%s @ %d:%d" (token_to_string lt.token) lt.start_pos.line
        lt.start_pos.column)
  |> String.concat "\n"
