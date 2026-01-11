(* Sketch DSL Lexer *)
(* Tokenizes the Sketch DSL language for art description *)

(** Token types for the Sketch DSL *)
type token =
  (* Literals *)
  | NUMBER of float          (* Numeric literals: 1.0, 2.5, -3.14 *)
  | IDENT of string          (* Identifiers: my_curve, face, p0 *)
  
  (* Keywords - Types *)
  | NUMBER_TYPE              (* "number" *)
  | VEC2_TYPE                (* "vec2" *)
  | SKETCH_TYPE              (* "sketch" *)
  
  (* Keywords - Primitives *)
  | DOT                      (* "dot" *)
  | HDASH                    (* "hdash" *)
  | VDASH                    (* "vdash" *)
  | LINE                     (* "line" *)
  | CURVE                    (* "curve" *)
  | ARC                      (* "arc" *)
  
  (* Keywords - Transformations *)
  | SCALE                    (* "scale" *)
  | ROTATE                   (* "rotate" *)
  | TRANSLATE                (* "translate" *)
  | REPEAT                   (* "repeat" *)
  | SYMMETRIC                (* "symmetric" *)
  
  (* Keywords - Spatial Relations *)
  | FROM                     (* "from" *)
  | TO                       (* "to" *)
  | THROUGH                  (* "through" *)
  | AND                      (* "and" *)
  | ALONG                    (* "along" *)
  | BY                       (* "by" *)
  | TIMES                    (* "times" *)
  | RELATIVE                 (* "relative" *)
  | INSIDE                   (* "inside" *)
  | CENTER                   (* "center" *)
  | OF                       (* "of" *)
  | AT                       (* "at" *)
  
  (* Keywords - Commands *)
  | DRAW                     (* "draw" *)
  | LET                      (* "let" - for bindings *)
  
  (* Keywords - Axes *)
  | X_AXIS                   (* "x_axis" *)
  | Y_AXIS                   (* "y_axis" *)
  
  (* Punctuation *)
  | COLON                    (* ":" *)
  | EQUALS                   (* "=" *)
  | LPAREN                   (* "(" *)
  | RPAREN                   (* ")" *)
  | COMMA                    (* "," *)
  | LBRACKET                 (* "[" *)
  | RBRACKET                 (* "]" *)
  | PLUS                     (* "+" *)
  | MINUS                    (* "-" *)
  | STAR                     (* "*" *)
  | SLASH                    (* "/" *)
  
  (* Special *)
  | NEWLINE                  (* End of statement *)
  | EOF                      (* End of file *)

(** Convert a token to a human-readable string *)
let token_to_string = function
  | NUMBER f -> Printf.sprintf "NUMBER(%g)" f
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | NUMBER_TYPE -> "NUMBER_TYPE"
  | VEC2_TYPE -> "VEC2_TYPE"
  | SKETCH_TYPE -> "SKETCH_TYPE"
  | DOT -> "DOT"
  | HDASH -> "HDASH"
  | VDASH -> "VDASH"
  | LINE -> "LINE"
  | CURVE -> "CURVE"
  | ARC -> "ARC"
  | SCALE -> "SCALE"
  | ROTATE -> "ROTATE"
  | TRANSLATE -> "TRANSLATE"
  | REPEAT -> "REPEAT"
  | SYMMETRIC -> "SYMMETRIC"
  | FROM -> "FROM"
  | TO -> "TO"
  | THROUGH -> "THROUGH"
  | AND -> "AND"
  | ALONG -> "ALONG"
  | BY -> "BY"
  | TIMES -> "TIMES"
  | RELATIVE -> "RELATIVE"
  | INSIDE -> "INSIDE"
  | CENTER -> "CENTER"
  | OF -> "OF"
  | AT -> "AT"
  | DRAW -> "DRAW"
  | LET -> "LET"
  | X_AXIS -> "X_AXIS"
  | Y_AXIS -> "Y_AXIS"
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

(** Lexer position tracking *)
type position = {
  line: int;
  column: int;
  offset: int;
}

(** Token with position information *)
type located_token = {
  token: token;
  start_pos: position;
  end_pos: position;
}

(** Lexer error type *)
type lexer_error = {
  message: string;
  position: position;
}

exception LexerError of lexer_error

(** Lexer state *)
type lexer = {
  input: string;
  mutable pos: int;
  mutable line: int;
  mutable column: int;
}

(** Create a new lexer from input string *)
let create input = {
  input;
  pos = 0;
  line = 1;
  column = 1;
}

(** Get current position *)
let current_position lexer = {
  line = lexer.line;
  column = lexer.column;
  offset = lexer.pos;
}

(** Check if at end of input *)
let is_eof lexer = lexer.pos >= String.length lexer.input

(** Peek at current character without consuming *)
let peek lexer =
  if is_eof lexer then None
  else Some (String.get lexer.input lexer.pos)

(** Peek at character n positions ahead *)
let peek_ahead lexer n =
  let idx = lexer.pos + n in
  if idx >= String.length lexer.input then None
  else Some (String.get lexer.input idx)

(** Advance the lexer by one character *)
let advance lexer =
  if not (is_eof lexer) then begin
    let c = String.get lexer.input lexer.pos in
    lexer.pos <- lexer.pos + 1;
    if c = '\n' then begin
      lexer.line <- lexer.line + 1;
      lexer.column <- 1
    end else
      lexer.column <- lexer.column + 1
  end

(** Skip whitespace (but not newlines - those are tokens) *)
let rec skip_whitespace lexer =
  match peek lexer with
  | Some ' ' | Some '\t' | Some '\r' ->
    advance lexer;
    skip_whitespace lexer
  | _ -> ()

(** Skip line comments starting with # or -- *)
let skip_comment lexer =
  match peek lexer with
  | Some '#' ->
    (* Skip until end of line *)
    while not (is_eof lexer) && peek lexer <> Some '\n' do
      advance lexer
    done
  | Some '-' when peek_ahead lexer 1 = Some '-' ->
    (* Skip until end of line *)
    while not (is_eof lexer) && peek lexer <> Some '\n' do
      advance lexer
    done
  | _ -> ()

(** Check if character is a digit *)
let is_digit = function '0'..'9' -> true | _ -> false

(** Check if character is alphabetic *)
let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false

(** Check if character can start an identifier *)
let is_ident_start c = is_alpha c || c = '_'

(** Check if character can be part of an identifier *)
let is_ident_char c = is_alpha c || is_digit c || c = '_'

(** Keyword lookup table *)
let keywords = [
  (* Types *)
  ("number", NUMBER_TYPE);
  ("vec2", VEC2_TYPE);
  ("sketch", SKETCH_TYPE);
  
  (* Primitives *)
  ("dot", DOT);
  ("hdash", HDASH);
  ("vdash", VDASH);
  ("line", LINE);
  ("curve", CURVE);
  ("arc", ARC);
  
  (* Transformations *)
  ("scale", SCALE);
  ("rotate", ROTATE);
  ("translate", TRANSLATE);
  ("repeat", REPEAT);
  ("symmetric", SYMMETRIC);
  
  (* Spatial relations *)
  ("from", FROM);
  ("to", TO);
  ("through", THROUGH);
  ("and", AND);
  ("along", ALONG);
  ("by", BY);
  ("times", TIMES);
  ("relative", RELATIVE);
  ("inside", INSIDE);
  ("center", CENTER);
  ("of", OF);
  ("at", AT);
  
  (* Commands *)
  ("draw", DRAW);
  ("let", LET);
  
  (* Axes *)
  ("x_axis", X_AXIS);
  ("y_axis", Y_AXIS);
]

(** Look up a word to see if it's a keyword *)
let lookup_keyword word =
  match List.assoc_opt (String.lowercase_ascii word) keywords with
  | Some tok -> tok
  | None -> IDENT word

(** Read a number (integer or float) *)
let read_number lexer =
  let start_pos = lexer.pos in
  let buf = Buffer.create 16 in
  
  (* Optional negative sign *)
  (match peek lexer with
   | Some '-' ->
     Buffer.add_char buf '-';
     advance lexer
   | _ -> ());
  
  (* Integer part *)
  while not (is_eof lexer) && Option.map is_digit (peek lexer) = Some true do
    Buffer.add_char buf (Option.get (peek lexer));
    advance lexer
  done;
  
  (* Decimal part *)
  if peek lexer = Some '.' && Option.map is_digit (peek_ahead lexer 1) = Some true then begin
    Buffer.add_char buf '.';
    advance lexer;
    while not (is_eof lexer) && Option.map is_digit (peek lexer) = Some true do
      Buffer.add_char buf (Option.get (peek lexer));
      advance lexer
    done
  end;
  
  let s = Buffer.contents buf in
  if String.length s = 0 || s = "-" then
    raise (LexerError {
      message = "Invalid number";
      position = { line = lexer.line; column = lexer.column; offset = start_pos }
    });
  
  NUMBER (float_of_string s)

(** Read an identifier or keyword *)
let read_identifier lexer =
  let buf = Buffer.create 16 in
  
  while not (is_eof lexer) && Option.map is_ident_char (peek lexer) = Some true do
    Buffer.add_char buf (Option.get (peek lexer));
    advance lexer
  done;
  
  let word = Buffer.contents buf in
  lookup_keyword word

(** Get the next token *)
let next_token lexer : located_token =
  (* Skip whitespace and comments *)
  skip_whitespace lexer;
  skip_comment lexer;
  skip_whitespace lexer;
  
  let start_pos = current_position lexer in
  
  if is_eof lexer then
    { token = EOF; start_pos; end_pos = start_pos }
  else
    let c = Option.get (peek lexer) in
    let token = match c with
      (* Newline *)
      | '\n' ->
        advance lexer;
        NEWLINE
      
      (* Single-character tokens *)
      | ':' -> advance lexer; COLON
      | '=' -> advance lexer; EQUALS
      | '(' -> advance lexer; LPAREN
      | ')' -> advance lexer; RPAREN
      | ',' -> advance lexer; COMMA
      | '[' -> advance lexer; LBRACKET
      | ']' -> advance lexer; RBRACKET
      | '+' -> advance lexer; PLUS
      | '*' -> advance lexer; STAR
      | '/' -> advance lexer; SLASH
      
      (* Minus can be operator or start of negative number *)
      | '-' when Option.map is_digit (peek_ahead lexer 1) = Some true ->
        read_number lexer
      | '-' -> advance lexer; MINUS
      | '0'..'9' ->
        read_number lexer
      
      (* Identifiers and keywords *)
      | c when is_ident_start c ->
        read_identifier lexer
      
      (* Unknown character *)
      | c ->
        raise (LexerError {
          message = Printf.sprintf "Unexpected character: '%c'" c;
          position = start_pos
        })
    in
    let end_pos = current_position lexer in
    { token; start_pos; end_pos }

(** Tokenize entire input, returning list of tokens *)
let tokenize input : located_token list =
  let lexer = create input in
  let rec go acc =
    let tok = next_token lexer in
    match tok.token with
    | EOF -> List.rev (tok :: acc)
    | _ -> go (tok :: acc)
  in
  go []

(** Tokenize and return just the tokens (without positions) *)
let tokenize_simple input : token list =
  tokenize input |> List.map (fun lt -> lt.token)

(** Pretty-print a list of tokens *)
let tokens_to_string tokens =
  tokens
  |> List.map token_to_string
  |> String.concat " "

(** Pretty-print located tokens with positions *)
let located_tokens_to_string tokens =
  tokens
  |> List.map (fun lt ->
       Printf.sprintf "%s @ %d:%d"
         (token_to_string lt.token)
         lt.start_pos.line
         lt.start_pos.column)
  |> String.concat "\n"
