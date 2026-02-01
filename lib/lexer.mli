(***
----------------------------------------------------------- 
lexer.mli
----------------------------------------------------------- 
tokenizer for the sketch dsl language.
***)

type token =
  | NUMBER of float
  | IDENT of string
  | NUMBER_TYPE
  | VEC_TYPE
  | SKETCH_TYPE
  | DOT
  | DASH
  | AT
  | STROKE
  | TO
  | VIA
  | CENTER
  | OF
  | ROTATE
  | MIRROR
  | ABOUT
  | TRANSLATE
  | SCALE
  | BY
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

val tokenize : string -> located_token list
val tokenize_simple : string -> token list
val token_to_string : token -> string
val tokens_to_string : token list -> string
val located_tokens_to_string : located_token list -> string
val format_error : lexer_error -> string 
