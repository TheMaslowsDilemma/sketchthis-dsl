(*
-----------------------------------------------------------
parser.mli
-----------------------------------------------------------
recursive descent parser for sketchlang.
*)

type parse_error = { message : string; position : Lexer.position }

exception ParseError of parse_error

val parse : string -> Ast.program
val parse_safe : string -> (Ast.program, parse_error) result
val parse_expr_string : string -> Ast.expr
val format_error : parse_error -> string
