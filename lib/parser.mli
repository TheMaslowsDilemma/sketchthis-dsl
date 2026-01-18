(***
----------------------------------------------------------- 
parser.mli
----------------------------------------------------------- 
recursive descent parser for the sketch dsl language.
***)

type parse_error = { message : string; position : Lexer.position }

exception ParseError of parse_error

val parse : string -> Ast.program
val parse_safe : string -> (Ast.program, parse_error) result
val parse_sketch_expr_string : string -> Ast.sketch_expr
val parse_vec_expr_string : string -> Ast.vec_expr
val format_error : parse_error -> string
