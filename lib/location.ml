(*
-----------------------------------------------------------
location.ml
-----------------------------------------------------------
inspired by Ocamls parser
https://github.com/ocaml/ocaml/blob/trunk/parsing/location.mli
*)

type t = { start_loc : Lexer.position; end_loc : Lexer.position }
type 'a loc = { txt : 'a; loc : t }
