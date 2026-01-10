(* (* lexer.ml - Lexical analyzer for Sketch DSL *)

type token =
  (* Keywords *)
  | POINT | LINE | CURVE | ARC | CIRCLE | RECTANGLE | POLYGON | PATH
  | FROM | TO | THROUGH | AT | BY | AROUND | ALONG | WITH | AND
  | ROTATE | SCALE | REFLECT | TRANSLATE | REPEAT
  | RELATIVE | ABSOLUTE | TIMES | THEN
  | BOUNDED | WITHIN | SYMMETRIC | ACROSS | MIRROR | RADIAL
  | X | Y
  (* Literals *)
  | LPAREN | RPAREN | LBRACKET | RBRACKET
  | COMMA | EQUALS
  | NUMBER of float
  | IDENT of string
  | DEG | RAD
  (* Special *)
  | EOF
  | NEWLINE

let keywords = [
  ("point", POINT); ("line", LINE); ("curve", CURVE); ("arc", ARC);
  ("circle", CIRCLE); ("rectangle", RECTANGLE); ("polygon", POLYGON);
  ("path", PATH);
  ("from", FROM); ("to", TO); ("through", THROUGH); ("at", AT);
  ("by", BY); ("around", AROUND); ("along", ALONG); ("with", WITH);
  ("and", AND);
  ("rotate", ROTATE); ("scale", SCALE); ("reflect", REFLECT);
  ("translate", TRANSLATE); ("repeat", REPEAT);
  ("relative", RELATIVE); ("absolute", ABSOLUTE); ("times", TIMES);
  ("then", THEN);
  ("bounded", BOUNDED); ("within", WITHIN);
  ("symmetric", SYMMETRIC); ("across", ACROSS);
  ("mirror", MIRROR); ("radial", RADIAL);
  ("x", X); ("y", Y);
  ("deg", DEG); ("rad", RAD);
]

let keyword_table = Hashtbl.create (List.length keywords)
let () = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v) keywords

(* Check if a string is a keyword *)
let token_of_ident s =
  let lower = String.lowercase_ascii s in
  match Hashtbl.find_opt keyword_table lower with
  | Some tok -> tok
  | None -> IDENT s

(* Character classification *)
let is_whitespace = function
  | ' ' | '\t' | '\r' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_alphanum c = is_alpha c || is_digit c

(* Tokenizer state *)
type lexer_state = {
  input: string;
  mutable pos: int;
  length: int;
}

let create_lexer input = {
  input;
  pos = 0;
  length = String.length input;
}

let peek state =
  if state.pos >= state.length then None
  else Some (String.get state.input state.pos)

let advance state =
  if state.pos < state.length then
    state.pos <- state.pos + 1

let peek_n state n =
  if state.pos + n >= state.length then None
  else Some (String.get state.input (state.pos + n))

(* Skip whitespace *)
let rec skip_whitespace state =
  match peek state with
  | Some c when is_whitespace c ->
      advance state;
      skip_whitespace state
  | _ -> ()

(* Skip line comment *)
let rec skip_line_comment state =
  match peek state with
  | Some '\n' | None -> ()
  | Some _ ->
      advance state;
      skip_line_comment state

(* Skip block comment *)
let rec skip_block_comment state depth =
  match peek state, peek_n state 1 with
  | Some '(', Some '*' ->
      advance state; advance state;
      skip_block_comment state (depth + 1)
  | Some '*', Some ')' ->
      advance state; advance state;
      if depth = 1 then ()
      else skip_block_comment state (depth - 1)
  | Some _, _ ->
      advance state;
      skip_block_comment state depth
  | None, _ -> 
      failwith "Unclosed block comment"

(* Read a number *)
let read_number state =
  let start = state.pos in
  let rec read_digits () =
    match peek state with
    | Some c when is_digit c ->
        advance state;
        read_digits ()
    | _ -> ()
  in
  
  (* Read integer part *)
  read_digits ();
  
  (* Read decimal part if present *)
  (match peek state with
  | Some '.' ->
      advance state;
      read_digits ()
  | _ -> ());
  
  let num_str = String.sub state.input start (state.pos - start) in
  NUMBER (float_of_string num_str)

(* Read an identifier *)
let read_ident state =
  let start = state.pos in
  let rec read_chars () =
    match peek state with
    | Some c when is_alphanum c ->
        advance state;
        read_chars ()
    | _ -> ()
  in
  
  read_chars ();
  let ident = String.sub state.input start (state.pos - start) in
  token_of_ident ident

(* Main tokenization function *)
let rec next_token state =
  skip_whitespace state;
  
  match peek state with
  | None -> EOF
  
  | Some '\n' ->
      advance state;
      NEWLINE
  
  | Some '#' ->
      skip_line_comment state;
      next_token state
  
  | Some '(' ->
      (match peek_n state 1 with
      | Some '*' ->
          advance state; advance state;
          skip_block_comment state 1;
          next_token state
      | _ ->
          advance state;
          LPAREN)
  
  | Some ')' ->
      advance state;
      RPAREN
  
  | Some '[' ->
      advance state;
      LBRACKET
  
  | Some ']' ->
      advance state;
      RBRACKET
  
  | Some ',' ->
      advance state;
      COMMA
  
  | Some '=' ->
      advance state;
      EQUALS
  
  | Some c when is_digit c ->
      read_number state
  
  | Some '-' ->
      (* Could be negative number or minus operator *)
      advance state;
      (match peek state with
      | Some c when is_digit c ->
          (match read_number state with
          | NUMBER n -> NUMBER (-.n)
          | _ -> failwith "Internal error: read_number returned non-NUMBER")
      | _ -> failwith "Unexpected '-' (only negative numbers supported)")
  
  | Some c when is_alpha c ->
      read_ident state
  
  | Some c ->
      failwith (Printf.sprintf "Unexpected character: '%c'" c)

(* Tokenize entire input *)
let tokenize input =
  let state = create_lexer input in
  let tokens = ref [] in
  let rec loop () =
    let tok = next_token state in
    match tok with
    | EOF -> List.rev (EOF :: !tokens)
    | NEWLINE -> loop ()  (* Skip newlines for now *)
    | _ ->
        tokens := tok :: !tokens;
        loop ()
  in
  loop ()

(* Token to string for debugging *)
let string_of_token = function
  | POINT -> "POINT" | LINE -> "LINE" | CURVE -> "CURVE"
  | ARC -> "ARC" | CIRCLE -> "CIRCLE" | RECTANGLE -> "RECTANGLE"
  | POLYGON -> "POLYGON" | PATH -> "PATH"
  | FROM -> "FROM" | TO -> "TO" | THROUGH -> "THROUGH"
  | AT -> "AT" | BY -> "BY" | AROUND -> "AROUND"
  | ALONG -> "ALONG" | WITH -> "WITH" | AND -> "AND"
  | ROTATE -> "ROTATE" | SCALE -> "SCALE" | REFLECT -> "REFLECT"
  | TRANSLATE -> "TRANSLATE" | REPEAT -> "REPEAT"
  | RELATIVE -> "RELATIVE" | ABSOLUTE -> "ABSOLUTE"
  | TIMES -> "TIMES" | THEN -> "THEN"
  | BOUNDED -> "BOUNDED" | WITHIN -> "WITHIN"
  | SYMMETRIC -> "SYMMETRIC" | ACROSS -> "ACROSS"
  | MIRROR -> "MIRROR" | RADIAL -> "RADIAL"
  | X -> "X" | Y -> "Y"
  | LPAREN -> "(" | RPAREN -> ")"
  | LBRACKET -> "[" | RBRACKET -> "]"
  | COMMA -> "," | EQUALS -> "="
  | NUMBER n -> Printf.sprintf "NUMBER(%g)" n
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | DEG -> "DEG" | RAD -> "RAD"
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"
 *)