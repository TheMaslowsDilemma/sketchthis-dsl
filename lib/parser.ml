(* Sketch DSL Parser *)
(* Recursive descent parser that produces an AST from tokens *)

open Lexer
open Ast

(* ===== Parser Error Handling ===== *)

type parse_error = {
  message: string;
  position: Lexer.position;
}

exception ParseError of parse_error

let error pos msg = raise (ParseError { message = msg; position = pos })

let error_at tok msg = 
  error tok.start_pos msg

let expected_error tok expected =
  error tok.start_pos 
    (Printf.sprintf "Expected %s but got %s" expected (token_to_string tok.token))

(* ===== Parser State ===== *)

type parser = {
  tokens: located_token array;
  mutable pos: int;
}

let create tokens = {
  tokens = Array.of_list tokens;
  pos = 0;
}

(* ===== Token Navigation ===== *)

(** Get current token *)
let current p =
  if p.pos >= Array.length p.tokens then
    p.tokens.(Array.length p.tokens - 1)  (* Return EOF *)
  else
    p.tokens.(p.pos)

(** Peek at the current token's type *)
let peek p = (current p).token

(** Check if at end of input *)
let is_at_end p = 
  match peek p with EOF -> true | _ -> false

(** Advance to next token, skipping newlines if skip_newlines is true *)
let advance ?(skip_newlines=false) p =
  if not (is_at_end p) then
    p.pos <- p.pos + 1;
  if skip_newlines then
    while peek p = NEWLINE && not (is_at_end p) do
      p.pos <- p.pos + 1
    done

(** Check if current token matches expected *)
let check p tok = peek p = tok

(** Consume token if it matches, return true; otherwise false *)
let match_token p tok =
  if check p tok then begin
    advance p;
    true
  end else
    false

(** Consume token or raise error *)
let expect p tok msg =
  if check p tok then
    advance p
  else
    expected_error (current p) msg

(** Skip any newline tokens *)
let skip_newlines p =
  while check p NEWLINE do
    advance p
  done

(* ===== Parsing Helpers ===== *)

(** Parse a number literal *)
let parse_number p =
  match peek p with
  | NUMBER f ->
    advance p;
    f
  | _ -> expected_error (current p) "number"

(** Parse an identifier *)
let parse_ident p =
  match peek p with
  | IDENT s ->
    advance p;
    s
  | _ -> expected_error (current p) "identifier"

(** Parse a type annotation *)
let parse_type_annotation p =
  match peek p with
  | NUMBER_TYPE -> advance p; TNumber
  | VEC2_TYPE -> advance p; TVec2
  | SKETCH_TYPE -> advance p; TSketch
  | _ -> expected_error (current p) "type (number, vec2, or sketch)"

(* ===== Expression Parsing ===== *)

(** Parse a numeric expression (simple for now - just literals and variables) *)
let rec parse_num_expr p : num_expr =
  parse_num_additive p

and parse_num_additive p : num_expr =
  let left = parse_num_multiplicative p in
  parse_num_additive_rest p left

and parse_num_additive_rest p left : num_expr =
  match peek p with
  | PLUS ->
    advance p;
    let right = parse_num_multiplicative p in
    parse_num_additive_rest p (NumAdd (left, right))
  | MINUS ->
    advance p;
    let right = parse_num_multiplicative p in
    parse_num_additive_rest p (NumSub (left, right))
  | _ -> left

and parse_num_multiplicative p : num_expr =
  let left = parse_num_atom p in
  parse_num_multiplicative_rest p left

and parse_num_multiplicative_rest p left : num_expr =
  match peek p with
  | STAR ->
    advance p;
    let right = parse_num_atom p in
    parse_num_multiplicative_rest p (NumMul (left, right))
  | SLASH ->
    advance p;
    let right = parse_num_atom p in
    parse_num_multiplicative_rest p (NumDiv (left, right))
  | _ -> left

and parse_num_atom p : num_expr =
  match peek p with
  | NUMBER f ->
    advance p;
    NumLit f
  | IDENT s ->
    advance p;
    NumVar s
  | MINUS ->
    advance p;
    let e = parse_num_atom p in
    NumNeg e
  | LPAREN ->
    advance p;
    let expr = parse_num_expr p in
    expect p RPAREN ")";
    expr
  | _ -> expected_error (current p) "numeric expression"

(** Parse a vector expression with arithmetic *)
and parse_vec_expr p : vec_expr =
  parse_vec_additive p

and parse_vec_additive p : vec_expr =
  let left = parse_vec_multiplicative p in
  parse_vec_additive_rest p left

and parse_vec_additive_rest p left : vec_expr =
  match peek p with
  | PLUS ->
    advance p;
    let right = parse_vec_multiplicative p in
    parse_vec_additive_rest p (VecAdd (left, right))
  | MINUS ->
    advance p;
    let right = parse_vec_multiplicative p in
    parse_vec_additive_rest p (VecSub (left, right))
  | _ -> left

and parse_vec_multiplicative p : vec_expr =
  let left = parse_vec_atom p in
  parse_vec_multiplicative_rest p left

and parse_vec_multiplicative_rest p left : vec_expr =
  match peek p with
  | STAR ->
    advance p;
    let right = parse_num_atom p in
    parse_vec_multiplicative_rest p (VecScale (left, right))
  | _ -> left

and parse_vec_atom p : vec_expr =
  match peek p with
  (* Vector construction: (expr, expr) *)
  | LPAREN ->
    advance p;
    let x = parse_num_expr p in
    expect p COMMA ",";
    let y = parse_num_expr p in
    expect p RPAREN ")";
    (* Optimize: if both are literals, use VecLit *)
    (match (x, y) with
     | (NumLit fx, NumLit fy) -> VecLit (fx, fy)
     | _ -> VecConstruct (x, y))
  
  (* "center of <sketch>" *)
  | CENTER ->
    advance p;
    expect p OF "of";
    let sk = parse_sketch_atom p in
    VecCenter sk
  
  (* Variable reference *)
  | IDENT s ->
    advance p;
    VecVar s
  
  | _ -> expected_error (current p) "vector expression"

(** Parse an axis specification *)
and parse_axis p : axis =
  match peek p with
  | X_AXIS -> 
    advance p;
    (* Check for optional 'at' clause *)
    if check p AT then begin
      advance p;
      let pos = parse_num_expr p in
      XAxisAt pos
    end else
      XAxis
  | Y_AXIS -> 
    advance p;
    (* Check for optional 'at' clause *)
    if check p AT then begin
      advance p;
      let pos = parse_num_expr p in
      YAxisAt pos
    end else
      YAxis
  | _ -> 
    (* Custom axis: from p1 to p2 *)
    let p1 = parse_vec_expr p in
    expect p TO "to";
    let p2 = parse_vec_expr p in
    CustomAxis (p1, p2)

(** Parse a primitive or variable (atomic sketch expression) *)
and parse_sketch_atom p : sketch_expr =
  match peek p with
  (* dot at <vec> *)
  | DOT ->
    advance p;
    if match_token p FROM || check p LPAREN || check p (IDENT "") || check p CENTER then
      let v = parse_vec_expr p in
      Primitive (Dot v)
    else
      (* Just "dot" followed by vec *)
      let v = parse_vec_expr p in
      Primitive (Dot v)
  
  (* hdash at <vec> *)
  | HDASH ->
    advance p;
    let v = parse_vec_expr p in
    Primitive (HDash v)
  
  (* vdash at <vec> *)
  | VDASH ->
    advance p;
    let v = parse_vec_expr p in
    Primitive (VDash v)
  
  (* line from <vec> to <vec> *)
  | LINE ->
    advance p;
    expect p FROM "from";
    let p0 = parse_vec_expr p in
    expect p TO "to";
    let p1 = parse_vec_expr p in
    Primitive (Line (p0, p1))
  
  (* curve from <vec> to <vec> through <vec> [and <vec>]* *)
  | CURVE ->
    advance p;
    expect p FROM "from";
    let p0 = parse_vec_expr p in
    expect p TO "to";
    let p1 = parse_vec_expr p in
    expect p THROUGH "through";
    let through = parse_vec_list p in
    Primitive (Curve (p0, through, p1))
  
  (* arc center <vec> radius <vec> from <num> to <num> *)
  | ARC ->
    advance p;
    expect p CENTER "center";
    let center = parse_vec_expr p in
    (* "radius" is not a keyword, so we look for the vec directly or use ident *)
    let radius = 
      if check p (IDENT "radius") then begin
        advance p;
        parse_vec_expr p
      end else
        parse_vec_expr p
    in
    expect p FROM "from";
    let a0 = parse_num_expr p in
    expect p TO "to";
    let a1 = parse_num_expr p in
    Primitive (Arc (center, radius, a0, a1))
  
  (* Bracketed list of sketches: [sk1, sk2, ...] *)
  | LBRACKET ->
    advance p;
    let sketches = parse_sketch_list p in
    expect p RBRACKET "]";
    Compose sketches
  
  (* Parenthesized sketch expression *)
  | LPAREN ->
    (* Could be a vec literal or a grouped sketch - need to look ahead *)
    (* For now, try vec first, if that fails, it's a grouped sketch *)
    let start_pos = p.pos in
    (try
      let _ = parse_vec_expr p in
      (* If we get here, it was a vec - but that's not a sketch! *)
      p.pos <- start_pos;
      error_at (current p) "Expected sketch expression, got vector"
    with ParseError _ ->
      p.pos <- start_pos;
      advance p; (* consume LPAREN *)
      let sk = parse_sketch_expr p in
      expect p RPAREN ")";
      sk)
  
  (* Variable reference *)
  | IDENT s ->
    advance p;
    SketchVar s
  
  | _ -> expected_error (current p) "sketch expression"

(** Parse a list of vec expressions separated by "and" *)
and parse_vec_list p : vec_expr list =
  let first = parse_vec_expr p in
  let rec go acc =
    if match_token p AND then
      let v = parse_vec_expr p in
      go (v :: acc)
    else
      List.rev acc
  in
  go [first]

(** Parse a comma-separated list of sketch expressions *)
and parse_sketch_list p : sketch_expr list =
  if check p RBRACKET then
    []  (* Empty list *)
  else begin
    let first = parse_sketch_expr p in
    let rec go acc =
      if match_token p COMMA then begin
        skip_newlines p;
        let sk = parse_sketch_expr p in
        go (sk :: acc)
      end else
        List.rev acc
    in
    go [first]
  end

(** Parse a full sketch expression (with transformations) *)
and parse_sketch_expr p : sketch_expr =
  (* First check for transformation keywords that wrap other expressions *)
  match peek p with
  
  (* scale <sketch> by <num> [along <vec>] *)
  | SCALE ->
    advance p;
    let sk = parse_sketch_atom p in
    expect p BY "by";
    let n = parse_num_expr p in
    let along = 
      if match_token p ALONG then Some (parse_vec_expr p)
      else None
    in
    Scale (sk, n, along)
  
  (* rotate <sketch> by <num> *)
  | ROTATE ->
    advance p;
    let sk = parse_sketch_atom p in
    expect p BY "by";
    let n = parse_num_expr p in
    Rotate (sk, n)
  
  (* translate <sketch> by <vec> *)
  | TRANSLATE ->
    advance p;
    let sk = parse_sketch_atom p in
    expect p BY "by";
    let v = parse_vec_expr p in
    Translate (sk, v)
  
  (* repeat <sketch> along <vec> <num> times *)
  | REPEAT ->
    advance p;
    let sk = parse_sketch_atom p in
    expect p ALONG "along";
    let v = parse_vec_expr p in
    let n = parse_num_expr p in
    expect p TIMES "times";
    Repeat (sk, v, n)
  
  (* symmetric <sketch> along <axis> *)
  (* OR: symmetric along <axis> <sketch> *)
  | SYMMETRIC ->
    advance p;
    if match_token p ALONG then begin
      (* symmetric along <axis> <sketch> *)
      let ax = parse_axis p in
      let sk = parse_sketch_atom p in
      Symmetric (sk, ax)
    end else begin
      (* symmetric <sketch> along <axis> *)
      let sk = parse_sketch_atom p in
      expect p ALONG "along";
      let ax = parse_axis p in
      Symmetric (sk, ax)
    end
  
  (* relative to <vec> <sketch> *)
  | RELATIVE ->
    advance p;
    expect p TO "to";
    let v = parse_vec_expr p in
    let sk = parse_sketch_expr p in  (* Allow nested transformations *)
    RelativeTo (v, sk)
  
  (* Otherwise, parse an atom and check for postfix "inside" *)
  | _ ->
    let sk = parse_sketch_atom p in
    (* Check for "inside <sketch>" postfix *)
    if match_token p INSIDE then
      let bounds = parse_sketch_atom p in
      Inside (sk, bounds)
    else
      sk

(* ===== Statement Parsing ===== *)

(** Parse a let binding *)
let parse_let_binding p : statement =
  expect p LET "let";
  let name = parse_ident p in
  expect p COLON ":";
  let typ = parse_type_annotation p in
  expect p EQUALS "=";
  
  match typ with
  | TNumber ->
    let expr = parse_num_expr p in
    LetNum (name, expr)
  | TVec2 ->
    let expr = parse_vec_expr p in
    LetVec (name, expr)
  | TSketch ->
    let expr = parse_sketch_expr p in
    LetSketch (name, expr)

(** Parse a draw command *)
let parse_draw p : statement =
  expect p DRAW "draw";
  let sk = parse_sketch_expr p in
  Draw sk

(** Parse a single statement *)
let parse_statement p : statement =
  skip_newlines p;
  match peek p with
  | LET -> parse_let_binding p
  | DRAW -> parse_draw p
  | _ -> expected_error (current p) "statement (let or draw)"

(** Parse a complete program *)
let parse_program p : program =
  let rec go acc =
    skip_newlines p;
    if is_at_end p then
      List.rev acc
    else
      let stmt = parse_statement p in
      skip_newlines p;
      go (stmt :: acc)
  in
  go []

(* ===== Public Interface ===== *)

(** Parse a string into an AST *)
let parse input : program =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  parse_program p

(** Parse a string and return Result instead of raising *)
let parse_safe input : (program, parse_error) result =
  try Ok (parse input)
  with ParseError e -> Error e

(** Parse a single expression (for testing) *)
let parse_sketch_expr_string input : sketch_expr =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  parse_sketch_expr p

(** Parse a single vec expression (for testing) *)
let parse_vec_expr_string input : vec_expr =
  let tokens = Lexer.tokenize input in
  let p = create tokens in
  parse_vec_expr p

(** Format a parse error for display *)
let format_error (e : parse_error) : string =
  Printf.sprintf "Parse error at line %d, column %d: %s"
    e.position.line e.position.column e.message
