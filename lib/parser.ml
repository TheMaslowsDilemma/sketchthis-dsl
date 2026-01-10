(* (* parser.ml - Recursive descent parser for Sketch DSL *)

open Ast
open Lexer

(* Helper function for Option.value (for compatibility) *)
let option_value ~default = function
  | Some x -> x
  | None -> default

type parser_state = {
  tokens: token list;
  mutable pos: int;
}

exception ParseError of string

let create_parser tokens = {
  tokens;
  pos = 0;
}

let current state =
  if state.pos >= List.length state.tokens then EOF
  else List.nth state.tokens state.pos

let peek state =
  current state

let advance state =
  if state.pos < List.length state.tokens then
    state.pos <- state.pos + 1

let expect state expected =
  let tok = current state in
  if tok = expected then
    advance state
  else
    raise (ParseError (Printf.sprintf "Expected %s but got %s"
      (string_of_token expected) (string_of_token tok)))

let expect_ident state =
  match current state with
  | IDENT s ->
      advance state;
      s
  | tok ->
      raise (ParseError (Printf.sprintf "Expected identifier but got %s"
        (string_of_token tok)))

let expect_number state =
  match current state with
  | NUMBER n ->
      advance state;
      n
  | tok ->
      raise (ParseError (Printf.sprintf "Expected number but got %s"
        (string_of_token tok)))

(* Parse coordinate: (x, y) or [x, y] or identifier *)
let parse_coord state =
  match current state with
  | LPAREN ->
      advance state;
      let x = expect_number state in
      expect state COMMA;
      let y = expect_number state in
      expect state RPAREN;
      Absolute (x, y)
  
  | LBRACKET ->
      advance state;
      let x = expect_number state in
      expect state COMMA;
      let y = expect_number state in
      expect state RBRACKET;
      Relative (x, y)
  
  | IDENT name ->
      advance state;
      Named name
  
  | tok ->
      raise (ParseError (Printf.sprintf "Expected coordinate but got %s"
        (string_of_token tok)))

(* Parse angle: number followed by 'deg' or 'rad' *)
let parse_angle state =
  let value = expect_number state in
  match current state with
  | DEG ->
      advance state;
      Degrees value
  | RAD ->
      advance state;
      Radians value
  | _ ->
      (* Default to degrees if no unit specified *)
      Degrees value

(* Parse optional clause *)
let parse_optional state keyword parser =
  if current state = keyword then begin
    advance state;
    Some (parser state)
  end else
    None

(* Parse list with separator *)
let rec parse_list state separator parser =
  let first = parser state in
  if current state = separator then begin
    advance state;
    first :: parse_list state separator parser
  end else
    [first]

(* Parse axis: x, y, or angle *)
let parse_axis state =
  match current state with
  | X ->
      advance state;
      X_Axis
  | Y ->
      advance state;
      Y_Axis
  | NUMBER _ ->
      let angle = parse_angle state in
      Custom (angle_to_radians angle)
  | tok ->
      raise (ParseError (Printf.sprintf "Expected axis but got %s"
        (string_of_token tok)))

(* Parse path element *)
let parse_path_element state =
  match current state with
  | POINT ->
      advance state;
      expect state AT;
      let pos = parse_coord state in
      Point (make_point pos)
  
  | LINE ->
      advance state;
      expect state FROM;
      let start = parse_coord state in
      expect state TO;
      let fin = parse_coord state in
      make_line start fin
  
  | CURVE ->
      advance state;
      expect state FROM;
      let start = parse_coord state in
      expect state TO;
      let fin = parse_coord state in
      expect state THROUGH;
      let control_points = parse_list state AND parse_coord in
      make_curve start fin control_points
  
  | ARC ->
      advance state;
      expect state FROM;
      let start = parse_coord state in
      expect state TO;
      let fin = parse_coord state in
      expect state WITH;
      expect state (IDENT "radius");  (* "with radius" *)
      let radius = expect_number state in
      Arc { start; fin; radius }
  
  | CIRCLE ->
      advance state;
      expect state AT;
      let center = parse_coord state in
      expect state WITH;
      expect state (IDENT "radius");
      let radius = expect_number state in
      make_circle center radius
  
  | RECTANGLE ->
      advance state;
      expect state FROM;
      let corner1 = parse_coord state in
      expect state TO;
      let corner2 = parse_coord state in
      Rectangle { corner1; corner2 }
  
  | POLYGON ->
      advance state;
      expect state AT;
      let vertices = parse_list state AND parse_coord in
      Polygon { vertices }
  
  | tok ->
      raise (ParseError (Printf.sprintf "Expected path element but got %s"
        (string_of_token tok)))

(* Parse transformation *)
let rec parse_transform state =
  match current state with
  | ROTATE ->
      advance state;
      expect state BY;
      let angle = parse_angle state in
      let center = parse_optional state AROUND parse_coord in
      Rotate { angle; center }
  
  | SCALE ->
      advance state;
      expect state BY;
      let x_scale = expect_number state in
      let y_scale = 
        if current state = COMMA then begin
          advance state;
          expect_number state
        end else
          x_scale
      in
      let origin = parse_optional state FROM parse_coord in
      Scale { x_scale; y_scale; origin }
  
  | REFLECT ->
      advance state;
      let axis = parse_optional state ACROSS parse_axis in
      let axis = option_value ~default:Y_Axis axis in
      let position = parse_optional state AT parse_coord in
      Reflect { axis; position }
  
  | TRANSLATE ->
      advance state;
      expect state BY;
      let offset = parse_coord state in
      Translate { offset }
  
  | REPEAT ->
      advance state;
      let count = int_of_float (expect_number state) in
      expect state TIMES;
      let transform = parse_optional state WITH parse_transform in
      Repeat { count; transform }
  
  | RELATIVE ->
      advance state;
      expect state TO;
      let offset = parse_coord state in
      Translate { offset }
  
  | SYMMETRIC ->
      advance state;
      expect state ACROSS;
      let axis = parse_axis state in
      expect state AT;
      let position = parse_coord state in
      (* Symmetric is implemented as reflect transform *)
      Reflect { axis; position = Some position }
  
  | tok ->
      raise (ParseError (Printf.sprintf "Expected transformation but got %s"
        (string_of_token tok)))

(* Parse modifiers (transformations applied to path element) *)
let rec parse_modifiers state acc =
  match current state with
  | ROTATE | SCALE | REFLECT | TRANSLATE | REPEAT 
  | RELATIVE | SYMMETRIC ->
      let transform = parse_transform state in
      parse_modifiers state (transform :: acc)
  | _ ->
      List.rev acc

(* Parse path definition *)
let parse_path_def state =
  let elements = ref [] in
  
  (* Parse first element *)
  elements := [parse_path_element state];
  
  (* Parse continuation with 'then' *)
  while current state = THEN do
    advance state;
    elements := parse_path_element state :: !elements
  done;
  
  let elements = List.rev !elements in
  
  (* Parse modifiers/transformations *)
  let transforms = parse_modifiers state [] in
  
  make_path ~transforms elements

(* Parse assignment *)
let parse_assignment state =
  let name = expect_ident state in
  expect state EQUALS;
  
  match current state with
  | LPAREN | LBRACKET | IDENT _ ->
      let coord = parse_coord state in
      Assignment { name; value = CoordValue coord }
  
  | NUMBER n ->
      advance state;
      Assignment { name; value = NumberValue n }
  
  | _ ->
      let path = parse_path_def state in
      Assignment { name; value = PathValue path }

(* Parse bounds declaration *)
let parse_bounds_decl state =
  expect state BOUNDED;
  expect state WITHIN;
  
  match parse_coord state with
  | Absolute (min_x, min_y) ->
      expect state TO;
      (match parse_coord state with
      | Absolute (max_x, max_y) ->
          BoundsDecl { min_x; max_x; min_y; max_y }
      | _ ->
          raise (ParseError "Bounds must use absolute coordinates"))
  | _ ->
      raise (ParseError "Bounds must use absolute coordinates")

(* Parse statement *)
let parse_statement state =
  match current state with
  | BOUNDED ->
      parse_bounds_decl state
  
  | IDENT _ ->
      (* Could be assignment or path with named points *)
      let saved_pos = state.pos in
      (try
        parse_assignment state
      with ParseError _ ->
        state.pos <- saved_pos;
        PathDef (parse_path_def state))
  
  | _ ->
      PathDef (parse_path_def state)

(* Parse entire program *)
let parse_program state =
  let statements = ref [] in
  let bounds = ref None in
  
  while current state <> EOF do
    try
      let stmt = parse_statement state in
      statements := stmt :: !statements;
      
      (* Extract bounds if present *)
      (match stmt with
      | BoundsDecl b -> bounds := Some b
      | _ -> ())
    with ParseError msg ->
      Printf.eprintf "Parse error: %s\n" msg;
      (* Skip to next statement (naive error recovery) *)
      advance state
  done;
  
  make_program ?bounds:!bounds (List.rev !statements)

(* Main parse function *)
let parse tokens =
  let state = create_parser tokens in
  parse_program state
 *)