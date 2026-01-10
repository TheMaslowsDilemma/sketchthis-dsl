(* ast.ml - Abstract Syntax Tree definitions for Sketch DSL *)

(* Coordinate types *)
type coord =
  | Absolute of float * float
  | Relative of float * float
  | Named of string

(* Angle representation *)
type angle =
  | Degrees of float
  | Radians of float

let angle_to_radians = function
  | Degrees d -> d *. Float.pi /. 180.0
  | Radians r -> r

let angle_to_degrees = function
  | Degrees d -> d
  | Radians r -> r *. 180.0 /. Float.pi

(* Point with optional name *)
type point = {
  pos: coord;
  name: string option;
}

(* Axis for reflection *)
type axis =
  | X_Axis
  | Y_Axis
  | Custom of float  (* angle in radians *)

(* Path elements - the building blocks of drawings *)
type path_element =
  | Point of point
  | Line of { start: coord; fin: coord }
  | Curve of { start: coord; fin: coord; through: coord list }
  | Arc of { start: coord; fin: coord; radius: float }
  | Circle of { center: coord; radius: float }
  | Rectangle of { corner1: coord; corner2: coord }
  | Polygon of { vertices: coord list }

(* Transformation operations *)
type transform =
  | Rotate of { angle: angle; center: coord option }
  | Scale of { x_scale: float; y_scale: float; origin: coord option }
  | Reflect of { axis: axis; position: coord option }
  | Translate of { offset: coord }
  | Repeat of { count: int; transform: transform option }

(* Bounds specification *)
type bounds = {
  min_x: float;
  max_x: float;
  min_y: float;
  max_y: float;
}

(* A complete path with metadata *)
type path = {
  elements: path_element list;
  transforms: transform list;
  closed: bool;
  name: string option;
}

(* Top-level statements *)
type statement =
  | PathDef of path
  | Assignment of { name: string; value: assignable }
  | BoundsDecl of bounds
  | Comment of string

and assignable =
  | CoordValue of coord
  | PathValue of path
  | NumberValue of float

(* Complete program *)
type program = {
  statements: statement list;
  bounds: bounds option;
}

(* Helper constructors *)
let make_point ?(name=None) pos = { pos; name }

let make_line start fin = Line { start; fin }

let make_curve start fin control_points = 
  Curve { start; fin; control_points }

let make_circle center radius = Circle { center; radius }

let make_path ?(transforms=[]) ?(closed=false) ?(name=None) elements =
  { elements; transforms; closed; name }

let make_program ?(bounds=None) statements =
  { statements; bounds }

(* String representation for debugging *)
let string_of_coord = function
  | Absolute (x, y) -> Printf.sprintf "(%g, %g)" x y
  | Relative (x, y) -> Printf.sprintf "[%g, %g]" x y
  | Named s -> s

let string_of_angle = function
  | Degrees d -> Printf.sprintf "%gdeg" d
  | Radians r -> Printf.sprintf "%grad" r

let string_of_path_element = function
  | Point p -> Printf.sprintf "point at %s" (string_of_coord p.pos)
  | Line { start; fin } -> 
      Printf.sprintf "line from %s to %s" 
        (string_of_coord start) (string_of_coord fin)
  | Curve { start; fin; control_points } ->
      let controls = String.concat " and " 
        (List.map string_of_coord control_points) in
      Printf.sprintf "curve from %s to %s through %s"
        (string_of_coord start) (string_of_coord fin) controls
  | Arc { start; fin; radius } ->
      Printf.sprintf "arc from %s to %s with radius %g"
        (string_of_coord start) (string_of_coord fin) radius
  | Circle { center; radius } ->
      Printf.sprintf "circle at %s with radius %g"
        (string_of_coord center) radius
  | Rectangle { corner1; corner2 } ->
      Printf.sprintf "rectangle from %s to %s"
        (string_of_coord corner1) (string_of_coord corner2)
  | Polygon { vertices } ->
      let verts = String.concat " and " (List.map string_of_coord vertices) in
      Printf.sprintf "polygon at %s" verts

let string_of_axis = function
  | X_Axis -> "x"
  | Y_Axis -> "y"
  | Custom angle -> Printf.sprintf "%grad" angle

let rec string_of_transform = function
  | Rotate { angle; center } ->
      let center_str = match center with
        | None -> ""
        | Some c -> " around " ^ string_of_coord c
      in
      Printf.sprintf "rotate by %s%s" (string_of_angle angle) center_str
  | Scale { x_scale; y_scale; origin } ->
      let origin_str = match origin with
        | None -> ""
        | Some o -> " from " ^ string_of_coord o
      in
      Printf.sprintf "scale by %g, %g%s" x_scale y_scale origin_str
  | Reflect { axis; position } ->
      let pos_str = match position with
        | None -> ""
        | Some p -> " at " ^ string_of_coord p
      in
      Printf.sprintf "reflect across %s%s" (string_of_axis axis) pos_str
  | Translate { offset } ->
      Printf.sprintf "translate by %s" (string_of_coord offset)
  | Repeat { count; transform } ->
      let trans_str = match transform with
        | None -> ""
        | Some t -> " with " ^ string_of_transform t
      in
      Printf.sprintf "repeat %d times%s" count trans_str

let string_of_path path =
  let elements_str = String.concat " then " 
    (List.map string_of_path_element path.elements) in
  let transforms_str = String.concat " " 
    (List.map string_of_transform path.transforms) in
  let name_str = match path.name with
    | None -> ""
    | Some n -> n ^ " = "
  in
  Printf.sprintf "%s%s %s" name_str elements_str transforms_str
