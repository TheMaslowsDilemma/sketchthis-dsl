(* (* gcode.ml - G-code generator for Sketch DSL *)

open Ast

(* Helper function for Option.value (for compatibility) *)
let option_value ~default = function
  | Some x -> x
  | None -> default

(* G-code configuration *)
type config = {
  feed_rate: float;         (* Drawing speed in mm/min *)
  travel_speed: float;      (* Travel speed in mm/min *)
  pen_up_z: float;         (* Z height when pen is up *)
  pen_down_z: float;       (* Z height when pen is down *)
  curve_tolerance: float;  (* Maximum deviation for curve approximation *)
}

let default_config = {
  feed_rate = 1000.0;
  travel_speed = 3000.0;
  pen_up_z = 5.0;
  pen_down_z = 0.0;
  curve_tolerance = 0.1;
}

(* Context for coordinate resolution *)
type context = {
  named_points: (string, float * float) Hashtbl.t;
  current_pos: float * float;
  bounds: bounds option;
}

let create_context ?bounds () = {
  named_points = Hashtbl.create 16;
  current_pos = (0.0, 0.0);
  bounds;
}

(* Resolve coordinate to absolute position *)
let resolve_coord ctx = function
  | Absolute (x, y) -> (x, y)
  | Relative (dx, dy) ->
      let (x0, y0) = ctx.current_pos in
      (x0 +. dx, y0 +. dy)
  | Named name ->
      (match Hashtbl.find_opt ctx.named_points name with
      | Some pos -> pos
      | None -> failwith (Printf.sprintf "Unknown point: %s" name))

(* Transform point *)
let transform_point (x, y) = function
  | Rotate { angle; center } ->
      let (cx, cy) = option_value ~default:(0.0, 0.0) center in
      let theta = angle_to_radians angle in
      let dx = x -. cx in
      let dy = y -. cy in
      let x' = dx *. cos theta -. dy *. sin theta +. cx in
      let y' = dx *. sin theta +. dy *. cos theta +. cy in
      (x', y')
  
  | Scale { x_scale; y_scale; origin } ->
      let (ox, oy) = option_value ~default:(0.0, 0.0) origin in
      let x' = (x -. ox) *. x_scale +. ox in
      let y' = (y -. oy) *. y_scale +. oy in
      (x', y')
  
  | Reflect { axis; position } ->
      let (px, py) = option_value ~default:(0.0, 0.0) position in
      (match axis with
      | X_Axis -> (x, 2.0 *. py -. y)
      | Y_Axis -> (2.0 *. px -. x, y)
      | Custom angle ->
          (* Reflect across line through (px, py) at angle *)
          let dx = x -. px in
          let dy = y -. py in
          let cos2 = cos (2.0 *. angle) in
          let sin2 = sin (2.0 *. angle) in
          let x' = dx *. cos2 +. dy *. sin2 +. px in
          let y' = dx *. sin2 -. dy *. cos2 +. py in
          (x', y'))
  
  | Translate { offset } ->
      let (dx, dy) = match offset with
        | Absolute (x, y) | Relative (x, y) -> (x, y)
        | Named _ -> failwith "Cannot translate by named point"
      in
      (x +. dx, y +. dy)
  
  | Repeat _ ->
      (* Repeat is handled at path level *)
      (x, y)

(* Apply transforms to point *)
let apply_transforms point transforms =
  List.fold_right transform_point transforms point

(* Bezier curve approximation using De Casteljau's algorithm *)
let approximate_bezier start fin controls tolerance =
  let rec subdivide p0 p1 p2 p3 t_start t_end acc =
    (* Calculate midpoint using De Casteljau *)
    let (x0, y0) = p0 and (x1, y1) = p1 
    and (x2, y2) = p2 and (x3, y3) = p3 in
    
    (* Linear approximation *)
    let (lx, ly) = 
      let t = (t_start +. t_end) /. 2.0 in
      (x0 +. t *. (x3 -. x0), y0 +. t *. (y3 -. y0)) 
    in
    
    (* Actual curve point *)
    let t = (t_start +. t_end) /. 2.0 in
    let u = 1.0 -. t in
    let cx = u*.u*.u*.x0 +. 3.0*.u*.u*.t*.x1 +. 
             3.0*.u*.t*.t*.x2 +. t*.t*.t*.x3 in
    let cy = u*.u*.u*.y0 +. 3.0*.u*.u*.t*.y1 +. 
             3.0*.u*.t*.t*.y2 +. t*.t*.t*.y3 in
    
    (* Check if approximation is good enough *)
    let dist = sqrt ((cx -. lx) ** 2.0 +. (cy -. ly) ** 2.0) in
    
    if dist < tolerance || (t_end -. t_start) < 0.001 then
      p3 :: acc
    else
      (* Subdivide further *)
      let mid01 = ((x0+.x1)/.2.0, (y0+.y1)/.2.0) in
      let mid12 = ((x1+.x2)/.2.0, (y1+.y2)/.2.0) in
      let mid23 = ((x2+.x3)/.2.0, (y2+.y3)/.2.0) in
      let mid012 = ((fst mid01+.fst mid12)/.2.0, 
                    (snd mid01+.snd mid12)/.2.0) in
      let mid123 = ((fst mid12+.fst mid23)/.2.0, 
                    (snd mid12+.snd mid23)/.2.0) in
      let mid = ((fst mid012+.fst mid123)/.2.0, 
                 (snd mid012+.snd mid123)/.2.0) in
      
      let acc = subdivide mid mid123 mid23 p3 
                         ((t_start+.t_end)/.2.0) t_end acc in
      subdivide p0 mid01 mid012 mid t_start ((t_start+.t_end)/.2.0) acc
  in
  
  match controls with
  | [c1; c2] ->
      (* Cubic bezier *)
      let points = subdivide start c1 c2 fin 0.0 1.0 [] in
      List.rev points
  | [c1] ->
      (* Quadratic bezier - convert to cubic *)
      let (x0, y0) = start and (x1, y1) = c1 and (x2, y2) = fin in
      let c1' = (x0 +. 2.0*.x1)/.3.0, (y0 +. 2.0*.y1)/.3.0 in
      let c2' = (x2 +. 2.0*.x1)/.3.0, (y2 +. 2.0*.y1)/.3.0 in
      approximate_bezier start fin [c1'; c2'] tolerance
  | _ ->
      (* More than 2 control points - use first and last as cubic bezier *)
      (match controls with
      | c1 :: rest ->
          let c2 = List.hd (List.rev rest) in
          approximate_bezier start fin [c1; c2] tolerance
      | [] -> [fin])

(* Generate G-code for movement *)
let move_to config (x, y) pen_down =
  let z = if pen_down then config.pen_down_z else config.pen_up_z in
  let speed = if pen_down then config.feed_rate else config.travel_speed in
  Printf.sprintf "G1 X%.3f Y%.3f Z%.3f F%.1f\n" x y z speed

(* Generate G-code for path element *)
let generate_element config ctx element =
  let lines = ref [] in
  
  let add_line line = lines := line :: !lines in
  
  match element with
  | Point { pos; _ } ->
      let (x, y) = resolve_coord ctx pos in
      ctx.current_pos <- (x, y);
      add_line (move_to config (x, y) false);
      add_line (move_to config (x, y) true);
      add_line (move_to config (x, y) false);
  
  | Line { start; fin } ->
      let (x1, y1) = resolve_coord ctx start in
      let (x2, y2) = resolve_coord ctx fin in
      ctx.current_pos <- (x2, y2);
      add_line (move_to config (x1, y1) false);
      add_line (move_to config (x2, y2) true);
  
  | Curve { start; fin; control_points } ->
      let start_pos = resolve_coord ctx start in
      let finpos = resolve_coord ctx fin in
      let control_pos = List.map (resolve_coord ctx) control_points in
      
      (* Approximate curve with line segments *)
      let points = approximate_bezier start_pos finpos control_pos 
                                      config.curve_tolerance in
      
      add_line (move_to config start_pos false);
      List.iter (fun pt -> add_line (move_to config pt true)) points;
      ctx.current_pos <- finpos;
  
  | Arc { start; fin; radius } ->
      (* Approximate arc with curve *)
      let start_pos = resolve_coord ctx start in
      let finpos = resolve_coord ctx fin in
      
      (* Simple arc approximation - use bezier curve *)
      let (x1, y1) = start_pos and (x2, y2) = finpos in
      let dx = x2 -. x1 and dy = y2 -. y1 in
      let dist = sqrt (dx *. dx +. dy *. dy) in
      let angle = atan2 dy dx in
      
      (* Control point for arc approximation *)
      let cx = (x1 +. x2) /. 2.0 +. radius *. sin angle in
      let cy = (y1 +. y2) /. 2.0 -. radius *. cos angle in
      
      let points = approximate_bezier start_pos finpos [(cx, cy)] 
                                      config.curve_tolerance in
      
      add_line (move_to config start_pos false);
      List.iter (fun pt -> add_line (move_to config pt true)) points;
      ctx.current_pos <- finpos;
  
  | Circle { center; radius } ->
      let (cx, cy) = resolve_coord ctx center in
      
      (* Approximate circle with 4 bezier curves *)
      let k = 0.5522847498 in  (* Magic number for circle approximation *)
      let r = radius in
      
      let p0 = (cx +. r, cy) in
      let p1 = (cx, cy +. r) in
      let p2 = (cx -. r, cy) in
      let p3 = (cx, cy -. r) in
      
      let segments = [
        (p0, p1, [(cx +. r, cy +. k*.r); (cx +. k*.r, cy +. r)]);
        (p1, p2, [(cx -. k*.r, cy +. r); (cx -. r, cy +. k*.r)]);
        (p2, p3, [(cx -. r, cy -. k*.r); (cx -. k*.r, cy -. r)]);
        (p3, p0, [(cx +. k*.r, cy -. r); (cx +. r, cy -. k*.r)]);
      ] in
      
      add_line (move_to config p0 false);
      List.iter (fun (start, fin, ctrl) ->
        let points = approximate_bezier start fin ctrl config.curve_tolerance in
        List.iter (fun pt -> add_line (move_to config pt true)) points
      ) segments;
      ctx.current_pos <- p0;
  
  | Rectangle { corner1; corner2 } ->
      let (x1, y1) = resolve_coord ctx corner1 in
      let (x2, y2) = resolve_coord ctx corner2 in
      
      add_line (move_to config (x1, y1) false);
      add_line (move_to config (x2, y1) true);
      add_line (move_to config (x2, y2) true);
      add_line (move_to config (x1, y2) true);
      add_line (move_to config (x1, y1) true);
      ctx.current_pos <- (x1, y1);
  
  | Polygon { vertices } ->
      let points = List.map (resolve_coord ctx) vertices in
      (match points with
      | first :: rest ->
          add_line (move_to config first false);
          List.iter (fun pt -> add_line (move_to config pt true)) rest;
          add_line (move_to config first true);
          ctx.current_pos <- first
      | [] -> ());
  
  String.concat "" (List.rev !lines)

(* Generate G-code for complete program *)
let generate ?(config=default_config) program =
  let ctx = create_context ?bounds:program.bounds () in
  let output = Buffer.create 1024 in
  
  (* G-code header *)
  Buffer.add_string output "G21 ; Set units to millimeters\n";
  Buffer.add_string output "G90 ; Absolute positioning\n";
  Buffer.add_string output (Printf.sprintf "G0 Z%.3f ; Pen up\n" config.pen_up_z);
  Buffer.add_string output "G28 ; Home\n\n";
  
  (* Process each statement *)
  List.iter (fun stmt ->
    match stmt with
    | PathDef path ->
        (* Generate code for each element *)
        List.iter (fun element ->
          let code = generate_element config ctx element in
          Buffer.add_string output code
        ) path.elements;
        Buffer.add_string output "\n"
    
    | Assignment { name; value } ->
        (match value with
        | CoordValue coord ->
            let pos = resolve_coord ctx coord in
            Hashtbl.replace ctx.named_points name pos
        | _ -> ())
    
    | BoundsDecl _ | Comment _ -> ()
  ) program.statements;
  
  (* G-code footer *)
  Buffer.add_string output (Printf.sprintf "\nG0 Z%.3f ; Pen up\n" config.pen_up_z);
  Buffer.add_string output "G28 ; Home\n";
  Buffer.add_string output "M2 ; Program end\n";
  
  Buffer.contents output
 *)