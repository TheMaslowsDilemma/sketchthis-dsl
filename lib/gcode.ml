(* Sketch DSL G-code Generator *)
(* Converts IR to optimized G-code for pen plotters *)

open Compiler

(* ===== G-code Configuration ===== *)

type gcode_config = {
  (* Movement speeds *)
  travel_speed: float;      (* Speed when pen is up (mm/min) *)
  draw_speed: float;        (* Speed when pen is down (mm/min) *)
  
  (* Pen control *)
  pen_up_command: string;   (* G-code to lift pen *)
  pen_down_command: string; (* G-code to lower pen *)
  
  (* Coordinate settings *)
  scale: float;             (* Scale factor for coordinates *)
  x_offset: float;          (* X origin offset *)
  y_offset: float;          (* Y origin offset *)
  
  (* Bezier approximation *)
  bezier_segments: int;     (* Number of line segments per bezier curve *)
  arc_segments: int;        (* Number of line segments per arc *)
  
  (* Output options *)
  decimal_places: int;      (* Decimal precision for coordinates *)
  include_comments: bool;   (* Include comments in output *)
}

let default_config = {
  travel_speed = 3000.0;
  draw_speed = 1000.0;
  pen_up_command = "M3 S0";    (* Servo up / laser off *)
  pen_down_command = "M3 S50"; (* Servo down / laser low power *)
  scale = 1.0;
  x_offset = 0.0;
  y_offset = 0.0;
  bezier_segments = 20;
  arc_segments = 32;
  decimal_places = 3;
  include_comments = true;
}

(* Uunatek-specific config based on $$ output:
   $130=420.000  - X max travel (mm) - A3 width
   $131=297.000  - Y max travel (mm) - A3 height  
   $132=12.000   - Z max travel (mm)
   $110=11000.000 - X max rate (mm/min)
   $111=11000.000 - Y max rate (mm/min)
   $112=5000.000  - Z max rate (mm/min)
   $120=500.000  - X acceleration (mm/sec^2)
   $121=500.000  - Y acceleration (mm/sec^2)
   $122=500.000  - Z acceleration (mm/sec^2)
   $100=80.000   - X steps/mm
   $101=80.000   - Y steps/mm
   $102=85.000   - Z steps/mm
   $30=0         - Max spindle speed (0 = pen mode)
*)
let uunatek_config = {
  travel_speed = 11000.0;    (* Max XY rate from $110/$111 *)
  draw_speed = 3000.0;       (* Comfortable drawing speed *)
  pen_up_command = "M5";     (* Pen/servo up *)
  pen_down_command = "M3 S1000"; (* Pen/servo down *)
  scale = 1.0;
  x_offset = 0.0;
  y_offset = 0.0;
  bezier_segments = 25;
  arc_segments = 36;
  decimal_places = 3;
  include_comments = true;
}

(* Uunatek work area limits *)
let uunatek_bounds = {
  min_x = 0.0;
  max_x = 420.0;   (* A3 width *)
  min_y = 0.0;
  max_y = 297.0;   (* A3 height *)
}

(* ===== Path Optimization ===== *)

(** Distance between two points *)
let distance p1 p2 =
  let dx = p2.x -. p1.x in
  let dy = p2.y -. p1.y in
  Float.sqrt (dx *. dx +. dy *. dy)

(** Get the endpoint of a path *)
let path_endpoint (path : path) : point =
  match List.rev path.segments with
  | [] -> path.start
  | seg :: _ ->
    match seg with
    | MoveTo p | LineTo p -> p
    | BezierTo (_, _, p) | QuadraticTo (_, p) -> p
    | ArcTo (center, radius, _, a1) ->
      let angle_rad = a1 *. Float.pi /. 180.0 in
      { x = center.x +. radius.x *. Float.cos angle_rad;
        y = center.y +. radius.y *. Float.sin angle_rad }

(** Reverse a path (draw it backwards) *)
let reverse_path (path : path) : path =
  let endpoint = path_endpoint path in
  let rec reverse_segments prev_end acc = function
    | [] -> acc
    | seg :: rest ->
      let new_seg, new_end = match seg with
        | MoveTo p -> (MoveTo prev_end, p)
        | LineTo p -> (LineTo prev_end, p)
        | QuadraticTo (c, p) -> (QuadraticTo (c, prev_end), p)
        | BezierTo (c1, c2, p) -> (BezierTo (c2, c1, prev_end), p)
        | ArcTo (center, radius, a0, a1) -> 
          (ArcTo (center, radius, a1, a0), prev_end) (* Swap angles *)
      in
      reverse_segments new_end (new_seg :: acc) rest
  in
  let reversed_segs = reverse_segments path.start [] (List.rev path.segments) in
  { start = endpoint; segments = reversed_segs }

(** Calculate total travel distance for a path order *)
let total_travel_distance paths current_pos =
  let rec go pos total = function
    | [] -> total
    | path :: rest ->
      let dist_to_start = distance pos path.start in
      let endpoint = path_endpoint path in
      go endpoint (total +. dist_to_start) rest
  in
  go current_pos 0.0 paths

(** Greedy nearest-neighbor path optimization *)
let optimize_paths_greedy (paths : path list) : path list =
  if List.length paths <= 1 then paths
  else
    let rec go current_pos remaining acc =
      match remaining with
      | [] -> List.rev acc
      | _ ->
        (* Find nearest path (considering both directions) *)
        let find_nearest () =
          let best = ref None in
          let best_dist = ref Float.infinity in
          List.iter (fun path ->
            (* Distance to start *)
            let d_start = distance current_pos path.start in
            if d_start < !best_dist then begin
              best := Some (path, false);
              best_dist := d_start
            end;
            (* Distance to end (reversed) *)
            let d_end = distance current_pos (path_endpoint path) in
            if d_end < !best_dist then begin
              best := Some (path, true);
              best_dist := d_end
            end
          ) remaining;
          !best
        in
        match find_nearest () with
        | None -> List.rev acc
        | Some (nearest, reversed) ->
          let path_to_add = if reversed then reverse_path nearest else nearest in
          let new_pos = path_endpoint path_to_add in
          let new_remaining = List.filter (fun p -> p != nearest) remaining in
          go new_pos new_remaining (path_to_add :: acc)
    in
    go (point 0.0 0.0) paths []

(** 2-opt local search improvement *)
let optimize_2opt (paths : path list) : path list =
  if List.length paths <= 3 then paths
  else
    let arr = Array.of_list paths in
    let n = Array.length arr in
    let improved = ref true in
    
    while !improved do
      improved := false;
      for i = 0 to n - 2 do
        for j = i + 2 to n - 1 do
          (* Calculate current distance *)
          let p_i = arr.(i) in
          let p_i1 = arr.(i + 1) in
          let p_j = arr.(j) in
          let p_j1 = if j + 1 < n then Some arr.(j + 1) else None in
          
          let end_i = path_endpoint p_i in
          let end_j = path_endpoint p_j in
          
          let current_dist = 
            distance end_i p_i1.start +.
            (match p_j1 with Some p -> distance end_j p.start | None -> 0.0)
          in
          
          (* Calculate distance after 2-opt swap *)
          let new_dist =
            distance end_i p_j.start +.
            (match p_j1 with Some p -> distance (path_endpoint p_i1) p.start | None -> 0.0)
          in
          
          if new_dist < current_dist -. 0.001 then begin
            (* Reverse the segment between i+1 and j *)
            let rec reverse_range lo hi =
              if lo < hi then begin
                let tmp = arr.(lo) in
                arr.(lo) <- reverse_path arr.(hi);
                arr.(hi) <- reverse_path tmp;
                reverse_range (lo + 1) (hi - 1)
              end else if lo = hi then
                arr.(lo) <- reverse_path arr.(lo)
            in
            reverse_range (i + 1) j;
            improved := true
          end
        done
      done
    done;
    
    Array.to_list arr

(** Full optimization pipeline *)
let optimize_paths (paths : path list) : path list =
  paths
  |> optimize_paths_greedy
  |> optimize_2opt

(* ===== Bezier Curve Flattening ===== *)

(** Evaluate quadratic bezier at parameter t *)
let eval_quadratic p0 p1 p2 t =
  let t2 = t *. t in
  let mt = 1.0 -. t in
  let mt2 = mt *. mt in
  {
    x = mt2 *. p0.x +. 2.0 *. mt *. t *. p1.x +. t2 *. p2.x;
    y = mt2 *. p0.y +. 2.0 *. mt *. t *. p1.y +. t2 *. p2.y;
  }

(** Evaluate cubic bezier at parameter t *)
let eval_cubic p0 p1 p2 p3 t =
  let t2 = t *. t in
  let t3 = t2 *. t in
  let mt = 1.0 -. t in
  let mt2 = mt *. mt in
  let mt3 = mt2 *. mt in
  {
    x = mt3 *. p0.x +. 3.0 *. mt2 *. t *. p1.x +. 3.0 *. mt *. t2 *. p2.x +. t3 *. p3.x;
    y = mt3 *. p0.y +. 3.0 *. mt2 *. t *. p1.y +. 3.0 *. mt *. t2 *. p2.y +. t3 *. p3.y;
  }

(** Flatten a quadratic bezier to line segments *)
let flatten_quadratic config p0 ctrl p_end =
  let n = config.bezier_segments in
  let points = Array.init (n + 1) (fun i ->
    let t = float_of_int i /. float_of_int n in
    eval_quadratic p0 ctrl p_end t
  ) in
  (* Skip first point (it's the start), return LineTo for rest *)
  Array.to_list (Array.sub points 1 n)
  |> List.map (fun p -> LineTo p)

(** Flatten a cubic bezier to line segments *)
let flatten_cubic config p0 c1 c2 p_end =
  let n = config.bezier_segments in
  let points = Array.init (n + 1) (fun i ->
    let t = float_of_int i /. float_of_int n in
    eval_cubic p0 c1 c2 p_end t
  ) in
  Array.to_list (Array.sub points 1 n)
  |> List.map (fun p -> LineTo p)

(** Flatten an arc to line segments *)
let flatten_arc config center radius a0 a1 =
  let n = config.arc_segments in
  let angle_span = a1 -. a0 in
  let points = Array.init (n + 1) (fun i ->
    let t = float_of_int i /. float_of_int n in
    let angle = (a0 +. t *. angle_span) *. Float.pi /. 180.0 in
    {
      x = center.x +. radius.x *. Float.cos angle;
      y = center.y +. radius.y *. Float.sin angle;
    }
  ) in
  Array.to_list (Array.sub points 1 n)
  |> List.map (fun p -> LineTo p)

(** Flatten all curves in a path to line segments *)
let flatten_path config (path : path) : path =
  let rec flatten_segments current_pos acc = function
    | [] -> List.rev acc
    | seg :: rest ->
      match seg with
      | MoveTo p -> 
        flatten_segments p (MoveTo p :: acc) rest
      | LineTo p -> 
        flatten_segments p (LineTo p :: acc) rest
      | QuadraticTo (ctrl, p_end) ->
        let lines = flatten_quadratic config current_pos ctrl p_end in
        flatten_segments p_end (List.rev_append lines acc) rest
      | BezierTo (c1, c2, p_end) ->
        let lines = flatten_cubic config current_pos c1 c2 p_end in
        flatten_segments p_end (List.rev_append lines acc) rest
      | ArcTo (center, radius, a0, a1) ->
        let lines = flatten_arc config center radius a0 a1 in
        let p_end = 
          let angle = a1 *. Float.pi /. 180.0 in
          { x = center.x +. radius.x *. Float.cos angle;
            y = center.y +. radius.y *. Float.sin angle }
        in
        flatten_segments p_end (List.rev_append lines acc) rest
  in
  { start = path.start; 
    segments = flatten_segments path.start [] path.segments }

(* ===== G-code Generation ===== *)

type gcode_state = {
  mutable current_pos: point;
  mutable pen_down: bool;
  mutable lines: string list;
}

let create_state () = {
  current_pos = point 0.0 0.0;
  pen_down = false;
  lines = [];
}

let emit state line =
  state.lines <- line :: state.lines

let format_coord config v =
  Printf.sprintf "%.*f" config.decimal_places v

let format_point config p =
  let x = p.x *. config.scale +. config.x_offset in
  let y = p.y *. config.scale +. config.y_offset in
  Printf.sprintf "X%s Y%s" (format_coord config x) (format_coord config y)

(** Generate G-code preamble *)
let emit_preamble config state =
  if config.include_comments then
    emit state "; Generated by Sketch DSL";
  emit state "G21 ; mm units";
  emit state "G90 ; absolute positioning";
  emit state (Printf.sprintf "G0 F%.0f ; travel speed" config.travel_speed);
  emit state config.pen_up_command;
  state.pen_down <- false

(** Generate G-code postamble *)
let emit_postamble config state =
  if state.pen_down then begin
    emit state config.pen_up_command;
    state.pen_down <- false
  end;
  emit state "G0 X0 Y0 ; return to origin";
  emit state "M5 ; ensure pen up";
  if config.include_comments then
    emit state "; End of program"

(** Generate Uunatek-specific preamble with homing *)
let emit_uunatek_preamble config state =
  if config.include_comments then begin
    emit state "; Generated by Sketch DSL for Uunatek Plotter";
    emit state "; Work area: 420mm x 297mm (A3)"
  end;
  emit state "G21 ; mm units";
  emit state "G90 ; absolute positioning";
  emit state "M5 ; pen up";
  emit state "$H ; home machine";
  emit state "G4 P0.5 ; wait for homing";
  emit state (Printf.sprintf "G0 F%.0f ; set travel speed" config.travel_speed);
  state.pen_down <- false

(** Generate Uunatek-specific postamble *)
let emit_uunatek_postamble config state =
  if state.pen_down then begin
    emit state "M5 ; pen up";
    state.pen_down <- false
  end;
  emit state "G4 P0.2 ; pause";
  emit state "G0 X0 Y0 ; return to origin";
  emit state "M5 ; ensure pen up";
  if config.include_comments then
    emit state "; End of program"

(** Check if IR fits within Uunatek bounds *)
let check_uunatek_bounds (ir : ir) : bool =
  if ir = [] then true
  else
    let b = Compiler.compute_bounds ir in
    b.min_x >= uunatek_bounds.min_x &&
    b.max_x <= uunatek_bounds.max_x &&
    b.min_y >= uunatek_bounds.min_y &&
    b.max_y <= uunatek_bounds.max_y

(** Scale and center IR to fit Uunatek work area with margin *)
let fit_to_uunatek ?(margin=10.0) (ir : ir) : ir =
  if ir = [] then ir
  else
    let b = Compiler.compute_bounds ir in
    let data_width = b.max_x -. b.min_x in
    let data_height = b.max_y -. b.min_y in
    
    let available_width = uunatek_bounds.max_x -. 2.0 *. margin in
    let available_height = uunatek_bounds.max_y -. 2.0 *. margin in
    
    let scale_x = available_width /. (if data_width > 0.001 then data_width else 1.0) in
    let scale_y = available_height /. (if data_height > 0.001 then data_height else 1.0) in
    let scale = Float.min scale_x scale_y in
    let scale = Float.min scale 1.0 in (* Don't upscale *)
    
    let scaled_width = data_width *. scale in
    let scaled_height = data_height *. scale in
    
    (* Center in work area *)
    let offset_x = margin +. (available_width -. scaled_width) /. 2.0 -. b.min_x *. scale in
    let offset_y = margin +. (available_height -. scaled_height) /. 2.0 -. b.min_y *. scale in
    
    transform_ir (fun p -> 
      { x = p.x *. scale +. offset_x; 
        y = p.y *. scale +. offset_y }
    ) ir

(** Pen up - move without drawing *)
let pen_up config state =
  if state.pen_down then begin
    emit state config.pen_up_command;
    state.pen_down <- false
  end

(** Pen down - start drawing *)
let pen_down config state =
  if not state.pen_down then begin
    emit state config.pen_down_command;
    emit state (Printf.sprintf "G1 F%.0f" config.draw_speed);
    state.pen_down <- true
  end

(** Rapid move (pen up) *)
let rapid_move config state target =
  pen_up config state;
  emit state (Printf.sprintf "G0 %s" (format_point config target));
  state.current_pos <- target

(** Linear move (pen down) *)
let linear_move config state target =
  pen_down config state;
  emit state (Printf.sprintf "G1 %s" (format_point config target));
  state.current_pos <- target

(** Generate G-code for a single path *)
let emit_path config state (path : path) =
  (* Move to start if not already there *)
  if distance state.current_pos path.start > 0.001 then
    rapid_move config state path.start;
  
  (* Draw segments *)
  List.iter (fun seg ->
    match seg with
    | MoveTo p -> rapid_move config state p
    | LineTo p -> linear_move config state p
    | QuadraticTo _ | BezierTo _ | ArcTo _ ->
      (* These should have been flattened already *)
      failwith "Unflattened curve in G-code generation"
  ) path.segments

(** Generate complete G-code from IR *)
let generate ?(config=default_config) (ir : ir) : string =
  let state = create_state () in
  
  (* Flatten all curves *)
  let flattened = List.map (flatten_path config) ir in
  
  (* Optimize path order *)
  let optimized = optimize_paths flattened in
  
  (* Generate G-code *)
  emit_preamble config state;
  
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths to draw" (List.length optimized));
  
  List.iteri (fun i path ->
    if config.include_comments then
      emit state (Printf.sprintf "; Path %d" (i + 1));
    emit_path config state path
  ) optimized;
  
  emit_postamble config state;
  
  (* Return G-code as string *)
  state.lines |> List.rev |> String.concat "\n"

(** Generate G-code optimized for Uunatek plotter *)
let generate_uunatek ?(fit=true) ?(margin=10.0) (ir : ir) : string =
  let config = uunatek_config in
  let state = create_state () in
  
  (* Optionally fit to work area *)
  let scaled_ir = if fit then fit_to_uunatek ~margin ir else ir in
  
  (* Check bounds *)
  if not (check_uunatek_bounds scaled_ir) then
    failwith "Drawing exceeds Uunatek work area (420mm x 297mm)";
  
  (* Flatten all curves *)
  let flattened = List.map (flatten_path config) scaled_ir in
  
  (* Optimize path order *)
  let optimized = optimize_paths flattened in
  
  (* Generate G-code *)
  emit_uunatek_preamble config state;
  
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths to draw" (List.length optimized));
  
  List.iteri (fun i path ->
    if config.include_comments then
      emit state (Printf.sprintf "; Path %d" (i + 1));
    emit_path config state path
  ) optimized;
  
  emit_uunatek_postamble config state;
  
  (* Return G-code as string *)
  state.lines |> List.rev |> String.concat "\n"

(** Generate Uunatek G-code with statistics *)
let generate_uunatek_with_stats ?(fit=true) ?(margin=10.0) (ir : ir) : string * string =
  let config = uunatek_config in
  let state = create_state () in
  
  (* Optionally fit to work area *)
  let scaled_ir = if fit then fit_to_uunatek ~margin ir else ir in
  
  (* Check bounds *)
  if not (check_uunatek_bounds scaled_ir) then
    failwith "Drawing exceeds Uunatek work area (420mm x 297mm)";
  
  (* Flatten all curves *)
  let flattened = List.map (flatten_path config) scaled_ir in
  
  (* Calculate pre-optimization travel *)
  let pre_travel = total_travel_distance flattened (point 0.0 0.0) in
  
  (* Optimize path order *)
  let optimized = optimize_paths flattened in
  
  (* Calculate post-optimization travel *)
  let post_travel = total_travel_distance optimized (point 0.0 0.0) in
  
  (* Generate G-code *)
  emit_uunatek_preamble config state;
  
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths to draw" (List.length optimized));
  
  List.iteri (fun i path ->
    if config.include_comments then
      emit state (Printf.sprintf "; Path %d" (i + 1));
    emit_path config state path
  ) optimized;
  
  emit_uunatek_postamble config state;
  
  let gcode = state.lines |> List.rev |> String.concat "\n" in
  
  (* Calculate total draw distance *)
  let draw_dist = List.fold_left (fun acc path ->
    let rec seg_dist prev_pos = function
      | [] -> 0.0
      | LineTo p :: rest -> distance prev_pos p +. seg_dist p rest
      | MoveTo p :: rest -> seg_dist p rest
      | _ :: rest -> seg_dist prev_pos rest
    in
    acc +. seg_dist path.start path.segments
  ) 0.0 optimized in
  
  let num_segments = List.fold_left (fun acc path -> 
    acc + List.length path.segments
  ) 0 optimized in
  
  (* Estimate time *)
  let draw_time = draw_dist /. config.draw_speed in (* minutes *)
  let travel_time = post_travel /. config.travel_speed in (* minutes *)
  let total_time = draw_time +. travel_time in
  
  let bounds = Compiler.compute_bounds scaled_ir in
  
  let stats = Printf.sprintf 
    "Uunatek Plotter Output\n\
     ----------------------\n\
     Paths: %d\n\
     Segments: %d\n\
     Bounds: X[%.1f - %.1f] Y[%.1f - %.1f] mm\n\
     Draw distance: %.1f mm\n\
     Travel distance: %.1f mm (was %.1f mm, %.0f%% reduction)\n\
     Estimated time: %.1f min (draw: %.1f min, travel: %.1f min)\n\
     G-code lines: %d"
    (List.length optimized)
    num_segments
    bounds.min_x bounds.max_x bounds.min_y bounds.max_y
    draw_dist
    post_travel pre_travel
    (if pre_travel > 0.001 then (1.0 -. post_travel /. pre_travel) *. 100.0 else 0.0)
    total_time draw_time travel_time
    (List.length state.lines)
  in
  
  (gcode, stats)

(** Generate with statistics *)
let generate_with_stats ?(config=default_config) (ir : ir) : string * string =
  let state = create_state () in
  
  (* Flatten all curves *)
  let flattened = List.map (flatten_path config) ir in
  
  (* Calculate pre-optimization travel *)
  let pre_travel = total_travel_distance flattened (point 0.0 0.0) in
  
  (* Optimize path order *)
  let optimized = optimize_paths flattened in
  
  (* Calculate post-optimization travel *)
  let post_travel = total_travel_distance optimized (point 0.0 0.0) in
  
  (* Generate G-code *)
  emit_preamble config state;
  
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths to draw" (List.length optimized));
  
  List.iteri (fun i path ->
    if config.include_comments then
      emit state (Printf.sprintf "; Path %d" (i + 1));
    emit_path config state path
  ) optimized;
  
  emit_postamble config state;
  
  let gcode = state.lines |> List.rev |> String.concat "\n" in
  
  (* Calculate total draw distance *)
  let draw_dist = List.fold_left (fun acc path ->
    let rec seg_dist prev_pos = function
      | [] -> 0.0
      | LineTo p :: rest -> distance prev_pos p +. seg_dist p rest
      | MoveTo p :: rest -> seg_dist p rest
      | _ :: rest -> seg_dist prev_pos rest
    in
    acc +. seg_dist path.start path.segments
  ) 0.0 optimized in
  
  let num_segments = List.fold_left (fun acc path -> 
    acc + List.length path.segments
  ) 0 optimized in
  
  let stats = Printf.sprintf 
    "Paths: %d\n\
     Segments: %d\n\
     Draw distance: %.2f mm\n\
     Travel distance: %.2f mm (was %.2f mm)\n\
     Travel reduction: %.1f%%\n\
     G-code lines: %d"
    (List.length optimized)
    num_segments
    (draw_dist *. config.scale)
    (post_travel *. config.scale)
    (pre_travel *. config.scale)
    (if pre_travel > 0.001 then (1.0 -. post_travel /. pre_travel) *. 100.0 else 0.0)
    (List.length state.lines)
  in
  
  (gcode, stats)

(* ===== SVG Preview Generation ===== *)

(** Generate SVG preview of the paths *)
let generate_svg ?(width=800) ?(height=600) (ir : ir) : string =
  let bounds = Compiler.compute_bounds ir in
  let padding = 20.0 in
  
  (* Calculate scale to fit in viewport *)
  let data_width = bounds.max_x -. bounds.min_x in
  let data_height = bounds.max_y -. bounds.min_y in
  let scale_x = (float_of_int width -. 2.0 *. padding) /. (if data_width > 0.001 then data_width else 1.0) in
  let scale_y = (float_of_int height -. 2.0 *. padding) /. (if data_height > 0.001 then data_height else 1.0) in
  let scale = Float.min scale_x scale_y in
  
  let transform_x x = padding +. (x -. bounds.min_x) *. scale in
  let transform_y y = float_of_int height -. padding -. (y -. bounds.min_y) *. scale in
  
  let path_to_svg (path : path) =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf "M %.2f %.2f" 
      (transform_x path.start.x) (transform_y path.start.y));
    List.iter (fun seg ->
      match seg with
      | MoveTo p ->
        Buffer.add_string buf (Printf.sprintf " M %.2f %.2f" 
          (transform_x p.x) (transform_y p.y))
      | LineTo p ->
        Buffer.add_string buf (Printf.sprintf " L %.2f %.2f" 
          (transform_x p.x) (transform_y p.y))
      | QuadraticTo (c, p) ->
        Buffer.add_string buf (Printf.sprintf " Q %.2f %.2f %.2f %.2f"
          (transform_x c.x) (transform_y c.y)
          (transform_x p.x) (transform_y p.y))
      | BezierTo (c1, c2, p) ->
        Buffer.add_string buf (Printf.sprintf " C %.2f %.2f %.2f %.2f %.2f %.2f"
          (transform_x c1.x) (transform_y c1.y)
          (transform_x c2.x) (transform_y c2.y)
          (transform_x p.x) (transform_y p.y))
      | ArcTo (center, radius, a0, a1) ->
        (* Approximate arc with line segments for SVG *)
        let n = 32 in
        let angle_span = a1 -. a0 in
        for i = 1 to n do
          let t = float_of_int i /. float_of_int n in
          let angle = (a0 +. t *. angle_span) *. Float.pi /. 180.0 in
          let px = center.x +. radius.x *. Float.cos angle in
          let py = center.y +. radius.y *. Float.sin angle in
          Buffer.add_string buf (Printf.sprintf " L %.2f %.2f"
            (transform_x px) (transform_y py))
        done
    ) path.segments;
    Buffer.contents buf
  in
  
  let paths_svg = ir 
    |> List.map (fun path ->
         Printf.sprintf {|  <path d="%s" fill="none" stroke="black" stroke-width="1"/>|}
           (path_to_svg path))
    |> String.concat "\n"
  in
  
  Printf.sprintf 
{|
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <rect width="100%%" height="100%%" fill="white"/>
%s
</svg>|}
    width height width height paths_svg
