(* Sketch DSL G-code Generator *)
open Compiler
open Ast

(*** G-code Configuration ***)

type gcode_config = {
  travel_speed : float;
  draw_speed : float;
  pen_up_command : string;
  pen_down_command : string;
  scale : float;
  x_offset : float;
  y_offset : float;
  decimal_places : int;
  include_comments : bool;
}

let default_config =
  {
    travel_speed = 3000.0;
    draw_speed = 1000.0;
    pen_up_command = "G0 Z0";
    pen_down_command = "G0 Z1.5";
    scale = 1.0;
    x_offset = 0.0;
    y_offset = 0.0;
    decimal_places = 3;
    include_comments = true;
  }

let machine_config =
  {
    travel_speed = 11000.0;
    draw_speed = 3000.0;
    pen_up_command = "G0 Z1.5";
    pen_down_command = "G0 Z1.5";
    scale = 1.0;
    x_offset = 0.0;
    y_offset = 0.0;
    decimal_places = 3;
    include_comments = true;
  }

let machine_bounds = { min_x = 0.0; max_x = 420.0; min_y = 0.0; max_y = 297.0 }

(*** Path Optimization ***)

let distance a b =
  let dx = b.x -. a.x in
  let dy = b.y -. a.y in
  Float.sqrt ((dx *. dx) +. (dy *. dy))

let path_endpoint (path : path) : vec =
  match List.rev path.segments with
  | [] -> path.start
  | seg :: _ -> ( match seg with MoveTo v | LineTo v -> v)

let reverse_path (path : path) : path =
  let endpoint = path_endpoint path in
  let rec reverse_segments prev_end acc = function
    | [] -> acc
    | seg :: rest ->
        let new_seg, new_end =
          match seg with
          | MoveTo v -> (MoveTo prev_end, v)
          | LineTo v -> (LineTo prev_end, v)
        in
        reverse_segments new_end (new_seg :: acc) rest
  in
  let reversed_segs = reverse_segments path.start [] (List.rev path.segments) in
  { start = endpoint; segments = reversed_segs }

let total_travel_distance paths current_pos =
  let rec go pos total = function
    | [] -> total
    | path :: rest ->
        let dist_to_start = distance pos path.start in
        go (path_endpoint path) (total +. dist_to_start) rest
  in
  go current_pos 0.0 paths

let optimize_paths_greedy (paths : path list) : path list =
  if List.length paths <= 1 then paths
  else
    let rec go current_pos remaining acc =
      match remaining with
      | [] -> List.rev acc
      | _ -> (
          let find_nearest () =
            let best = ref None in
            let best_dist = ref Float.infinity in
            List.iter
              (fun path ->
                let d_start = distance current_pos path.start in
                if d_start < !best_dist then begin
                  best := Some (path, false);
                  best_dist := d_start
                end;
                let d_end = distance current_pos (path_endpoint path) in
                if d_end < !best_dist then begin
                  best := Some (path, true);
                  best_dist := d_end
                end)
              remaining;
            !best
          in
          match find_nearest () with
          | None -> List.rev acc
          | Some (nearest, reversed) ->
              let path_to_add =
                if reversed then reverse_path nearest else nearest
              in
              let new_pos = path_endpoint path_to_add in
              let new_remaining =
                List.filter (fun p -> p != nearest) remaining
              in
              go new_pos new_remaining (path_to_add :: acc))
    in
    go { x = 0.0; y = 0.0 } paths []

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
          let p_i = arr.(i) in
          let p_i1 = arr.(i + 1) in
          let p_j = arr.(j) in
          let p_j1 = if j + 1 < n then Some arr.(j + 1) else None in
          let end_i = path_endpoint p_i in
          let end_j = path_endpoint p_j in
          let current_dist =
            distance end_i p_i1.start
            +. match p_j1 with Some p -> distance end_j p.start | None -> 0.0
          in
          let new_dist =
            distance end_i p_j.start
            +.
            match p_j1 with
            | Some p -> distance (path_endpoint p_i1) p.start
            | None -> 0.0
          in
          if new_dist < current_dist -. 0.001 then begin
            let rec reverse_range lo hi =
              if lo < hi then begin
                let tmp = arr.(lo) in
                arr.(lo) <- reverse_path arr.(hi);
                arr.(hi) <- reverse_path tmp;
                reverse_range (lo + 1) (hi - 1)
              end
              else if lo = hi then arr.(lo) <- reverse_path arr.(lo)
            in
            reverse_range (i + 1) j;
            improved := true
          end
        done
      done
    done;
    Array.to_list arr

let optimize_paths paths = paths |> optimize_paths_greedy |> optimize_2opt

(*** G-code Generation ***)

type gcode_state = {
  mutable current_pos : vec;
  mutable pen_down : bool;
  mutable lines : string list;
}

let create_state () =
  { current_pos = { x = 0.0; y = 0.0 }; pen_down = false; lines = [] }

let emit state line = state.lines <- line :: state.lines
let format_coord config v = Printf.sprintf "%.*f" config.decimal_places v

let format_vec config v =
  let x = (v.x *. config.scale) +. config.x_offset in
  let y = (v.y *. config.scale) +. config.y_offset in
  Printf.sprintf "X%s Y%s" (format_coord config x) (format_coord config y)

let emit_preamble config state =
  if config.include_comments then emit state "; Generated by Sketch DSL";
  emit state "G21";
  emit state "G90";
  emit state (Printf.sprintf "G0 F%.0f" config.travel_speed);
  emit state config.pen_up_command;
  state.pen_down <- false

let emit_postamble config state =
  if state.pen_down then begin
    emit state config.pen_up_command;
    state.pen_down <- false
  end;
  emit state "G0 X0 Y0";
  emit state "M5";
  if config.include_comments then emit state "; End"

let emit_machine_preamble config state =
  if config.include_comments then begin
    emit state "; Generated by Sketch DSL for Machine";
    emit state "; Work area: 420mm x 297mm (A3)"
  end;
  emit state "G21";
  emit state "G90";
  emit state "M5";
  emit state "$H";
  emit state "G4 P0.5";
  emit state (Printf.sprintf "G0 F%.0f" config.travel_speed);
  state.pen_down <- false

let emit_machine_postamble config state =
  if state.pen_down then begin
    emit state "M5";
    state.pen_down <- false
  end;
  emit state "G4 P0.2";
  emit state "G0 X0 Y0";
  emit state "M5";
  if config.include_comments then emit state "; End"

let check_machine_bounds (ir : ir) : bool =
  if ir = [] then true
  else
    let b = compute_bounds ir in
    b.min_x >= machine_bounds.min_x
    && b.max_x <= machine_bounds.max_x
    && b.min_y >= machine_bounds.min_y
    && b.max_y <= machine_bounds.max_y

let fit_to_machine ?(margin = 10.0) (ir : ir) : ir =
  if ir = [] then ir
  else
    let b = compute_bounds ir in
    let data_width = b.max_x -. b.min_x in
    let data_height = b.max_y -. b.min_y in
    let available_width = machine_bounds.max_x -. (2.0 *. margin) in
    let available_height = machine_bounds.max_y -. (2.0 *. margin) in
    let scale_x =
      available_width /. if data_width > 0.001 then data_width else 1.0
    in
    let scale_y =
      available_height /. if data_height > 0.001 then data_height else 1.0
    in
    let scale = Float.min (Float.min scale_x scale_y) 1.0 in
    let scaled_width = data_width *. scale in
    let scaled_height = data_height *. scale in
    let offset_x =
      margin +. ((available_width -. scaled_width) /. 2.0) -. (b.min_x *. scale)
    in
    let offset_y =
      margin
      +. ((available_height -. scaled_height) /. 2.0)
      -. (b.min_y *. scale)
    in
    transform_ir
      (fun v ->
        { x = (v.x *. scale) +. offset_x; y = (v.y *. scale) +. offset_y })
      ir

let pen_up config state =
  if state.pen_down then begin
    emit state config.pen_up_command;
    state.pen_down <- false
  end

let pen_down config state =
  if not state.pen_down then begin
    emit state config.pen_down_command;
    emit state (Printf.sprintf "G1 F%.0f" config.draw_speed);
    state.pen_down <- true
  end

let rapid_move config state target =
  pen_up config state;
  emit state (Printf.sprintf "G0 %s" (format_vec config target));
  state.current_pos <- target

let linear_move config state target =
  pen_down config state;
  emit state (Printf.sprintf "G1 %s" (format_vec config target));
  state.current_pos <- target

let emit_path config state (path : path) =
  if distance state.current_pos path.start > 0.001 then
    rapid_move config state path.start;
  List.iter
    (fun seg ->
      match seg with
      | MoveTo v -> rapid_move config state v
      | LineTo v -> linear_move config state v)
    path.segments

let generate ?(config = default_config) (ir : ir) : string =
  let state = create_state () in
  let optimized = optimize_paths ir in
  emit_preamble config state;
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths" (List.length optimized));
  List.iteri
    (fun i path ->
      if config.include_comments then
        emit state (Printf.sprintf "; Path %d" (i + 1));
      emit_path config state path)
    optimized;
  emit_postamble config state;
  state.lines |> List.rev |> String.concat "\n"

let generate_machine ?(fit = true) ?(margin = 10.0) (ir : ir) : string =
  let config = machine_config in
  let state = create_state () in
  let scaled_ir = if fit then fit_to_machine ~margin ir else ir in
  if not (check_machine_bounds scaled_ir) then
    failwith "Drawing exceeds Machine work area (420mm x 297mm)";
  let optimized = optimize_paths scaled_ir in
  emit_machine_preamble config state;
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths" (List.length optimized));
  List.iteri
    (fun i path ->
      if config.include_comments then
        emit state (Printf.sprintf "; Path %d" (i + 1));
      emit_path config state path)
    optimized;
  emit_machine_postamble config state;
  state.lines |> List.rev |> String.concat "\n"

(*** Statistics ***)

let calc_draw_distance (paths : path list) : float =
  List.fold_left
    (fun acc path ->
      let rec seg_dist prev = function
        | [] -> 0.0
        | seg :: rest ->
            let d, next =
              match seg with
              | MoveTo v -> (0.0, v)
              | LineTo v -> (distance prev v, v)
            in
            d +. seg_dist next rest
      in
      acc +. seg_dist path.start path.segments)
    0.0 paths

let count_segments (paths : path list) : int * int =
  List.fold_left
    (fun (moves, lines) path ->
      List.fold_left
        (fun (m, l) seg ->
          match seg with MoveTo _ -> (m + 1, l) | LineTo _ -> (m, l + 1))
        (moves, lines) path.segments)
    (0, 0) paths

let generate_machine_with_stats ?(fit = true) ?(margin = 10.0) (ir : ir) :
    string * string =
  let config = machine_config in
  let state = create_state () in
  let scaled_ir = if fit then fit_to_machine ~margin ir else ir in
  if not (check_machine_bounds scaled_ir) then
    failwith "Drawing exceeds Machine work area (420mm x 297mm)";
  let pre_travel = total_travel_distance scaled_ir { x = 0.0; y = 0.0 } in
  let optimized = optimize_paths scaled_ir in
  let post_travel = total_travel_distance optimized { x = 0.0; y = 0.0 } in
  emit_machine_preamble config state;
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths" (List.length optimized));
  List.iteri
    (fun i path ->
      if config.include_comments then
        emit state (Printf.sprintf "; Path %d" (i + 1));
      emit_path config state path)
    optimized;
  emit_machine_postamble config state;
  let gcode = state.lines |> List.rev |> String.concat "\n" in
  let draw_dist = calc_draw_distance optimized in
  let num_moves, num_lines = count_segments optimized in
  let draw_time = draw_dist /. config.draw_speed in
  let travel_time = post_travel /. config.travel_speed in
  let total_time = draw_time +. travel_time in
  let bounds = compute_bounds scaled_ir in
  let stats =
    Printf.sprintf
      "Machine Output\n\
       --------------\n\
       Paths: %d\n\
       Segments: %d (lines: %d, moves: %d)\n\
       Bounds: X[%.1f - %.1f] Y[%.1f - %.1f] mm\n\
       Draw: %.1f mm\n\
       Travel: %.1f mm (was %.1f mm, %.0f%% reduction)\n\
       Time: %.1f min (draw: %.1f, travel: %.1f)\n\
       G-code lines: %d"
      (List.length optimized) (num_moves + num_lines) num_lines num_moves
      bounds.min_x bounds.max_x bounds.min_y bounds.max_y draw_dist post_travel
      pre_travel
      (if pre_travel > 0.001 then (1.0 -. (post_travel /. pre_travel)) *. 100.0
       else 0.0)
      total_time draw_time travel_time (List.length state.lines)
  in
  (gcode, stats)

let generate_with_stats ?(config = default_config) (ir : ir) : string * string =
  let state = create_state () in
  let pre_travel = total_travel_distance ir { x = 0.0; y = 0.0 } in
  let optimized = optimize_paths ir in
  let post_travel = total_travel_distance optimized { x = 0.0; y = 0.0 } in
  emit_preamble config state;
  if config.include_comments then
    emit state (Printf.sprintf "; %d paths" (List.length optimized));
  List.iteri
    (fun i path ->
      if config.include_comments then
        emit state (Printf.sprintf "; Path %d" (i + 1));
      emit_path config state path)
    optimized;
  emit_postamble config state;
  let gcode = state.lines |> List.rev |> String.concat "\n" in
  let draw_dist = calc_draw_distance optimized in
  let num_moves, num_lines = count_segments optimized in
  let stats =
    Printf.sprintf
      "Paths: %d\n\
       Segments: %d (lines: %d, moves: %d)\n\
       Draw: %.2f mm\n\
       Travel: %.2f mm (was %.2f mm, %.1f%% reduction)\n\
       G-code lines: %d"
      (List.length optimized) (num_moves + num_lines) num_lines num_moves
      (draw_dist *. config.scale)
      (post_travel *. config.scale)
      (pre_travel *. config.scale)
      (if pre_travel > 0.001 then (1.0 -. (post_travel /. pre_travel)) *. 100.0
       else 0.0)
      (List.length state.lines)
  in
  (gcode, stats)

(*** SVG Preview ***)

let generate_svg ?(width = 800) ?(height = 600) (ir : ir) : string =
  let bounds = compute_bounds ir in
  let padding = 20.0 in
  let data_width = bounds.max_x -. bounds.min_x in
  let data_height = bounds.max_y -. bounds.min_y in
  let scale_x =
    (float_of_int width -. (2.0 *. padding))
    /. if data_width > 0.001 then data_width else 1.0
  in
  let scale_y =
    (float_of_int height -. (2.0 *. padding))
    /. if data_height > 0.001 then data_height else 1.0
  in
  let scale = Float.min scale_x scale_y in
  let tx x = padding +. ((x -. bounds.min_x) *. scale) in
  let ty y = float_of_int height -. padding -. ((y -. bounds.min_y) *. scale) in
  let path_to_svg path =
    let buf = Buffer.create 256 in
    Buffer.add_string buf
      (Printf.sprintf "M %.2f %.2f" (tx path.start.x) (ty path.start.y));
    List.iter
      (fun seg ->
        match seg with
        | MoveTo v ->
            Buffer.add_string buf
              (Printf.sprintf " M %.2f %.2f" (tx v.x) (ty v.y))
        | LineTo v ->
            Buffer.add_string buf
              (Printf.sprintf " L %.2f %.2f" (tx v.x) (ty v.y)))
      path.segments;
    Buffer.contents buf
  in
  let paths_svg =
    ir
    |> List.map (fun path ->
        Printf.sprintf
          {|  <path d="%s" fill="none" stroke="black" stroke-width="1"/>|}
          (path_to_svg path))
    |> String.concat "\n"
  in
  Printf.sprintf
    {|<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <rect width="100%%" height="100%%" fill="white"/>
%s
</svg>|}
    width height width height paths_svg
