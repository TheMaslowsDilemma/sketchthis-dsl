(*
----------------------------------------------------------- 
gcode.ml
----------------------------------------------------------- 
*)

open Vector
open Splines
open Compiler

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

type placement = { pos_x : float; pos_y : float; width : float; height : float }

let default_config =
  {
    travel_speed = 3000.0;
    draw_speed = 1000.0;
    pen_up_command = "G0 Z0";
    pen_down_command = "G0 Z2.2";
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
    pen_up_command = "G0 Z0";
    pen_down_command = "G0 Z2.2";
    scale = 1.0;
    x_offset = 0.0;
    y_offset = 0.0;
    decimal_places = 3;
    include_comments = true;
  }

let machine_bounds = { min_x = 0.0; max_x = 420.0; min_y = 0.0; max_y = 297.0 }

(*** Path Operations ***)

let path_endpoint path =
  match List.rev path.segments with
  | [] -> path.start
  | seg :: _ -> (
      match seg with MoveTo v | LineTo v -> v | ArcTo arc -> arc.endpoint)

let reverse_segment prev_end = function
  | MoveTo v -> (MoveTo prev_end, v)
  | LineTo v -> (LineTo prev_end, v)
  | ArcTo arc ->
      ( ArcTo
          {
            endpoint = prev_end;
            center = arc.center;
            clockwise = not arc.clockwise;
          },
        arc.endpoint )

let reverse_path path =
  let endpoint = path_endpoint path in
  let rec go prev acc = function
    | [] -> acc
    | seg :: rest ->
        let new_seg, new_end = reverse_segment prev seg in
        go new_end (new_seg :: acc) rest
  in
  { start = endpoint; segments = go path.start [] (List.rev path.segments) }

let total_travel paths pos =
  let rec go p total = function
    | [] -> total
    | path :: rest ->
        go (path_endpoint path) (total +. vec_distance p path.start) rest
  in
  go pos 0.0 paths

(*** Deduplication ***)

let vec_approx_eq ?(eps = 0.05) a b =
  Float.abs (a.x -. b.x) < eps && Float.abs (a.y -. b.y) < eps

let seg_approx_eq ?(eps = 0.1) s1 s2 =
  match (s1, s2) with
  | MoveTo a, MoveTo b | LineTo a, LineTo b -> vec_approx_eq ~eps a b
  | ArcTo a, ArcTo b ->
      vec_approx_eq ~eps a.endpoint b.endpoint
      && vec_approx_eq ~eps a.center b.center
      && a.clockwise = b.clockwise
  | _ -> false

let path_approx_eq ?(eps = 0.1) p1 p2 =
  vec_approx_eq ~eps p1.start p2.start
  && List.length p1.segments = List.length p2.segments
  && List.for_all2 (seg_approx_eq ~eps) p1.segments p2.segments

let path_approx_eq_bidir ?(eps = 0.1) p1 p2 =
  path_approx_eq ~eps p1 p2 || path_approx_eq ~eps p1 (reverse_path p2)

let deduplicate_paths ?(eps = 0.1) paths =
  let n_in = List.length paths in
  let rec go acc = function
    | [] -> List.rev acc
    | p :: rest ->
        if List.exists (path_approx_eq_bidir ~eps p) acc then go acc rest
        else go (p :: acc) rest
  in
  let result = go [] paths in
  Printf.printf "deduplicated %d paths\n" (n_in - List.length result);
  result

(*** Greedy Optimization ***)

let optimize_greedy paths =
  if List.length paths <= 1 then paths
  else
    let rec go pos remaining acc =
      match remaining with
      | [] -> List.rev acc
      | _ -> (
          let best = ref None and best_dist = ref Float.infinity in
          List.iter
            (fun p ->
              let d_start = vec_distance pos p.start in
              if d_start < !best_dist then (
                best := Some (p, false);
                best_dist := d_start);
              let d_end = vec_distance pos (path_endpoint p) in
              if d_end < !best_dist then (
                best := Some (p, true);
                best_dist := d_end))
            remaining;
          match !best with
          | None -> List.rev acc
          | Some (nearest, rev) ->
              let p = if rev then reverse_path nearest else nearest in
              go (path_endpoint p)
                (List.filter (fun x -> x != nearest) remaining)
                (p :: acc))
    in
    go (vec 0.0 0.0) paths []

(*** 2-opt Optimization ***)

let optimize_2opt ?(max_iters = 100) paths =
  let n = List.length paths in
  if n <= 3 then paths
  else
    let arr = Array.of_list paths in
    let starts = Array.map (fun p -> p.start) arr in
    let ends = Array.map path_endpoint arr in
    let update i =
      starts.(i) <- arr.(i).start;
      ends.(i) <- path_endpoint arr.(i)
    in
    let improved = ref true and iters = ref 0 in
    while !improved && !iters < max_iters do
      incr iters;
      improved := false;
      for i = 0 to n - 2 do
        for j = i + 2 to n - 1 do
          let curr =
            vec_distance ends.(i) starts.(i + 1)
            +. if j + 1 < n then vec_distance ends.(j) starts.(j + 1) else 0.0
          in
          let next =
            vec_distance ends.(i) ends.(j)
            +.
            if j + 1 < n then vec_distance starts.(i + 1) starts.(j + 1)
            else 0.0
          in
          if next < curr -. 0.001 then begin
            let lo = ref (i + 1) and hi = ref j in
            while !lo < !hi do
              let tmp = arr.(!lo) in
              arr.(!lo) <- reverse_path arr.(!hi);
              arr.(!hi) <- reverse_path tmp;
              update !lo;
              update !hi;
              incr lo;
              decr hi
            done;
            if !lo = !hi then (
              arr.(!lo) <- reverse_path arr.(!lo);
              update !lo);
            improved := true
          end
        done
      done
    done;
    Array.to_list arr

let optimize_paths paths =
  paths |> deduplicate_paths |> optimize_greedy |> optimize_2opt
  |> deduplicate_paths

(*** Bounds and Fitting ***)

let check_machine_bounds ir =
  if ir = [] then true
  else
    let b = compute_bounds ir in
    b.min_x >= machine_bounds.min_x
    && b.max_x <= machine_bounds.max_x
    && b.min_y >= machine_bounds.min_y
    && b.max_y <= machine_bounds.max_y

let fit_to_machine ?(margin = 10.0) ir =
  if ir = [] then ir
  else
    let b = compute_bounds ir in
    let dw = b.max_x -. b.min_x and dh = b.max_y -. b.min_y in
    let aw = machine_bounds.max_x -. (2.0 *. margin) in
    let ah = machine_bounds.max_y -. (2.0 *. margin) in
    let sx = aw /. if dw > Globals.precision then dw else 1.0 in
    let sy = ah /. if dh > Globals.precision then dh else 1.0 in
    let s = Float.min (Float.min sx sy) 1.0 in
    let ox = margin +. ((aw -. (dw *. s)) /. 2.0) -. (b.min_x *. s) in
    let oy = margin +. ((ah -. (dh *. s)) /. 2.0) -. (b.min_y *. s) in
    transform_ir (fun v -> vec ((v.x *. s) +. ox) ((v.y *. s) +. oy)) ir

let fit_to_placement p ir =
  if ir = [] then ir
  else
    let b = compute_bounds ir in
    let dw = b.max_x -. b.min_x and dh = b.max_y -. b.min_y in
    let sx = p.width /. if dw > Globals.precision then dw else 1.0 in
    let sy = p.height /. if dh > Globals.precision then dh else 1.0 in
    let s = Float.min sx sy in
    let ox = p.pos_x -. (b.min_x *. s) in
    let oy = p.pos_y -. (b.min_y *. s) in
    transform_ir (fun v -> vec ((v.x *. s) +. ox) ((v.y *. s) +. oy)) ir

let fit_to_machine_with_placement ?(margin = 10.0) ?placement ir =
  match placement with
  | Some p -> fit_to_placement p ir
  | None -> fit_to_machine ~margin ir

(*** G-code Generation ***)

type state = {
  mutable pos : vec;
  mutable pen_down : bool;
  mutable lines : string list;
}

let emit s line = s.lines <- line :: s.lines
let fmt_coord cfg v = Printf.sprintf "%.*f" cfg.decimal_places v

let fmt_vec cfg v =
  Printf.sprintf "X%s Y%s"
    (fmt_coord cfg ((v.x *. cfg.scale) +. cfg.x_offset))
    (fmt_coord cfg ((v.y *. cfg.scale) +. cfg.y_offset))

let fmt_arc_ij cfg pos center =
  Printf.sprintf "I%s J%s"
    (fmt_coord cfg ((center.x -. pos.x) *. cfg.scale))
    (fmt_coord cfg ((center.y -. pos.y) *. cfg.scale))

let pen_up cfg s =
  if s.pen_down then (
    emit s cfg.pen_up_command;
    s.pen_down <- false)

let pen_down cfg s =
  if not s.pen_down then (
    emit s cfg.pen_down_command;
    emit s (Printf.sprintf "G1 F%.0f" cfg.draw_speed);
    s.pen_down <- true)

let rapid cfg s v =
  pen_up cfg s;
  emit s (Printf.sprintf "G0 %s" (fmt_vec cfg v));
  s.pos <- v

let linear cfg s v =
  pen_down cfg s;
  emit s (Printf.sprintf "G1 %s" (fmt_vec cfg v));
  s.pos <- v

let arc cfg s a =
  pen_down cfg s;
  emit s
    (Printf.sprintf "%s %s %s"
       (if a.clockwise then "G2" else "G3")
       (fmt_vec cfg a.endpoint)
       (fmt_arc_ij cfg s.pos a.center));
  s.pos <- a.endpoint

let emit_path cfg s path =
  if vec_distance s.pos path.start > Globals.precision then
    rapid cfg s path.start;
  List.iter
    (function
      | MoveTo v -> rapid cfg s v
      | LineTo v -> linear cfg s v
      | ArcTo a -> arc cfg s a)
    path.segments

let emit_preamble cfg s =
  if cfg.include_comments then emit s "; Generated by Sketch DSL";
  emit s "G21";
  emit s "G90";
  emit s (Printf.sprintf "G0 F%.0f" cfg.travel_speed);
  emit s cfg.pen_up_command;
  s.pen_down <- false

let emit_postamble cfg s =
  pen_up cfg s;
  emit s "G4 P0.2";
  emit s "G0 X0 Y0";
  if cfg.include_comments then emit s "; End"

let emit_machine_preamble cfg s =
  if cfg.include_comments then (
    emit s "; Generated by Sketch DSL for Machine";
    emit s "; Work area: 420mm x 297mm (A3)");
  emit s "G21";
  emit s "G90";
  emit s "M5";
  emit s "G4 P0.5";
  emit s (Printf.sprintf "G0 F%.0f" cfg.travel_speed);
  s.pen_down <- false

let emit_machine_postamble cfg s =
  emit s cfg.pen_up_command;
  emit s "M5";
  s.pen_down <- false;
  emit s "G4 P0.3";
  emit s "G0 X0 Y0";
  if cfg.include_comments then emit s "; End"

let generate ?(config = default_config) ir =
  let s = { pos = vec 0.0 0.0; pen_down = false; lines = [] } in
  let opt = optimize_paths ir in
  emit_preamble config s;
  if config.include_comments then
    emit s (Printf.sprintf "; %d paths" (List.length opt));
  List.iteri
    (fun i p ->
      if config.include_comments then
        emit s (Printf.sprintf "; Path %d" (i + 1));
      emit_path config s p)
    opt;
  emit_postamble config s;
  s.lines |> List.rev |> String.concat "\n"

let generate_machine ?(fit = true) ?(margin = 10.0) ?placement ir =
  let cfg = machine_config in
  let s = { pos = vec 0.0 0.0; pen_down = false; lines = [] } in
  let scaled =
    if fit then fit_to_machine_with_placement ~margin ?placement ir else ir
  in
  if not (check_machine_bounds scaled) then
    failwith "Drawing exceeds Machine work area (420mm x 297mm)";
  let opt = optimize_paths scaled in
  emit_machine_preamble cfg s;
  if cfg.include_comments then
    emit s (Printf.sprintf "; %d paths" (List.length opt));
  List.iteri
    (fun i p ->
      if cfg.include_comments then emit s (Printf.sprintf "; Path %d" (i + 1));
      emit_path cfg s p)
    opt;
  emit_machine_postamble cfg s;
  s.lines |> List.rev |> String.concat "\n"

(*** Statistics ***)

let arc_length center start_pt end_pt cw =
  let r = vec_distance center start_pt in
  let a1 = Float.atan2 (start_pt.y -. center.y) (start_pt.x -. center.x) in
  let a2 = Float.atan2 (end_pt.y -. center.y) (end_pt.x -. center.x) in
  let da = ref (a2 -. a1) in
  if cw then (if !da > 0.0 then da := !da -. (2.0 *. Float.pi))
  else if !da < 0.0 then da := !da +. (2.0 *. Float.pi);
  r *. Float.abs !da

let draw_distance paths =
  List.fold_left
    (fun acc p ->
      let rec go prev = function
        | [] -> 0.0
        | seg :: rest ->
            let d, next =
              match seg with
              | MoveTo v -> (0.0, v)
              | LineTo v -> (vec_distance prev v, v)
              | ArcTo a ->
                  (arc_length a.center prev a.endpoint a.clockwise, a.endpoint)
            in
            d +. go next rest
      in
      acc +. go p.start p.segments)
    0.0 paths

let count_segments paths =
  List.fold_left
    (fun (m, l, a) p ->
      List.fold_left
        (fun (m, l, a) -> function
          | MoveTo _ -> (m + 1, l, a)
          | LineTo _ -> (m, l + 1, a)
          | ArcTo _ -> (m, l, a + 1))
        (m, l, a) p.segments)
    (0, 0, 0) paths

let generate_with_stats ?(config = default_config) ir =
  let s = { pos = vec 0.0 0.0; pen_down = false; lines = [] } in
  let pre_travel = total_travel ir (vec 0.0 0.0) in
  let opt = optimize_paths ir in
  let post_travel = total_travel opt (vec 0.0 0.0) in
  emit_preamble config s;
  if config.include_comments then
    emit s (Printf.sprintf "; %d paths" (List.length opt));
  List.iteri
    (fun i p ->
      if config.include_comments then
        emit s (Printf.sprintf "; Path %d" (i + 1));
      emit_path config s p)
    opt;
  emit_postamble config s;
  let gcode = s.lines |> List.rev |> String.concat "\n" in
  let draw = draw_distance opt in
  let moves, lines, arcs = count_segments opt in
  let stats =
    Printf.sprintf
      "Paths: %d\n\
       Segments: %d (lines: %d, arcs: %d, moves: %d)\n\
       Draw: %.2f mm\n\
       Travel: %.2f mm (was %.2f mm, %.1f%% reduction)\n\
       G-code lines: %d"
      (List.length opt)
      (moves + lines + arcs)
      lines arcs moves (draw *. config.scale)
      (post_travel *. config.scale)
      (pre_travel *. config.scale)
      (if pre_travel > Globals.precision then
         (1.0 -. (post_travel /. pre_travel)) *. 100.0
       else 0.0)
      (List.length s.lines)
  in
  (gcode, stats)

let generate_machine_with_stats ?(fit = true) ?(margin = 10.0) ?placement ir =
  let cfg = machine_config in
  let s = { pos = vec 0.0 0.0; pen_down = false; lines = [] } in
  let scaled =
    if fit then fit_to_machine_with_placement ~margin ?placement ir else ir
  in
  if not (check_machine_bounds scaled) then
    failwith "Drawing exceeds Machine work area (420mm x 297mm)";
  let pre_opt = List.length scaled in
  let pre_travel = total_travel scaled (vec 0.0 0.0) in
  let opt = optimize_paths scaled in
  let post_travel = total_travel opt (vec 0.0 0.0) in
  emit_machine_preamble cfg s;
  if cfg.include_comments then
    emit s (Printf.sprintf "; %d paths" (List.length opt));
  List.iteri
    (fun i p ->
      if cfg.include_comments then emit s (Printf.sprintf "; Path %d" (i + 1));
      emit_path cfg s p)
    opt;
  emit_machine_postamble cfg s;
  let gcode = s.lines |> List.rev |> String.concat "\n" in
  let draw = draw_distance opt in
  let moves, lines, arcs = count_segments opt in
  let draw_time = draw /. cfg.draw_speed in
  let travel_time = post_travel /. cfg.travel_speed in
  let b = compute_bounds scaled in
  let removed = pre_opt - List.length opt in
  let stats =
    Printf.sprintf
      "Machine Output\n\
       --------------\n\
       Paths: %d%s\n\
       Segments: %d (lines: %d, arcs: %d, moves: %d)\n\
       Bounds: X[%.1f - %.1f] Y[%.1f - %.1f] mm\n\
       Draw: %.1f mm\n\
       Travel: %.1f mm (was %.1f mm, %.0f%% reduction)\n\
       Time: %.1f min (draw: %.1f, travel: %.1f)\n\
       G-code lines: %d"
      (List.length opt)
      (if removed > 0 then Printf.sprintf " (%d duplicates removed)" removed
       else "")
      (moves + lines + arcs)
      lines arcs moves b.min_x b.max_x b.min_y b.max_y draw post_travel
      pre_travel
      (if pre_travel > Globals.precision then
         (1.0 -. (post_travel /. pre_travel)) *. 100.0
       else 0.0)
      (draw_time +. travel_time) draw_time travel_time (List.length s.lines)
  in
  (gcode, stats)

(*** SVG Preview ***)

let svg_arc start_pt arc =
  let r = vec_distance arc.center start_pt in
  let a1 =
    Float.atan2 (start_pt.y -. arc.center.y) (start_pt.x -. arc.center.x)
  in
  let a2 =
    Float.atan2 (arc.endpoint.y -. arc.center.y) (arc.endpoint.x -. arc.center.x)
  in
  let da = ref (a2 -. a1) in
  if arc.clockwise then (if !da > 0.0 then da := !da -. (2.0 *. Float.pi))
  else if !da < 0.0 then da := !da +. (2.0 *. Float.pi);
  Printf.sprintf "A %.2f %.2f 0 %d %d" r r
    (if Float.abs !da > Float.pi then 1 else 0)
    (if arc.clockwise then 1 else 0)

let generate_svg ?(width = 800) ?(height = 600) ir =
  let b = compute_bounds ir in
  let pad = 20.0 in
  let dw = b.max_x -. b.min_x and dh = b.max_y -. b.min_y in
  let sx =
    (float_of_int width -. (2.0 *. pad))
    /. if dw > Globals.precision then dw else 1.0
  in
  let sy =
    (float_of_int height -. (2.0 *. pad))
    /. if dh > Globals.precision then dh else 1.0
  in
  let s = Float.min sx sy in
  let tx v =
    vec
      (pad +. ((v.x -. b.min_x) *. s))
      (float_of_int height -. pad -. ((v.y -. b.min_y) *. s))
  in
  let path_svg p =
    let buf = Buffer.create 256 in
    let st = tx p.start in
    Buffer.add_string buf (Printf.sprintf "M %.2f %.2f" st.x st.y);
    let cur = ref p.start in
    List.iter
      (fun seg ->
        match seg with
        | MoveTo v ->
            let vt = tx v in
            Buffer.add_string buf (Printf.sprintf " M %.2f %.2f" vt.x vt.y);
            cur := v
        | LineTo v ->
            let vt = tx v in
            Buffer.add_string buf (Printf.sprintf " L %.2f %.2f" vt.x vt.y);
            cur := v
        | ArcTo a ->
            let cur_t = tx !cur in
            let a_t =
              {
                endpoint = tx a.endpoint;
                center = tx a.center;
                clockwise = not a.clockwise;
              }
            in
            Buffer.add_string buf
              (Printf.sprintf " %s %.2f %.2f" (svg_arc cur_t a_t) a_t.endpoint.x
                 a_t.endpoint.y);
            cur := a.endpoint)
      p.segments;
    Buffer.contents buf
  in
  let paths_svg =
    optimize_paths ir
    |> List.map (fun p ->
        Printf.sprintf
          {|  <path d="%s" fill="none" stroke="black" stroke-width="1"/>|}
          (path_svg p))
    |> String.concat "\n"
  in
  Printf.sprintf
    {|<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <rect width="100%%" height="100%%" fill="white"/>
%s
</svg>|}
    width height width height paths_svg
