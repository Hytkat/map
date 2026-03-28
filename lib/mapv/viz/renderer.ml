open Raylib
open Core

type view_mode = Registers | Heap | Both

type playback = {
  mutable tick : int;
  mutable playing : bool;
  mutable speed : float;
  mutable accum : float;
}

type highlight = {
  mutable reg : int option;
  mutable addr : int option;
  mutable age : float;
}

type state = {
  trace : Trace.Serializer.Read.t;
  pb : playback;
  hl : highlight;
  mutable mode : view_mode;
  mutable show_settings : bool;
  total_ticks : int;
}

let screen_w = 1920
let screen_h = 1080
let scale = 1.25
let sw = int_of_float (float_of_int screen_w /. scale)
let sh = int_of_float (float_of_int screen_h /. scale)
let col_bg = Color.create 15 15 20 255
let col_panel = Color.create 22 22 30 255
let col_border = Color.create 45 45 60 255
let col_text = Color.create 210 210 220 255
let col_dim = Color.create 90 90 110 255
let col_accent = Color.create 100 180 255 255
let col_green = Color.create 80 220 120 255
let col_red = Color.create 220 80 80 255
let col_yellow = Color.create 220 200 80 255
let col_young = Color.create 60 120 200 255
let col_old = Color.create 180 80 180 255
let col_free = Color.create 30 30 40 255
let col_scrubber = Color.create 50 50 70 255
let col_thumb = Color.create 100 180 255 255
let col_fiber_run = Color.create 80 220 120 255
let col_fiber_sus = Color.create 220 200 80 255
let col_fiber_dead = Color.create 90 90 110 255
let col_highlight = Color.create 255 220 100 255
let font_size_sm = 16
let font_size_md = 18
let font_size_lg = 24
let pad = 14
let bar_h = 52
let ctrl_h = 64
let instr_h = 56
let s x = int_of_float (float_of_int x *. scale)

let op_name op =
  match op with
  | 0x00 -> "Nop"
  | 0x01 -> "Halt"
  | 0x02 -> "Mov"
  | 0x03 -> "Load"
  | 0x04 -> "LoadF"
  | 0x05 -> "LoadB"
  | 0x06 -> "LoadNil"
  | 0x07 -> "LoadK"
  | 0x08 -> "LoadS"
  | 0x10 -> "Add"
  | 0x11 -> "Sub"
  | 0x12 -> "Mul"
  | 0x13 -> "Div"
  | 0x14 -> "Mod"
  | 0x15 -> "AddI"
  | 0x16 -> "SubI"
  | 0x17 -> "MulI"
  | 0x18 -> "AddF"
  | 0x19 -> "SubF"
  | 0x1A -> "MulF"
  | 0x1B -> "DivF"
  | 0x20 -> "And"
  | 0x21 -> "Or"
  | 0x22 -> "Xor"
  | 0x23 -> "Shl"
  | 0x24 -> "Shr"
  | 0x25 -> "ShrU"
  | 0x26 -> "ShlI"
  | 0x27 -> "ShrI"
  | 0x28 -> "ShrUI"
  | 0x30 -> "Eq"
  | 0x31 -> "Ne"
  | 0x32 -> "Lt"
  | 0x33 -> "LtU"
  | 0x34 -> "Lte"
  | 0x35 -> "LteU"
  | 0x36 -> "EqF"
  | 0x37 -> "NeF"
  | 0x38 -> "LtF"
  | 0x39 -> "LteF"
  | 0x40 -> "I2F"
  | 0x41 -> "F2I"
  | 0x42 -> "TypeOf"
  | 0x50 -> "Alloc"
  | 0x51 -> "GetField"
  | 0x52 -> "SetField"
  | 0x53 -> "GetTag"
  | 0x54 -> "Len"
  | 0x60 -> "Jmp"
  | 0x61 -> "Jz"
  | 0x62 -> "Jnz"
  | 0x70 -> "Call"
  | 0x71 -> "TailCall"
  | 0x72 -> "DynCall"
  | 0x73 -> "TailDynCall"
  | 0x74 -> "Ret"
  | 0x80 -> "Try"
  | 0x81 -> "Throw"
  | 0x82 -> "EndTry"
  | 0x90 -> "ConNew"
  | 0x91 -> "ConYield"
  | 0x92 -> "ConResume"
  | 0x93 -> "ConStatus"
  | _ -> Printf.sprintf "0x%02X" op

let value_to_string v =
  match v with
  | Value.Nil -> "nil"
  | Value.Bool b -> if b then "true" else "false"
  | Value.Int n -> string_of_int n
  | Value.Float f -> Printf.sprintf "%.6g" f
  | Value.Ptr p -> Printf.sprintf "ptr(%d)" p
  | Value.NativeFun _ -> "<fun>"
  | Value.NativePtr _ -> "<native>"

let reg_value_at trace tick reg =
  let writes = trace.Trace.Serializer.Read.vm.reg_writes in
  let last = ref None in
  Array.iter
    (fun (t, r, v) -> if t <= tick && r = reg then last := Some v)
    writes;
  !last

let reg_last_written_at trace tick reg =
  let writes = trace.Trace.Serializer.Read.vm.reg_writes in
  let last_tick = ref (-1) in
  Array.iter
    (fun (t, r, _) -> if t <= tick && r = reg then last_tick := t)
    writes;
  !last_tick

let allocs_at trace tick =
  Array.to_seq trace.Trace.Serializer.Read.heap.allocs
  |> Seq.filter (fun (t, _, _, _) -> t <= tick)
  |> List.of_seq

let frees_at trace tick =
  Array.to_seq trace.Trace.Serializer.Read.heap.frees
  |> Seq.filter (fun (t, _) -> t <= tick)
  |> Seq.map snd |> List.of_seq

let calls_at trace tick =
  Array.to_seq trace.Trace.Serializer.Read.vm.calls
  |> Seq.filter (fun (t, _, _) -> t <= tick)
  |> List.of_seq

let rets_at trace tick =
  Array.to_seq trace.Trace.Serializer.Read.vm.rets
  |> Seq.filter (fun (t, _) -> t <= tick)
  |> List.of_seq

let con_events_at trace tick =
  let news =
    Array.to_seq trace.Trace.Serializer.Read.vm.con_news
    |> Seq.filter (fun (t, _) -> t <= tick)
    |> List.of_seq
  in
  let yields =
    Array.to_seq trace.Trace.Serializer.Read.vm.con_yields
    |> Seq.filter (fun (t, _) -> t <= tick)
    |> List.of_seq
  in
  let resumes =
    Array.to_seq trace.Trace.Serializer.Read.vm.con_resumes
    |> Seq.filter (fun (t, _) -> t <= tick)
    |> List.of_seq
  in
  (news, yields, resumes)

let current_instr trace tick =
  let instrs = trace.Trace.Serializer.Read.vm.instrs in
  let last = ref None in
  Array.iter
    (fun (t, pc, op) -> if t <= tick then last := Some (t, pc, op))
    instrs;
  !last

let draw_panel x y w h =
  draw_rectangle (s x) (s y) (s w) (s h) col_panel;
  draw_rectangle_lines (s x) (s y) (s w) (s h) col_border

let draw_label x y text col = draw_text text (s x) (s y) (s font_size_sm) col

let draw_section_title x y text =
  draw_text text (s x) (s y) (s font_size_md) col_accent

let lerp_color a b t =
  let f x y =
    int_of_float (float_of_int x +. ((float_of_int y -. float_of_int x) *. t))
  in
  let r = f (Color.r a) (Color.r b) in
  let g = f (Color.g a) (Color.g b) in
  let bl = f (Color.b a) (Color.b b) in
  Color.create r g bl 255

let active_regs_at trace tick =
  let writes = trace.Trace.Serializer.Read.vm.reg_writes in
  let seen = Hashtbl.create 32 in
  Array.iter
    (fun (t, r, _) -> if t <= tick then Hashtbl.replace seen r ())
    writes;
  seen

let draw_registers state x y w h =
  draw_panel x y w h;
  draw_section_title (x + pad) (y + pad) "REGISTERS";
  let row_h = 24 in
  let header_h = (pad * 2) + font_size_md in
  let avail_h = h - header_h in
  let max_rows = avail_h / row_h in
  let visible = min 32 max_rows in
  let active = active_regs_at state.trace state.pb.tick in
  for i = 0 to visible - 1 do
    let ry = y + header_h + (i * row_h) in
    let last_t = reg_last_written_at state.trace state.pb.tick i in
    let recently_written = last_t >= 0 && state.pb.tick - last_t <= 3 in
    let is_hl = state.hl.reg = Some i in
    let has_value = Hashtbl.mem active i in
    let bg =
      if is_hl then lerp_color col_accent col_panel (1.0 -. state.hl.age)
      else if recently_written then
        lerp_color col_highlight col_panel
          (float_of_int (state.pb.tick - last_t) /. 3.0)
      else if has_value then Color.create 28 28 38 255
      else col_panel
    in
    draw_rectangle (s (x + pad)) (s ry) (s (w - (pad * 2))) (s (row_h - 2)) bg;
    let label = Printf.sprintf "r%-2d" i in
    let label_col = if has_value then col_dim else Color.create 50 50 65 255 in
    draw_text label (s (x + pad + 4)) (s (ry + 4)) (s font_size_sm) label_col;
    let val_str =
      match reg_value_at state.trace state.pb.tick i with
      | None -> "-"
      | Some v -> value_to_string v
    in
    let val_col =
      if recently_written then col_highlight
      else if has_value then col_text
      else col_dim
    in
    draw_text val_str (s (x + pad + 52)) (s (ry + 4)) (s font_size_sm) val_col
  done;
  let call_y = y + header_h + (visible * row_h) + pad in
  if call_y + 80 < y + h then begin
    draw_rectangle (s (x + pad)) (s call_y) (s (w - (pad * 2))) 1 col_border;
    let call_y = call_y + pad in
    draw_section_title (x + pad) call_y "CALL STACK";
    let calls = calls_at state.trace state.pb.tick in
    let rets = rets_at state.trace state.pb.tick in
    let depth = List.length calls - List.length rets in
    let depth = max 0 depth in
    let active_calls =
      let all = Array.of_list calls in
      let n = Array.length all in
      let start = max 0 (n - depth) in
      Array.to_list (Array.sub all start (n - start))
    in
    let n = List.length active_calls in
    if n = 0 then
      draw_text "  (empty)"
        (s (x + pad + 4))
        (s (call_y + font_size_md + pad))
        (s font_size_sm) col_dim
    else
      List.iteri
        (fun i (_, pc, target) ->
          let cy = call_y + font_size_md + pad + (i * 26) in
          if cy < y + h - pad then begin
            let is_top = i = n - 1 in
            let frame_col =
              if is_top then col_accent else Color.create 40 50 70 255
            in
            draw_rectangle
              (s (x + pad))
              (s cy)
              (s (w - (pad * 2)))
              (s 22) frame_col;
            let depth_str = Printf.sprintf "#%d" i in
            draw_text depth_str
              (s (x + pad + 4))
              (s (cy + 3))
              (s font_size_sm)
              (if is_top then col_bg else col_dim);
            let s_str = Printf.sprintf "pc=%d -> %d" pc target in
            draw_text s_str
              (s (x + pad + 36))
              (s (cy + 3))
              (s font_size_sm)
              (if is_top then col_bg else col_text)
          end)
        active_calls
  end

let draw_heap state x y w h =
  draw_panel x y w h;
  draw_section_title (x + pad) (y + pad) "HEAP";
  let allocs = allocs_at state.trace state.pb.tick in
  let freed = frees_at state.trace state.pb.tick in
  let cell_sz = 16 in
  let cell_gap = 3 in
  let cols = (w - (pad * 2)) / (cell_sz + cell_gap) in
  let header_h = (pad * 2) + font_size_md in
  let young_label_y = y + header_h in
  draw_label (x + pad) young_label_y "YOUNG GEN" col_dim;
  let young_cells_y = young_label_y + font_size_sm + 6 in
  let young_allocs = List.filter (fun (_, _, _, tag) -> tag < 10) allocs in
  let old_allocs = List.filter (fun (_, _, _, tag) -> tag >= 10) allocs in
  let rows_for cells =
    let n = List.length cells in
    max 1 ((n + cols - 1) / cols)
  in
  let young_rows = min 4 (rows_for young_allocs) in
  let draw_cells cells base_y is_young max_rows =
    List.iteri
      (fun i (_, addr, size, _tag) ->
        let row = i / cols in
        let col = i mod cols in
        if row < max_rows then begin
          let cx = x + pad + (col * (cell_sz + cell_gap)) in
          let cy = base_y + (row * (cell_sz + cell_gap)) in
          let is_freed = List.mem addr freed in
          let color =
            if is_freed then col_free
            else if is_young then col_young
            else col_old
          in
          draw_rectangle (s cx) (s cy) (s cell_sz) (s cell_sz) color;
          if size > 1 then
            draw_rectangle
              (s (cx + 3))
              (s (cy + 3))
              (s (cell_sz - 6))
              (s (cell_sz - 6))
              (lerp_color color col_bg 0.5)
        end)
      cells
  in
  draw_cells young_allocs young_cells_y true young_rows;
  let old_label_y = young_cells_y + (young_rows * (cell_sz + cell_gap)) + pad in
  draw_label (x + pad) old_label_y "OLD GEN" col_dim;
  let old_cells_y = old_label_y + font_size_sm + 6 in
  let old_rows = min 4 (rows_for old_allocs) in
  draw_cells old_allocs old_cells_y false old_rows;
  let stats_y = old_cells_y + (old_rows * (cell_sz + cell_gap)) + pad in
  if stats_y + 20 < y + h then begin
    let n_young = List.length young_allocs in
    let n_old = List.length old_allocs in
    let n_freed = List.length freed in
    let stats =
      Printf.sprintf "young: %d   old: %d   freed: %d" n_young n_old n_freed
    in
    draw_text stats (s (x + pad)) (s stats_y) (s font_size_sm) col_dim
  end;
  let fiber_y = stats_y + font_size_sm + (pad * 2) in
  if fiber_y + 20 < y + h then begin
    draw_rectangle
      (s (x + pad))
      (s (fiber_y - pad))
      (s (w - (pad * 2)))
      1 col_border;
    draw_section_title (x + pad) fiber_y "FIBERS";
    let news, yields, resumes = con_events_at state.trace state.pb.tick in
    let n_fibers = List.length news in
    let n_yielded = List.length yields in
    let n_resumed = List.length resumes in
    for i = 0 to n_fibers - 1 do
      let fy = fiber_y + font_size_md + pad + (i * 28) in
      if fy < y + h - pad then begin
        let is_running = i < n_resumed in
        let is_yielded = i < n_yielded in
        let status, color =
          if is_running then ("running", col_fiber_run)
          else if is_yielded then ("suspended", col_fiber_sus)
          else ("dead", col_fiber_dead)
        in
        draw_rectangle
          (s (x + pad))
          (s fy)
          (s (w - (pad * 2)))
          (s 22)
          (Color.create 28 28 38 255);
        draw_circle (s (x + pad + 14)) (s (fy + 11)) (s 5 |> float_of_int) color;
        let label = Printf.sprintf "fiber_%d" i in
        draw_text label
          (s (x + pad + 28))
          (s (fy + 4))
          (s font_size_sm) col_text;
        draw_text status
          (s (x + pad + 28 + 80))
          (s (fy + 4))
          (s font_size_sm) color
      end
    done;
    ignore n_yielded;
    ignore n_resumed
  end

let draw_gc_overlay state x y w =
  let gc = state.trace.Trace.Serializer.Read.gc.events in
  let in_gc = ref false in
  Array.iter
    (fun (t, ev) ->
      if t <= state.pb.tick then
        match ev with
        | Heap.Minor_start | Heap.Major_mark _ -> in_gc := true
        | Heap.Minor_end _ | Heap.Major_end -> in_gc := false
        | _ -> ())
    gc;
  if !in_gc then begin
    draw_rectangle (s x) (s y) (s w) (s 4) col_red;
    draw_rectangle
      (s (x + w - 60))
      (s (y + 6))
      (s 52) (s 20)
      (Color.create 60 20 20 255);
    draw_text "GC" (s (x + w - 52)) (s (y + 8)) (s font_size_sm) col_red
  end

let draw_instr_bar state x y w =
  draw_panel x y w instr_h;
  match current_instr state.trace state.pb.tick with
  | None ->
      draw_text "no instruction"
        (s (x + pad))
        (s (y + (instr_h / 2) - (font_size_sm / 2)))
        (s font_size_sm) col_dim
  | Some (tick, pc, op) -> (
      let tick_str = Printf.sprintf "tick %05d / %05d" tick state.total_ticks in
      draw_text tick_str (s (x + pad)) (s (y + pad)) (s font_size_sm) col_dim;
      let op_str = op_name op in
      draw_rectangle
        (s (x + pad + 160))
        (s (y + 6))
        (s 120)
        (s (instr_h - 12))
        (Color.create 30 40 60 255);
      draw_text op_str
        (s (x + pad + 168))
        (s (y + pad))
        (s font_size_lg) col_accent;
      let pc_str = Printf.sprintf "pc = %d" pc in
      draw_text pc_str
        (s (x + pad + 300))
        (s (y + pad))
        (s font_size_sm) col_dim;
      let reg_writes = state.trace.Trace.Serializer.Read.vm.reg_writes in
      let last_write = ref None in
      Array.iter
        (fun (t, r, v) -> if t = tick then last_write := Some (r, v))
        reg_writes;
      match !last_write with
      | None -> ()
      | Some (r, v) ->
          let ws = Printf.sprintf "r%d <- %s" r (value_to_string v) in
          draw_text ws
            (s (x + pad + 460))
            (s (y + pad))
            (s font_size_sm) col_highlight)

let draw_scrubber state x y w =
  draw_panel x y w ctrl_h;
  let total = float_of_int (max 1 state.total_ticks) in
  let t = float_of_int state.pb.tick /. total in
  let btn_area = 180 in
  let spd_area = 130 in
  let track_x = x + pad + btn_area in
  let track_w = w - (pad * 2) - btn_area - spd_area in
  let track_y = y + (ctrl_h / 2) - 4 in
  draw_rectangle (s track_x) (s track_y) (s track_w) (s 8) col_scrubber;
  let filled_w = int_of_float (t *. float_of_int track_w) in
  draw_rectangle (s track_x) (s track_y) (s filled_w) (s 8) col_accent;
  let thumb_x = track_x + filled_w in
  draw_circle (s thumb_x) (s (track_y + 4)) (s 9 |> float_of_int) col_thumb;
  let btn_y = y + (ctrl_h / 2) - 11 in
  draw_text "|<" (s (x + pad)) (s btn_y) (s font_size_md) col_text;
  draw_text "<" (s (x + pad + 36)) (s btn_y) (s font_size_md) col_text;
  let play_str = if state.pb.playing then "||" else ">" in
  draw_text play_str (s (x + pad + 68)) (s btn_y) (s font_size_md) col_accent;
  draw_text ">" (s (x + pad + 96)) (s btn_y) (s font_size_md) col_text;
  draw_text ">|" (s (x + pad + 124)) (s btn_y) (s font_size_md) col_text;
  let spd_x = x + w - spd_area + pad in
  let speed_str = Printf.sprintf "%.1fx" state.pb.speed in
  draw_text speed_str (s spd_x) (s btn_y) (s font_size_md) col_yellow;
  let bar_w = spd_area - pad - 20 in
  let filled_spd = int_of_float (state.pb.speed /. 4.0 *. float_of_int bar_w) in
  draw_rectangle (s spd_x)
    (s (btn_y + font_size_md + 4))
    (s bar_w) (s 5) col_scrubber;
  draw_rectangle (s spd_x)
    (s (btn_y + font_size_md + 4))
    (s filled_spd) (s 5) col_yellow

let draw_topbar state =
  draw_rectangle 0 0 screen_w (s bar_h) col_panel;
  draw_rectangle 0 (s bar_h - 1) screen_w 1 col_border;
  draw_text "MAPVIZ" (s pad)
    (s ((bar_h / 2) - (font_size_lg / 2)))
    (s font_size_lg) col_accent;
  let tick_info =
    Printf.sprintf "tick %d / %d" state.pb.tick state.total_ticks
  in
  draw_text tick_info
    (s (pad + 120))
    (s ((bar_h / 2) - (font_size_sm / 2)))
    (s font_size_sm) col_dim;
  let btn_w = 108 in
  let btn_gap = 4 in
  let btn_y = (s bar_h - s 28) / 2 in
  let modes = [| "Registers"; "Heap"; "Both" |] in
  Array.iteri
    (fun i label ->
      let bx =
        screen_w - s (3 * (btn_w + btn_gap)) - s pad + (i * s (btn_w + btn_gap))
      in
      let active =
        match state.mode with
        | Registers -> i = 0
        | Heap -> i = 1
        | Both -> i = 2
      in
      draw_rectangle bx btn_y (s btn_w) (s 28)
        (if active then col_accent else col_scrubber);
      draw_text label
        (bx + s 8)
        (btn_y + s 5)
        (s font_size_sm)
        (if active then col_bg else col_text))
    modes

let handle_input state =
  let dt = get_frame_time () in
  if state.pb.playing then begin
    state.pb.accum <- state.pb.accum +. (dt *. state.pb.speed);
    while state.pb.accum >= 1.0 do
      if state.pb.tick < state.total_ticks - 1 then
        state.pb.tick <- state.pb.tick + 1
      else state.pb.playing <- false;
      state.pb.accum <- state.pb.accum -. 1.0
    done
  end;
  if state.hl.age > 0.0 then
    state.hl.age <- max 0.0 (state.hl.age -. (dt *. 2.0));
  if is_key_pressed Key.Space then state.pb.playing <- not state.pb.playing;
  if is_key_pressed Key.Right then
    if state.pb.tick < state.total_ticks - 1 then
      state.pb.tick <- state.pb.tick + 1;
  if is_key_pressed Key.Left then
    if state.pb.tick > 0 then state.pb.tick <- state.pb.tick - 1;
  if is_key_pressed Key.Home then state.pb.tick <- 0;
  if is_key_pressed Key.End then state.pb.tick <- state.total_ticks - 1;
  if is_key_pressed Key.Tab then
    state.mode <-
      (match state.mode with
      | Registers -> Heap
      | Heap -> Both
      | Both -> Registers);
  let wheel = get_mouse_wheel_move () in
  if wheel <> 0.0 then
    state.pb.speed <- max 0.1 (min 4.0 (state.pb.speed +. (wheel *. 0.1)));
  let mx = get_mouse_x () in
  let my = get_mouse_y () in
  let scrub_y = screen_h - s ctrl_h in
  let btn_area = 180 in
  let spd_area = 130 in
  let track_x = s (pad + btn_area) in
  let track_w = screen_w - s (pad * 2) - s btn_area - s spd_area in
  if
    is_mouse_button_down MouseButton.Left
    && my >= scrub_y
    && my <= scrub_y + s ctrl_h
    && mx >= track_x
    && mx <= track_x + track_w
  then begin
    let t = float_of_int (mx - track_x) /. float_of_int track_w in
    state.pb.tick <- int_of_float (t *. float_of_int state.total_ticks);
    state.pb.tick <- max 0 (min (state.total_ticks - 1) state.pb.tick);
    state.pb.playing <- false
  end;
  let btn_y = scrub_y + (s ctrl_h / 2) - s 11 in
  if
    is_mouse_button_pressed MouseButton.Left
    && my >= btn_y
    && my <= btn_y + s 22
  then begin
    if mx >= s pad && mx <= s (pad + 30) then state.pb.tick <- 0;
    if mx >= s (pad + 36) && mx <= s (pad + 60) then
      if state.pb.tick > 0 then state.pb.tick <- state.pb.tick - 1;
    if mx >= s (pad + 68) && mx <= s (pad + 90) then
      state.pb.playing <- not state.pb.playing;
    if mx >= s (pad + 96) && mx <= s (pad + 120) then
      if state.pb.tick < state.total_ticks - 1 then
        state.pb.tick <- state.pb.tick + 1;
    if mx >= s (pad + 124) && mx <= s (pad + 160) then
      state.pb.tick <- state.total_ticks - 1
  end;
  if is_mouse_button_pressed MouseButton.Left && my >= 0 && my <= s bar_h then begin
    let btn_w = 108 in
    let btn_gap = 4 in
    let modes_x = screen_w - s (3 * (btn_w + btn_gap)) - s pad in
    if mx >= modes_x && mx <= modes_x + s btn_w then state.mode <- Registers;
    if
      mx >= modes_x + s (btn_w + btn_gap)
      && mx <= modes_x + s (2 * (btn_w + btn_gap))
    then state.mode <- Heap;
    if
      mx >= modes_x + s (2 * (btn_w + btn_gap))
      && mx <= modes_x + s (3 * (btn_w + btn_gap))
    then state.mode <- Both
  end

let draw state =
  begin_drawing ();
  clear_background col_bg;
  draw_topbar state;
  let content_y = bar_h in
  let content_h = sh - bar_h - instr_h - ctrl_h in
  let instr_y = content_y + content_h in
  let scrub_y = instr_y + instr_h in
  (match state.mode with
  | Registers ->
      draw_registers state 0 content_y sw content_h;
      draw_gc_overlay state 0 content_y sw
  | Heap ->
      draw_heap state 0 content_y sw content_h;
      draw_gc_overlay state 0 content_y sw
  | Both ->
      let half = sw / 2 in
      draw_registers state 0 content_y half content_h;
      draw_heap state half content_y (sw - half) content_h;
      draw_gc_overlay state 0 content_y sw);
  draw_instr_bar state 0 instr_y sw;
  draw_scrubber state 0 scrub_y sw;
  end_drawing ()

let run path =
  let trace = Trace.Serializer.deserialize_from_file path in
  let total_ticks = Array.length trace.Trace.Serializer.Read.vm.instrs in
  let state =
    {
      trace;
      pb = { tick = 0; playing = false; speed = 1.0; accum = 0.0 };
      hl = { reg = None; addr = None; age = 0.0 };
      mode = Both;
      show_settings = false;
      total_ticks;
    }
  in
  init_window screen_w screen_h "MAPVIZ -- MAP Virtual Machine Inspector";
  set_target_fps 60;
  while not (window_should_close ()) do
    handle_input state;
    draw state
  done;
  close_window ()
