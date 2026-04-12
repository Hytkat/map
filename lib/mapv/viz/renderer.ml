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

type inspector = { mutable active : bool; mutable addr : int }
type scroll = { mutable offset : int }

type state = {
  trace : Trace.Serializer.Read.t;
  pb : playback;
  hl : highlight;
  insp : inspector;
  reg_scroll : scroll;
  stack_scroll : scroll;
  heap_scroll : scroll;
  con_scroll : scroll;
  mutable mode : view_mode;
  total_ticks : int;
}

let screen_w = 1920
let screen_h = 1080
let scale = 1.75
let sw = int_of_float (float_of_int screen_w /. scale)
let sh = int_of_float (float_of_int screen_h /. scale)
let bar_h = 36
let instr_h = 52
let event_h = 22
let ctrl_h = 52
let pad = 12
let row_h = 22
let fxs = 11
let fsm = 14
let fmd = 18
let flg = 22
let s x = int_of_float (float_of_int x *. scale)
let c r g b = Color.create r g b 255
let col_bg = c 11 12 17
let col_panel = c 16 18 26
let col_panel2 = c 20 22 32
let col_border = c 38 42 58
let col_border2 = c 55 60 82
let col_text = c 210 212 228
let col_dim = c 82 88 112
let col_dim2 = c 46 50 68
let col_accent = c 90 170 255
let col_green = c 68 210 110
let col_red = c 220 72 72
let col_yellow = c 230 200 60
let col_orange = c 240 150 55
let col_purple = c 170 120 255
let col_teal = c 72 205 195
let col_young = c 55 120 210
let col_old = c 160 72 205
let col_recent = c 255 210 70
let col_scrubber = c 30 34 48
let col_thumb = c 90 170 255

let lerp_color a b t =
  let f x y =
    int_of_float (float_of_int x +. ((float_of_int y -. float_of_int x) *. t))
  in
  c
    (f (Color.r a) (Color.r b))
    (f (Color.g a) (Color.g b))
    (f (Color.b a) (Color.b b))

let clamp_str str n =
  if String.length str <= n then str else String.sub str 0 (n - 1) ^ "…"

let draw_txt x y sz col str = draw_text str (s x) (s y) (s sz) col
let draw_rect x y w h col = draw_rectangle (s x) (s y) (s w) (s h) col
let draw_rect_l x y w h col = draw_rectangle_lines (s x) (s y) (s w) (s h) col
let draw_circ x y r col = draw_circle (s x) (s y) (float_of_int (s r)) col

let panel x y w h =
  draw_rect x y w h col_panel;
  draw_rect_l x y w h col_border

let panel2 x y w h =
  draw_rect x y w h col_panel2;
  draw_rect_l x y w h col_border2

let section_hdr x y w lbl =
  draw_txt x y fxs col_dim lbl;
  draw_rect x (y + fxs + 3) w 1 col_border2

let op_name = function
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
  | op -> Printf.sprintf "op_%02X" op

let op_col = function
  | 0x00 | 0x01 -> col_dim
  | op when op >= 0x02 && op <= 0x08 -> col_accent
  | op when op >= 0x10 && op <= 0x1B -> col_green
  | op when op >= 0x20 && op <= 0x28 -> col_purple
  | op when op >= 0x30 && op <= 0x39 -> col_teal
  | 0x40 | 0x41 | 0x42 -> col_orange
  | op when op >= 0x50 && op <= 0x54 -> col_old
  | op when op >= 0x60 && op <= 0x62 -> c 205 110 110
  | op when op >= 0x70 && op <= 0x74 -> col_green
  | op when op >= 0x80 && op <= 0x82 -> col_red
  | op when op >= 0x90 && op <= 0x93 -> col_teal
  | _ -> col_dim

let value_str = function
  | Value.Nil -> "nil"
  | Value.Bool b -> if b then "true" else "false"
  | Value.Int n -> string_of_int n
  | Value.Float f -> Printf.sprintf "%.5g" f
  | Value.Ptr p -> Printf.sprintf "ptr(%d)" p
  | Value.NativeFun _ -> "<native-fn>"
  | Value.NativePtr _ -> "<native-ptr>"

let value_col = function
  | Value.Nil -> col_dim
  | Value.Bool _ -> col_teal
  | Value.Int _ -> col_text
  | Value.Float _ -> col_orange
  | Value.Ptr _ -> col_accent
  | Value.NativeFun _ | Value.NativePtr _ -> col_purple

let tvm s = s.trace.Trace.Serializer.Read.vm
let tgc s = s.trace.Trace.Serializer.Read.gc
let thp s = s.trace.Trace.Serializer.Read.heap

let reg_value_at state tick reg =
  let last = ref None in
  Array.iter
    (fun (t, r, v) -> if t <= tick && r = reg then last := Some v)
    (tvm state).reg_writes;
  !last

let reg_last_tick state tick reg =
  let lt = ref (-1) in
  Array.iter
    (fun (t, r, _) -> if t <= tick && r = reg then lt := t)
    (tvm state).reg_writes;
  !lt

let active_regs state tick =
  let tbl = Hashtbl.create 64 in
  Array.iter
    (fun (t, r, _) -> if t <= tick then Hashtbl.replace tbl r ())
    (tvm state).reg_writes;
  tbl

let current_instr state tick =
  let last = ref None in
  Array.iter
    (fun (t, pc, op) -> if t <= tick then last := Some (t, pc, op))
    (tvm state).instrs;
  !last

let writes_at_tick state tick =
  Array.to_list (tvm state).reg_writes
  |> List.filter (fun (t, _, _) -> t = tick)

let calls_at state tick =
  Array.to_list (tvm state).calls |> List.filter (fun (t, _, _) -> t <= tick)

let rets_at state tick =
  Array.to_list (tvm state).rets |> List.filter (fun (t, _) -> t <= tick)

let allocs_at state tick =
  Array.to_list (thp state).allocs
  |> List.filter (fun (t, _, _, _) -> t <= tick)

let frees_at state tick =
  Array.to_list (thp state).frees
  |> List.filter (fun (t, _) -> t <= tick)
  |> List.map snd

let promotes_at state tick =
  Array.to_list (thp state).promotes
  |> List.filter (fun (t, _) -> t <= tick)
  |> List.map snd

let gc_events_at state tick =
  Array.to_list (tgc state).events |> List.filter (fun (t, _) -> t <= tick)

let gc_minor_count state tick =
  List.fold_left
    (fun a (_, ev) -> match ev with Heap.Minor_end _ -> a + 1 | _ -> a)
    0 (gc_events_at state tick)

let gc_major_count state tick =
  List.fold_left
    (fun a (_, ev) -> match ev with Heap.Major_end -> a + 1 | _ -> a)
    0 (gc_events_at state tick)

let gc_promoted_total state tick =
  List.fold_left
    (fun a (_, ev) ->
      match ev with Heap.Minor_end { promoted } -> a + promoted | _ -> a)
    0 (gc_events_at state tick)

let gc_freed_total state tick =
  List.fold_left
    (fun a (_, ev) ->
      match ev with Heap.Major_sweep { freed; _ } -> a + freed | _ -> a)
    0 (gc_events_at state tick)

let in_gc state tick =
  let r = ref false in
  Array.iter
    (fun (t, ev) ->
      if t <= tick then
        match ev with
        | Heap.Minor_start | Heap.Major_mark _ -> r := true
        | Heap.Minor_end _ | Heap.Major_end -> r := false
        | _ -> ())
    (tgc state).events;
  !r

let gen_split state tick =
  let allocs = allocs_at state tick in
  let promoted = promotes_at state tick in
  let old_set = Hashtbl.create 16 in
  List.iter (fun addr -> Hashtbl.replace old_set addr ()) promoted;
  let young =
    List.filter (fun (_, a, _, _) -> not (Hashtbl.mem old_set a)) allocs
  in
  let old = List.filter (fun (_, a, _, _) -> Hashtbl.mem old_set a) allocs in
  (young, old)

let continuations_at state tick =
  let con_news =
    Array.to_list (tvm state).con_news |> List.filter (fun (t, _) -> t <= tick)
  in
  let n_cons = List.length con_news in
  Array.to_list
    (Array.init n_cons (fun idx ->
         let birth_tick, _birth_pc = List.nth con_news idx in
         let yields =
           Array.to_list (tvm state).con_yields
           |> List.filter (fun (t, cid, _) -> t <= tick && cid = idx)
         in
         let resumes =
           Array.to_list (tvm state).con_resumes
           |> List.filter (fun (t, cid, _) -> t <= tick && cid = idx)
         in
         let last_yield =
           List.fold_left (fun a (t, _, _) -> max a t) (-1) yields
         in
         let last_resume =
           List.fold_left (fun a (t, _, _) -> max a t) (-1) resumes
         in
         let status =
           if last_resume > last_yield && last_resume >= 0 then `Running
           else if last_yield >= 0 then `Suspended
           else `New
         in
         (idx, birth_tick, status, List.length yields, List.length resumes)))

let call_depth state tick =
  let calls = calls_at state tick in
  let rets = rets_at state tick in
  max 0 (List.length calls - List.length rets)

let active_call_frames state tick =
  let calls = calls_at state tick in
  let depth = call_depth state tick in
  let n = List.length calls in
  let start = max 0 (n - depth) in
  let arr = Array.of_list calls in
  Array.to_list (Array.sub arr start (n - start))

let fill_bar x y w h used total col_fill =
  draw_rect x y w h col_scrubber;
  let pct =
    if total <= 0 then 0.0
    else Float.min 1.0 (float_of_int used /. float_of_int total)
  in
  let fw = int_of_float (pct *. float_of_int w) in
  if fw > 0 then draw_rect x y fw h col_fill;
  pct

let rows_visible body_y body_h = (body_y, body_y + body_h)

let draw_registers state x y w h =
  panel x y w h;
  let active = active_regs state state.pb.tick in
  let sparse =
    let acc = ref [] in
    for i = 255 downto 0 do
      if Hashtbl.mem active i then acc := i :: !acc
    done;
    !acc
  in
  let n_live = List.length sparse in
  let hdr_h = pad + fmd + 6 in
  draw_txt (x + pad) (y + pad) fmd col_accent "REGISTERS";
  draw_txt
    (x + w - pad - 80)
    (y + pad + 2)
    fxs col_dim
    (Printf.sprintf "%d live" n_live);
  draw_rect x (y + hdr_h) w 1 col_border;
  let body_y = y + hdr_h + 1 in
  let body_h = h - hdr_h - 1 in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_live = 0 then
    draw_txt
      (x + pad + 6)
      (body_y + 6) fsm col_dim2 "(no registers written yet)"
  else begin
    let visible_start = state.reg_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let slice = Array.of_list sparse in
    for i = visible_start to min (n_live - 1) (visible_start + max_visible) do
      let reg = slice.(i) in
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let lt = reg_last_tick state state.pb.tick reg in
        let age = state.pb.tick - lt in
        let recent = lt >= 0 && age <= 6 in
        let is_hl = state.hl.reg = Some reg in
        let bg =
          if is_hl then lerp_color col_accent col_panel (1.0 -. state.hl.age)
          else if recent then
            lerp_color (c 58 48 16) col_panel (float_of_int age /. 6.0)
          else c 20 22 32
        in
        draw_rect (x + 1) ry (w - 2) (row_h - 1) bg;
        draw_rect (x + 1) ry 3 (row_h - 1)
          (if recent then col_recent else col_dim2);
        draw_txt (x + 7) (ry + 4) fxs
          (if recent then col_recent else col_dim)
          (Printf.sprintf "r%-3d" reg);
        let v =
          Option.value (reg_value_at state state.pb.tick reg) ~default:Value.Nil
        in
        let vcol = if recent then col_recent else value_col v in
        draw_txt (x + 54) (ry + 4) fxs vcol (clamp_str (value_str v) 28);
        let age_str = if lt < 0 then "" else Printf.sprintf "+%d" age in
        draw_txt (x + w - pad - 28) (ry + 4) fxs col_dim2 age_str
      end
    done
  end

let draw_call_stack state x y w h =
  draw_rect x y w h (c 14 16 22);
  draw_rect_l x y w h col_border;
  let hdr_h = pad + fxs + 6 in
  section_hdr (x + pad) (y + pad) (w - (pad * 2)) "CALL STACK";
  draw_rect x (y + hdr_h) w 1 col_border;
  let frames = active_call_frames state state.pb.tick in
  let n_frames = List.length frames in
  let depth = call_depth state state.pb.tick in
  draw_txt (x + w - pad - 28) (y + pad) fxs col_dim (string_of_int depth);
  let body_y = y + hdr_h + 1 in
  let body_h = h - hdr_h - 1 in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_frames = 0 then
    draw_txt (x + pad + 4) (body_y + 6) fsm col_dim2 "(empty)"
  else begin
    let visible_start = state.stack_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let arr = Array.of_list frames in
    for i = visible_start to min (n_frames - 1) (visible_start + max_visible) do
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let _, src, dst = arr.(i) in
        let is_top = i = n_frames - 1 in
        let bg = if is_top then col_accent else c 26 30 44 in
        draw_rect (x + 1) ry (w - 2) (row_h - 1) bg;
        let tc = if is_top then col_bg else col_dim in
        let vc = if is_top then col_bg else col_text in
        draw_txt (x + 6) (ry + 4) fxs tc (Printf.sprintf "#%d" i);
        draw_txt (x + 28) (ry + 4) fxs vc
          (Printf.sprintf "pc %-5d  ->r%-5d" src dst)
      end
    done
  end

let draw_continuations state x y w h =
  draw_rect x y w h (c 14 16 22);
  draw_rect_l x y w h col_border;
  let hdr_h = pad + fxs + 6 in
  let cons = continuations_at state state.pb.tick in
  let n_cons = List.length cons in
  section_hdr (x + pad) (y + pad) (w - (pad * 2)) "CONTINUATIONS";
  draw_txt (x + w - pad - 28) (y + pad) fxs col_dim (string_of_int n_cons);
  draw_rect x (y + hdr_h) w 1 col_border;
  let body_y = y + hdr_h + 1 in
  let body_h = h - hdr_h - 1 in
  let clip0, clip1 = rows_visible body_y body_h in
  if n_cons = 0 then draw_txt (x + pad + 4) (body_y + 6) fsm col_dim2 "(none)"
  else begin
    let visible_start = state.con_scroll.offset in
    let max_visible = (body_h / row_h) + 2 in
    let arr = Array.of_list cons in
    for i = visible_start to min (n_cons - 1) (visible_start + max_visible) do
      let ry = body_y + ((i - visible_start) * row_h) in
      if ry + row_h > clip0 && ry < clip1 then begin
        let idx, birth, status, yields, resumes = arr.(i) in
        let sc, ss =
          match status with
          | `Running -> (col_green, "running")
          | `Suspended -> (col_yellow, "suspended")
          | `New -> (col_dim, "new")
        in
        let is_running = status = `Running in
        let bg = if is_running then c 14 26 18 else c 20 22 32 in
        draw_rect (x + 1) ry (w - 2) (row_h - 1) bg;
        draw_rect (x + 1) ry 3 (row_h - 1) sc;
        draw_circ (x + 10) (ry + (row_h / 2) - 1) 3 sc;
        draw_txt (x + 18) (ry + 4) fxs col_text (Printf.sprintf "con_%d" idx);
        draw_txt (x + 52) (ry + 4) fxs col_dim (Printf.sprintf "t=%d" birth);
        draw_txt (x + 90) (ry + 4) fxs sc ss;
        draw_txt
          (x + w - pad - 60)
          (ry + 4) fxs col_dim2
          (Printf.sprintf "y%d r%d" yields resumes)
      end
    done
  end

let draw_heap state x y w h =
  panel x y w h;
  let iw = w - (pad * 2) in
  let cy = ref (y + pad) in
  draw_txt (x + pad) !cy fmd col_accent "HEAP";
  cy := !cy + fmd + 8;
  draw_rect (x + pad) !cy iw 1 col_border;
  cy := !cy + 6;
  let freed_addrs = frees_at state state.pb.tick in
  let young, old = gen_split state state.pb.tick in
  let n_young = List.length young in
  let n_old = List.length old in
  let n_freed = List.length freed_addrs in
  let n_total = n_young + n_old + n_freed in
  let b_young = List.fold_left (fun a (_, _, sz, _) -> a + sz) 0 young in
  let b_old = List.fold_left (fun a (_, _, sz, _) -> a + sz) 0 old in
  section_hdr (x + pad) !cy iw "MEMORY";
  cy := !cy + fxs + 8;
  let bar_row lbl n b col_fill =
    draw_txt (x + pad) !cy fxs col_dim lbl;
    let stats_str = Printf.sprintf "%d obj | %dw" n b in
    draw_txt (x + iw - pad - 100) !cy fxs col_dim stats_str;
    cy := !cy + fxs + 4;
    let bar_w = iw - pad - 60 in
    let pct = fill_bar (x + pad) !cy bar_w 10 n (max 1 n_total) col_fill in
    let pct_str = Printf.sprintf "%3d%%" (int_of_float (pct *. 100.0)) in
    draw_txt (x + pad + bar_w + 8) (!cy - 1) fxs col_dim pct_str;
    cy := !cy + 24
  in
  bar_row "young" n_young b_young col_young;
  bar_row "old" n_old b_old col_old;
  bar_row "freed" n_freed 0 col_dim2;
  cy := !cy + 4;
  let sw4 = iw / 4 in
  let stats =
    [|
      ("total", Printf.sprintf "%dw" (b_young + b_old), col_text);
      ("live", string_of_int (n_young + n_old), col_green);
      ( "promo",
        string_of_int (List.length (promotes_at state state.pb.tick)),
        col_purple );
      ( "frag",
        (if n_total = 0 then "n/a"
         else
           Printf.sprintf "%.0f%%"
             (100.0 *. float_of_int n_freed /. float_of_int n_total)),
        col_yellow );
    |]
  in
  Array.iteri
    (fun i (k, v, vc) ->
      let sx = x + pad + (i * sw4) in
      draw_txt sx !cy fxs col_dim k;
      draw_txt sx (!cy + fxs + 3) fsm vc v)
    stats;
  cy := !cy + fxs + fsm + 10;
  draw_rect (x + pad) !cy iw 1 col_border;
  cy := !cy + 6;
  section_hdr (x + pad) !cy iw "GC";
  cy := !cy + fxs + 8;
  if in_gc state state.pb.tick then begin
    draw_rect (x + pad) !cy iw (fsm + 6) (c 50 18 18);
    draw_txt (x + pad + 6) (!cy + 3) fsm col_red "GC IN PROGRESS";
    cy := !cy + fsm + 10
  end;
  let gc_rows =
    [|
      ("minor runs", gc_minor_count state state.pb.tick, col_young);
      ("major runs", gc_major_count state state.pb.tick, col_old);
      ("objs promo", gc_promoted_total state state.pb.tick, col_purple);
      ("objs swept", gc_freed_total state state.pb.tick, col_dim);
    |]
  in
  let col2 = iw / 2 in
  Array.iteri
    (fun i (k, v, vc) ->
      let sx = x + pad + (i mod 2 * col2) in
      let sy = !cy + (i / 2 * (fxs + fsm + 8)) in
      draw_txt sx sy fxs col_dim k;
      draw_txt sx (sy + fxs + 3) fsm vc (string_of_int v))
    gc_rows;
  cy := !cy + (2 * (fxs + fsm + 8)) + 6;
  draw_rect (x + pad) !cy iw 1 col_border;
  cy := !cy + 6;
  section_hdr (x + pad) !cy iw "ALLOCATION MAP";
  cy := !cy + fxs + 8;
  let cell = 12 and gap = 2 in
  let cols_n = iw / (cell + gap) in
  let freed_set = Hashtbl.create 16 in
  List.iter (fun a -> Hashtbl.replace freed_set a ()) freed_addrs;
  let promoted_set = Hashtbl.create 16 in
  List.iter
    (fun a -> Hashtbl.replace promoted_set a ())
    (promotes_at state state.pb.tick);
  let draw_cells cells max_rows =
    List.iteri
      (fun i (_, addr, size, _) ->
        let row = i / cols_n and col2 = i mod cols_n in
        if row < max_rows then begin
          let cx = x + pad + (col2 * (cell + gap)) in
          let cy2 = !cy + (row * (cell + gap)) in
          let is_freed = Hashtbl.mem freed_set addr in
          let is_old = Hashtbl.mem promoted_set addr && not is_freed in
          let bc =
            if is_freed then c 20 22 32
            else if is_old then col_old
            else col_young
          in
          draw_rect cx cy2 cell cell bc;
          if size > 1 && not is_freed then
            draw_rect (cx + 3) (cy2 + 3) (cell - 6) (cell - 6)
              (lerp_color bc col_bg 0.4)
        end)
      cells
  in
  let rows_y = min 3 (max 1 ((n_young + cols_n - 1) / cols_n)) in
  let rows_o = min 3 (max 1 ((n_old + cols_n - 1) / cols_n)) in
  draw_cells young rows_y;
  cy := !cy + (rows_y * (cell + gap)) + 3;
  draw_cells old rows_o;
  cy := !cy + (rows_o * (cell + gap)) + 8

let draw_inspector state x y w h =
  if not state.insp.active then ()
  else begin
    panel2 x y w h;
    let cy = ref (y + pad) in
    draw_txt (x + pad) !cy fmd col_accent "INSPECT";
    cy := !cy + fmd + 6;
    let addr = state.insp.addr in
    draw_txt (x + pad) !cy fxs col_dim (Printf.sprintf "heap addr  %d" addr);
    cy := !cy + fxs + 8;
    draw_rect (x + pad) !cy (w - (pad * 2)) 1 col_border;
    cy := !cy + 6;
    let all = allocs_at state state.pb.tick in
    let freed = frees_at state state.pb.tick in
    let proms = promotes_at state state.pb.tick in
    match List.find_opt (fun (_, a, _, _) -> a = addr) all with
    | None -> draw_txt (x + pad) !cy fsm col_dim2 "(not yet allocated)"
    | Some (_, _, size, tag) ->
        let is_freed = List.mem addr freed in
        let is_old = List.mem addr proms in
        let kv lbl v vc =
          draw_txt (x + pad) !cy fxs col_dim lbl;
          draw_txt (x + pad + 64) !cy fxs vc v;
          cy := !cy + fxs + 5
        in
        kv "tag" (string_of_int tag) col_yellow;
        kv "size" (Printf.sprintf "%d words" size) col_text;
        kv "gen"
          (if is_old then "old" else "young")
          (if is_old then col_old else col_young);
        kv "freed"
          (if is_freed then "yes" else "no")
          (if is_freed then col_red else col_green);
        cy := !cy + 4;
        draw_rect (x + pad) !cy (w - (pad * 2)) 1 col_border;
        cy := !cy + 6;
        section_hdr (x + pad) !cy (w - (pad * 2)) "FIELDS";
        cy := !cy + fxs + 8;
        let writes = (thp state).writes in
        for field = 0 to size - 1 do
          let last_v = ref Value.Nil in
          Array.iter
            (fun (t, a, f, v) ->
              if t <= state.pb.tick && a = addr && f = field then last_v := v)
            writes;
          draw_txt (x + pad) !cy fxs col_dim (Printf.sprintf "[%d]" field);
          draw_txt
            (x + pad + 36)
            !cy fxs (value_col !last_v) (value_str !last_v);
          cy := !cy + fxs + 4
        done
  end

let draw_event_track state x y w =
  draw_rect x y w event_h col_panel;
  draw_rect x (y + event_h - 1) w 1 col_border;
  let total = float_of_int (max 1 state.total_ticks) in
  let tx t = x + int_of_float (float_of_int t /. total *. float_of_int w) in
  Array.iter
    (fun (t, ev) ->
      let ec, eh =
        match ev with
        | Heap.Minor_start -> (col_young, event_h / 2)
        | Heap.Minor_end _ -> (col_accent, event_h / 2)
        | Heap.Major_mark _ -> (col_old, event_h)
        | Heap.Major_sweep _ -> (col_purple, event_h)
        | Heap.Major_end -> (col_red, event_h)
      in
      draw_rect (tx t) (y + event_h - eh) 2 eh ec)
    (tgc state).events;
  Array.iter
    (fun (t, _, _) -> draw_rect (tx t) y 1 (event_h / 3) col_green)
    (tvm state).calls;
  Array.iter
    (fun (t, _, _) -> draw_rect (tx t) y 1 (event_h / 3) col_teal)
    (tvm state).con_yields;
  draw_rect (tx state.pb.tick) y 2 event_h col_recent

let draw_instr_bar state x y w =
  panel x y w instr_h;
  let mid = y + (instr_h / 2) in
  match current_instr state state.pb.tick with
  | None -> draw_txt (x + pad) (mid - (fsm / 2)) fsm col_dim "(no instruction)"
  | Some (tick, pc, op) ->
      draw_txt (x + pad)
        (mid - fxs - 2)
        fxs col_dim
        (Printf.sprintf "t=%05d / %05d" tick state.total_ticks);
      draw_txt (x + pad) (mid + 2) fxs col_dim (Printf.sprintf "pc=%-6d" pc);
      let cat_col = op_col op in
      let op_str = op_name op in
      let badge_x = x + pad + 118 in
      let mid_y = y + (instr_h / 2) in
      let bh = instr_h - 16 in
      draw_rect badge_x (mid_y - (bh / 2)) 3 bh cat_col;
      draw_txt (badge_x + 8) (mid_y - (flg / 2)) flg cat_col op_str;
      let writes = writes_at_tick state tick in
      let wx = ref (badge_x + 140) in
      List.iter
        (fun (_, r, v) ->
          if !wx + 100 < x + w - pad then begin
            let y_pos = mid - (fxs / 2) in
            draw_txt !wx y_pos fxs col_dim (Printf.sprintf "r%d" r);
            draw_txt (!wx + 18) y_pos fxs col_dim2 "<-";
            draw_txt (!wx + 36) y_pos fxs (value_col v)
              (clamp_str (value_str v) 10);
            wx := !wx + 108
          end)
        writes

let draw_scrubber state x y w =
  panel x y w ctrl_h;
  let total = float_of_int (max 1 state.total_ticks) in
  let t = float_of_int state.pb.tick /. total in
  let btn_w = 160 and spd_w = 110 in
  let track_x = x + pad + btn_w in
  let track_w = w - (pad * 2) - btn_w - spd_w in
  let track_y = y + (ctrl_h / 2) - 4 in
  draw_rect track_x track_y track_w 8 col_scrubber;
  let fw = int_of_float (t *. float_of_int track_w) in
  draw_rect track_x track_y fw 8 col_accent;
  draw_circ (track_x + fw) (track_y + 4) 7 col_thumb;
  let by = y + (ctrl_h / 2) - (fmd / 2) in
  let play_lbl = if state.pb.playing then "||" else " >" in
  let btns =
    [| ("|<", 0); (" <", 30); (play_lbl, 58); (" >", 88); (">|", 116) |]
  in
  Array.iter
    (fun (lbl, bx) ->
      let col = if lbl = play_lbl then col_accent else col_text in
      draw_txt (x + pad + bx) by fmd col lbl)
    btns;
  let sx = x + w - spd_w + pad in
  draw_txt sx by fmd col_yellow (Printf.sprintf "%.1fx" state.pb.speed);
  let bw = spd_w - pad - 16 in
  let fs = int_of_float (state.pb.speed /. 4.0 *. float_of_int bw) in
  draw_rect sx (by + fmd + 4) bw 4 col_scrubber;
  draw_rect sx (by + fmd + 4) fs 4 col_yellow

let draw_topbar state =
  draw_rect 0 0 sw bar_h col_panel;
  draw_rect 0 (bar_h - 1) sw 1 col_border;
  draw_txt pad ((bar_h / 2) - (flg / 2)) flg col_accent "Map.Viz";
  draw_txt (pad + 90)
    ((bar_h / 2) - (fxs / 2))
    fxs col_dim
    (Printf.sprintf "tick %d / %d" state.pb.tick state.total_ticks);
  let depth = call_depth state state.pb.tick in
  draw_txt (pad + 210)
    ((bar_h / 2) - (fxs / 2))
    fxs col_dim
    (Printf.sprintf "depth %d" depth);
  if in_gc state state.pb.tick then begin
    draw_rect (pad + 290)
      ((bar_h / 2) - ((fxs + 4) / 2))
      60 (fxs + 4) (c 60 10 10);
    draw_txt (pad + 294) ((bar_h / 2) - (fxs / 2)) fxs col_red "GC"
  end;
  if state.insp.active then begin
    draw_rect (pad + 360)
      ((bar_h / 2) - ((fsm + 4) / 2))
      180 (fsm + 4) (c 18 28 50);
    draw_txt (pad + 366)
      ((bar_h / 2) - (fsm / 2))
      fsm col_accent
      (Printf.sprintf "insp ptr(%d)" state.insp.addr)
  end;
  let btn_w = 90 and btn_gap = 3 in
  let modes = [| ("Regs", Registers); ("Heap", Heap); ("Both", Both) |] in
  Array.iteri
    (fun i (lbl, mode) ->
      let bx = sw - (3 * (btn_w + btn_gap)) - pad + (i * (btn_w + btn_gap)) in
      let active = state.mode = mode in
      draw_rect bx
        ((bar_h / 2) - 12)
        btn_w 24
        (if active then col_accent else col_scrubber);
      draw_rect_l bx ((bar_h / 2) - 12) btn_w 24 col_border;
      draw_txt (bx + 6)
        ((bar_h / 2) - (fsm / 2))
        fsm
        (if active then col_bg else col_text)
        lbl)
    modes

let content_y () = bar_h
let content_h () = sh - bar_h - instr_h - event_h - ctrl_h
let instr_y () = content_y () + content_h ()
let event_y () = instr_y () + instr_h
let scrub_y () = event_y () + event_h
let reg_panel_w = 280
let stack_h = 140
let con_h = 120
let insp_w = 210

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
  if is_key_pressed Key.Right && state.pb.tick < state.total_ticks - 1 then
    state.pb.tick <- state.pb.tick + 1;
  if is_key_pressed Key.Left && state.pb.tick > 0 then
    state.pb.tick <- state.pb.tick - 1;
  if is_key_pressed Key.Home then state.pb.tick <- 0;
  if is_key_pressed Key.End then state.pb.tick <- state.total_ticks - 1;
  if is_key_pressed Key.Tab then
    state.mode <-
      (match state.mode with
      | Registers -> Heap
      | Heap -> Both
      | Both -> Registers);
  if is_key_pressed Key.Escape then state.insp.active <- false;
  let mx = int_of_float (float_of_int (get_mouse_x ()) /. scale) in
  let my = int_of_float (float_of_int (get_mouse_y ()) /. scale) in
  let wheel = get_mouse_wheel_move () in
  let cy = content_y () and ch = content_h () in
  let reg_h = ch - stack_h - con_h in
  let reg_right =
    match state.mode with Registers -> sw | Both -> reg_panel_w | Heap -> 0
  in
  let in_reg_body = mx >= 0 && mx < reg_right && my >= cy && my < cy + reg_h in
  let in_stack_body =
    mx >= 0 && mx < reg_right && my >= cy + reg_h && my < cy + reg_h + stack_h
  in
  let in_con_body =
    mx >= 0 && mx < reg_right && my >= cy + reg_h + stack_h && my < cy + ch
  in
  let in_heap =
    match state.mode with
    | Heap -> mx >= 0 && mx < sw && my >= cy && my < cy + ch
    | Both -> mx >= reg_panel_w && mx < sw && my >= cy && my < cy + ch
    | _ -> false
  in
  if wheel <> 0.0 then begin
    let d = -int_of_float wheel in
    if in_reg_body then
      state.reg_scroll.offset <- max 0 (state.reg_scroll.offset + d)
    else if in_stack_body then
      state.stack_scroll.offset <- max 0 (state.stack_scroll.offset + d)
    else if in_con_body then
      state.con_scroll.offset <- max 0 (state.con_scroll.offset + d)
    else if in_heap then
      state.heap_scroll.offset <- max 0 (state.heap_scroll.offset + d)
    else state.pb.speed <- max 0.1 (min 4.0 (state.pb.speed +. (wheel *. 0.1)))
  end;
  let sy = scrub_y () in
  let btn_w = 160 and spd_w = 110 in
  let track_x = pad + btn_w in
  let track_w = sw - (pad * 2) - btn_w - spd_w in
  if
    is_mouse_button_down MouseButton.Left
    && my >= sy
    && my <= sy + ctrl_h
    && mx >= track_x
    && mx <= track_x + track_w
  then begin
    let t = float_of_int (mx - track_x) /. float_of_int track_w in
    state.pb.tick <-
      max 0
        (min (state.total_ticks - 1)
           (int_of_float (t *. float_of_int state.total_ticks)));
    state.pb.playing <- false
  end;
  let by = sy + (ctrl_h / 2) - (fmd / 2) in
  if is_mouse_button_pressed MouseButton.Left && my >= by && my <= by + fmd then begin
    if mx >= pad && mx < pad + 28 then state.pb.tick <- 0;
    if mx >= pad + 30 && mx < pad + 56 then
      if state.pb.tick > 0 then state.pb.tick <- state.pb.tick - 1;
    if mx >= pad + 58 && mx < pad + 84 then
      state.pb.playing <- not state.pb.playing;
    if mx >= pad + 88 && mx < pad + 114 then
      if state.pb.tick < state.total_ticks - 1 then
        state.pb.tick <- state.pb.tick + 1;
    if mx >= pad + 116 && mx < pad + 152 then
      state.pb.tick <- state.total_ticks - 1
  end;
  let btn_w2 = 90 and btn_gap = 3 in
  let bx0 = sw - (3 * (btn_w2 + btn_gap)) - pad in
  if is_mouse_button_pressed MouseButton.Left && my >= 0 && my <= bar_h then begin
    if mx >= bx0 && mx < bx0 + btn_w2 then state.mode <- Registers;
    if mx >= bx0 + btn_w2 + btn_gap && mx < bx0 + (2 * (btn_w2 + btn_gap)) then
      state.mode <- Heap;
    if
      mx >= bx0 + (2 * (btn_w2 + btn_gap)) && mx < bx0 + (3 * (btn_w2 + btn_gap))
    then state.mode <- Both
  end;
  if is_mouse_button_pressed MouseButton.Right && in_reg_body then begin
    let active = active_regs state state.pb.tick in
    let sparse =
      let acc = ref [] in
      for i = 255 downto 0 do
        if Hashtbl.mem active i then acc := i :: !acc
      done;
      !acc
    in
    let hdr_h = pad + fmd + 6 in
    let body_y = cy + hdr_h + 1 in
    let row = (my - body_y) / row_h in
    let idx = state.reg_scroll.offset + row in
    if idx >= 0 && idx < List.length sparse then begin
      let reg = List.nth sparse idx in
      match reg_value_at state state.pb.tick reg with
      | Some (Value.Ptr addr) ->
          state.insp.active <- true;
          state.insp.addr <- addr
      | _ -> state.insp.active <- false
    end
  end;
  let ey = event_y () in
  if is_mouse_button_pressed MouseButton.Left && my >= ey && my < ey + event_h
  then begin
    let t = float_of_int mx /. float_of_int sw in
    state.pb.tick <-
      max 0
        (min (state.total_ticks - 1)
           (int_of_float (t *. float_of_int state.total_ticks)))
  end

let draw state =
  begin_drawing ();
  clear_background col_bg;
  draw_topbar state;
  let cy = content_y () in
  let ch = content_h () in
  let iy = instr_y () in
  let ey = event_y () in
  let sby = scrub_y () in
  let reg_h = ch - stack_h - con_h in
  if in_gc state state.pb.tick then draw_rect 0 0 sw 2 col_red;
  (match state.mode with
  | Registers ->
      let rw = if state.insp.active then sw - insp_w else sw in
      draw_registers state 0 cy rw reg_h;
      draw_call_stack state 0 (cy + reg_h) rw stack_h;
      draw_continuations state 0 (cy + reg_h + stack_h) rw con_h;
      if state.insp.active then draw_inspector state rw cy insp_w ch
  | Heap ->
      let hw = if state.insp.active then sw - insp_w else sw in
      draw_heap state 0 cy hw ch;
      if state.insp.active then draw_inspector state hw cy insp_w ch
  | Both ->
      let heap_x = reg_panel_w in
      let heap_w =
        if state.insp.active then sw - reg_panel_w - insp_w
        else sw - reg_panel_w
      in
      draw_registers state 0 cy reg_panel_w reg_h;
      draw_call_stack state 0 (cy + reg_h) reg_panel_w stack_h;
      draw_continuations state 0 (cy + reg_h + stack_h) reg_panel_w con_h;
      draw_heap state heap_x cy heap_w ch;
      if state.insp.active then
        draw_inspector state (heap_x + heap_w) cy insp_w ch);
  draw_instr_bar state 0 iy sw;
  draw_event_track state 0 ey sw;
  draw_scrubber state 0 sby sw;
  end_drawing ()

let run path =
  let trace = Trace.Serializer.deserialize_from_file path in
  let total_ticks = Array.length trace.Trace.Serializer.Read.vm.instrs in
  let state =
    {
      trace;
      pb = { tick = 0; playing = false; speed = 1.0; accum = 0.0 };
      hl = { reg = None; addr = None; age = 0.0 };
      insp = { active = false; addr = 0 };
      reg_scroll = { offset = 0 };
      stack_scroll = { offset = 0 };
      heap_scroll = { offset = 0 };
      con_scroll = { offset = 0 };
      mode = Both;
      total_ticks;
    }
  in
  init_window screen_w screen_h "Map.Viz";
  set_target_fps 60;
  while not (window_should_close ()) do
    handle_input state;
    draw state
  done;
  close_window ()
