open Core

let magic = "MAPVT"
let version = 1
let sec_vm = 0x00
let sec_heap = 0x01
let sec_gc = 0x02

module Write = struct
  let u8 buf v = Buffer.add_uint8 buf v
  let u16 buf v = Buffer.add_uint16_le buf v
  let u32 buf v = Buffer.add_int32_le buf (Int32.of_int v)
  let u64 buf v = Buffer.add_int64_le buf v

  let align buf n =
    let r = Buffer.length buf mod n in
    if r <> 0 then
      for _ = 1 to n - r do
        Buffer.add_char buf '\x00'
      done

  let value buf = function
    | Value.Nil -> u8 buf 0
    | Value.Bool b ->
        u8 buf 1;
        u8 buf (if b then 1 else 0)
    | Value.Int n ->
        u8 buf 2;
        u64 buf (Int64.of_int n)
    | Value.Float f ->
        u8 buf 3;
        u64 buf (Int64.bits_of_float f)
    | Value.Ptr p ->
        u8 buf 4;
        u32 buf p
    | Value.NativeFun _ -> u8 buf 5
    | Value.NativePtr _ -> u8 buf 6

  let gc_event buf = function
    | Heap.Minor_start -> u8 buf 0
    | Heap.Minor_end { promoted } ->
        u8 buf 1;
        u32 buf promoted
    | Heap.Major_mark { steps } ->
        u8 buf 2;
        u32 buf steps
    | Heap.Major_sweep { steps; freed } ->
        u8 buf 3;
        u32 buf steps;
        u32 buf freed
    | Heap.Major_end -> u8 buf 4

  let vm_section (ctx : Vm.Tracing.ctx) =
    let buf = Buffer.create 1024 in
    let instrs = List.rev ctx.instrs in
    let calls = List.rev ctx.calls in
    let rets = List.rev ctx.rets in
    let throws = List.rev ctx.throws in
    let con_news = List.rev ctx.con_news in
    let con_yields = List.rev ctx.con_yields in
    let con_resumes = List.rev ctx.con_resumes in
    let reg_writes = List.rev ctx.reg_writes in
    u32 buf (List.length instrs);
    List.iter
      (fun (tick, pc, op) ->
        u32 buf tick;
        u32 buf pc;
        u8 buf op;
        u8 buf 0;
        u16 buf 0)
      instrs;
    u32 buf (List.length calls);
    List.iter
      (fun (tick, pc, target) ->
        u32 buf tick;
        u32 buf pc;
        u32 buf target)
      calls;
    u32 buf (List.length rets);
    List.iter
      (fun (tick, pc) ->
        u32 buf tick;
        u32 buf pc)
      rets;
    u32 buf (List.length throws);
    List.iter
      (fun (tick, pc) ->
        u32 buf tick;
        u32 buf pc)
      throws;
    u32 buf (List.length con_news);
    List.iter
      (fun (tick, pc) ->
        u32 buf tick;
        u32 buf pc)
      con_news;
    u32 buf (List.length con_yields);
    List.iter
      (fun (tick, pc) ->
        u32 buf tick;
        u32 buf pc)
      con_yields;
    u32 buf (List.length con_resumes);
    List.iter
      (fun (tick, pc) ->
        u32 buf tick;
        u32 buf pc)
      con_resumes;
    u32 buf (List.length reg_writes);
    List.iter
      (fun (tick, reg, v) ->
        u32 buf tick;
        u32 buf reg;
        value buf v)
      reg_writes;
    buf

  let heap_section (ctx : Heap.Tracing.ctx) =
    let buf = Buffer.create 1024 in
    let allocs = List.rev ctx.allocs in
    let frees = List.rev ctx.frees in
    let promotes = List.rev ctx.promotes in
    let reads = List.rev ctx.reads in
    let writes = List.rev ctx.writes in
    u32 buf (List.length allocs);
    List.iter
      (fun (tick, addr, size, tag) ->
        u32 buf tick;
        u32 buf addr;
        u32 buf size;
        u32 buf tag)
      allocs;
    u32 buf (List.length frees);
    List.iter
      (fun (tick, addr) ->
        u32 buf tick;
        u32 buf addr)
      frees;
    u32 buf (List.length promotes);
    List.iter
      (fun (tick, addr) ->
        u32 buf tick;
        u32 buf addr)
      promotes;
    u32 buf (List.length reads);
    List.iter
      (fun (tick, addr, field) ->
        u32 buf tick;
        u32 buf addr;
        u32 buf field)
      reads;
    u32 buf (List.length writes);
    List.iter
      (fun (tick, addr, field, v) ->
        u32 buf tick;
        u32 buf addr;
        u32 buf field;
        value buf v)
      writes;
    buf

  let gc_section (ctx : Heap.Tracing.ctx) =
    let buf = Buffer.create 256 in
    let events = List.rev ctx.events in
    u32 buf (List.length events);
    List.iter
      (fun (tick, ev) ->
        u32 buf tick;
        gc_event buf ev)
      events;
    buf

  let program vm_ctx heap_ctx =
    let out = Buffer.create 4096 in
    Buffer.add_string out magic;
    u16 out version;
    u8 out 0;
    let secs =
      [|
        (sec_vm, vm_section vm_ctx);
        (sec_heap, heap_section heap_ctx);
        (sec_gc, gc_section heap_ctx);
      |]
    in
    let n_secs = Array.length secs in
    u32 out n_secs;
    let table_start = 5 + 2 + 1 + 4 in
    let body_start = table_start + (n_secs * 12) in
    let offsets = Array.make n_secs 0 in
    let pos = ref body_start in
    Array.iteri
      (fun i (_, b) ->
        let padding = (8 - (!pos mod 8)) mod 8 in
        offsets.(i) <- !pos + padding;
        pos := offsets.(i) + Buffer.length b)
      secs;
    Array.iteri
      (fun i (id, b) ->
        u32 out id;
        u32 out offsets.(i);
        u32 out (Buffer.length b))
      secs;
    Array.iteri
      (fun i (_, b) ->
        let current =
          table_start + (n_secs * 12)
          + Array.fold_left
              (fun acc j ->
                if j < i then
                  acc
                  + ((8 - ((table_start + (n_secs * 12) + acc) mod 8)) mod 8)
                  + Buffer.length (snd secs.(j))
                else acc)
              0
              (Array.init i (fun j -> j))
        in
        ignore current;
        let padding = (8 - (Buffer.length out mod 8)) mod 8 in
        for _ = 1 to padding do
          Buffer.add_char out '\x00'
        done;
        Buffer.add_buffer out b)
      secs;
    out
end

module Read = struct
  type cursor = { data : bytes; mutable pos : int }

  let make data = { data; pos = 0 }
  let seek cur p = cur.pos <- p

  let u8 cur =
    let v = Bytes.get_uint8 cur.data cur.pos in
    cur.pos <- cur.pos + 1;
    v

  let u16 cur =
    let v = Bytes.get_uint16_le cur.data cur.pos in
    cur.pos <- cur.pos + 2;
    v

  let u32 cur =
    let v = Int32.to_int (Bytes.get_int32_le cur.data cur.pos) in
    cur.pos <- cur.pos + 4;
    v

  let u64 cur =
    let v = Bytes.get_int64_le cur.data cur.pos in
    cur.pos <- cur.pos + 8;
    v

  let value cur =
    match u8 cur with
    | 0 -> Value.Nil
    | 1 -> Value.Bool (u8 cur <> 0)
    | 2 -> Value.Int (Int64.to_int (u64 cur))
    | 3 -> Value.Float (Int64.float_of_bits (u64 cur))
    | 4 -> Value.Ptr (u32 cur)
    | 5 -> Value.Nil
    | 6 -> Value.Nil
    | _ -> Value.Nil

  type vm_trace = {
    instrs : (int * int * int) array;
    calls : (int * int * int) array;
    rets : (int * int) array;
    throws : (int * int) array;
    con_news : (int * int) array;
    con_yields : (int * int) array;
    con_resumes : (int * int) array;
    reg_writes : (int * int * Value.t) array;
  }

  type heap_trace = {
    allocs : (int * int * int * int) array;
    frees : (int * int) array;
    promotes : (int * int) array;
    reads : (int * int * int) array;
    writes : (int * int * int * Value.t) array;
  }

  type gc_trace = { events : (int * Heap.event) array }
  type t = { vm : vm_trace; heap : heap_trace; gc : gc_trace }

  let vm_section cur =
    let n_instrs = u32 cur in
    let instrs =
      Array.init n_instrs (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          let op = u8 cur in
          cur.pos <- cur.pos + 3;
          (tick, pc, op))
    in
    let n_calls = u32 cur in
    let calls =
      Array.init n_calls (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          let target = u32 cur in
          (tick, pc, target))
    in
    let n_rets = u32 cur in
    let rets =
      Array.init n_rets (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_throws = u32 cur in
    let throws =
      Array.init n_throws (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_con_news = u32 cur in
    let con_news =
      Array.init n_con_news (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_con_yields = u32 cur in
    let con_yields =
      Array.init n_con_yields (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_con_resumes = u32 cur in
    let con_resumes =
      Array.init n_con_resumes (fun _ ->
          let tick = u32 cur in
          let pc = u32 cur in
          (tick, pc))
    in
    let n_reg_writes = u32 cur in
    let reg_writes =
      Array.init n_reg_writes (fun _ ->
          let tick = u32 cur in
          let reg = u32 cur in
          let v = value cur in
          (tick, reg, v))
    in
    {
      instrs;
      calls;
      rets;
      throws;
      con_news;
      con_yields;
      con_resumes;
      reg_writes;
    }

  let heap_section cur =
    let n_allocs = u32 cur in
    let allocs =
      Array.init n_allocs (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let size = u32 cur in
          let tag = u32 cur in
          (tick, addr, size, tag))
    in
    let n_frees = u32 cur in
    let frees =
      Array.init n_frees (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          (tick, addr))
    in
    let n_promotes = u32 cur in
    let promotes =
      Array.init n_promotes (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          (tick, addr))
    in
    let n_reads = u32 cur in
    let reads =
      Array.init n_reads (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let field = u32 cur in
          (tick, addr, field))
    in
    let n_writes = u32 cur in
    let writes =
      Array.init n_writes (fun _ ->
          let tick = u32 cur in
          let addr = u32 cur in
          let field = u32 cur in
          let v = value cur in
          (tick, addr, field, v))
    in
    { allocs; frees; promotes; reads; writes }

  let gc_event cur =
    match u8 cur with
    | 0 -> Heap.Minor_start
    | 1 -> Heap.Minor_end { promoted = u32 cur }
    | 2 -> Heap.Major_mark { steps = u32 cur }
    | 3 ->
        let steps = u32 cur in
        let freed = u32 cur in
        Heap.Major_sweep { steps; freed }
    | 4 -> Heap.Major_end
    | _ -> Heap.Major_end

  let gc_section cur =
    let n = u32 cur in
    let events =
      Array.init n (fun _ ->
          let tick = u32 cur in
          let ev = gc_event cur in
          (tick, ev))
    in
    { events }

  let load cur =
    let m = Bytes.sub_string cur.data cur.pos 5 in
    if m <> magic then
      raise
        (Exception.Panic
           (Exception.Alloc_error (Format.asprintf "trace: invalid magic %S" m)));
    cur.pos <- 5;
    let ver = u16 cur in
    if ver <> version then
      raise
        (Exception.Panic
           (Exception.Alloc_error
              (Format.asprintf "trace: unknown version %d" ver)));
    cur.pos <- 8;
    let n_sec = u32 cur in
    let secs =
      Array.init n_sec (fun _ ->
          let id = u32 cur in
          let off = u32 cur in
          let sz = u32 cur in
          (id, off, sz))
    in
    let get_sec id =
      Array.find_map (fun (i, o, _) -> if i = id then Some o else None) secs
    in
    let require id name =
      match get_sec id with
      | Some o -> o
      | None ->
          raise
            (Exception.Panic
               (Exception.Alloc_error
                  (Format.asprintf "trace: missing section '%s'" name)))
    in
    let vm_off = require sec_vm "vm" in
    seek cur vm_off;
    let vm = vm_section cur in
    let heap_off = require sec_heap "heap" in
    seek cur heap_off;
    let heap = heap_section cur in
    let gc_off = require sec_gc "gc" in
    seek cur gc_off;
    let gc = gc_section cur in
    { vm; heap; gc }
end

let serialize vm_ctx heap_ctx =
  let buf = Write.program vm_ctx heap_ctx in
  Buffer.to_bytes buf

let serialize_to_file path vm_ctx heap_ctx =
  let b = serialize vm_ctx heap_ctx in
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_bytes oc b)

let deserialize data = Read.load (Read.make data)

let deserialize_from_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let b = Bytes.create n in
      really_input ic b 0 n;
      deserialize b)
