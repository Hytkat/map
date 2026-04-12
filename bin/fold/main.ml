open Mapv
open Mapv.Core
open Mapv.Bytecode

let gc_pressure_code =
  Instr.
    [|
      (* 0 *) Load (1, 1000);
      (* 1 *) Load (11, 0);
      (* 2 *) Lt (3, 11, 1);
      (* 3 *) Jz (3, 24);
      (* 4 *) Alloc (4, 0, 10);
      (* 5 *) Alloc (5, 0, 10);
      (* 6 *) Alloc (6, 0, 10);
      (* 7 *) Load (2, 100000);
      (* 8 *) Alloc (12, 0, 10);
      (* 9 *) SetField (12, 0, 2);
      (* 10 *) SubI (2, 2, 1);
      (* 11 *) Lt (13, 11, 2);
      (* 12 *) Jnz (13, 8);
      (* 13 *) Alloc (10, 1, 5);
      (* 14 *) SetField (10, 0, 4);
      (* 15 *) Mov (4, 10);
      (* 16 *) Load (12, 2);
      (* 17 *) Mod (13, 1, 12);
      (* 18 *) Eq (13, 13, 11);
      (* 19 *) Jz (13, 22);
      (* 20 *) Alloc (14, 2, 500);
      (* 21 *) SubI (1, 1, 1);
      (* 22 *) ConYield (0, 11);
      (* 23 *) Jmp 2;
      (* 24 *) Halt;
    |]

let scheduler_code =
  Instr.
    [|
      (* 1 *) LoadK (0, 1);
      (* 2 *) ConNew (1, 0);
      (* 3 *) LoadNil 2;
      (* 4 *) ConResume (3, 1, 2);
      (* 5 *) ConStatus (4, 1);
      (* 6 *) Load (5, 1);
      (* 7 *) Eq (6, 4, 5);
      (* 8 *) Jnz (6, 3);
      (* 9 *) Halt;
    |]

let program : Serializer.program =
  {
    edition = Serializer.edition;
    imports = [||];
    constants = [||];
    funcs =
      [|
        { name = "scheduler"; arity = 0; code = scheduler_code };
        { name = "gc_pressure"; arity = 0; code = gc_pressure_code };
      |];
    debug = [||];
  }

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let module L = Loader.Make (H) (VM) in
  let config =
    {
      Config.default with
      heap = { chunk_size = 128; young_limit = 256; max_chunks = 128 };
      gc = { major_threshold = 128; major_growth_factor = 1.1 };
    }
  in

  ignore (Serializer.serialize_to_file "_gc_crush.mapv" program);

  let heap_ctx =
    Heap.Tracing.make ~max_chunks:config.heap.max_chunks
      ~chunk_size:config.heap.chunk_size ~sample_rate:1
  in
  let vm_ctx = Vm.Tracing.make () in
  let vm = L.load_file "_gc_crush.mapv" config heap_ctx vm_ctx in

  (match VM.run vm with
  | () -> ()
  | exception e ->
      Printf.printf "VM Panic: %s\n" (Printexc.to_string e);
      exit 1);

  Trace.Serializer.serialize_to_file "_gc_crush.mapvt" vm_ctx heap_ctx;
  Viz.Renderer.run "_gc_crush.mapvt"
