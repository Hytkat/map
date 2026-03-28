open Mapv
open Mapv.Core
open Mapv.Bytecode

let factorial_code =
  Instr.
    [|
      Load (1, 10);
      Load (2, 1);
      Load (3, 0);
      Eq (4, 1, 3);
      Jnz (4, 56);
      Mul (2, 2, 1);
      SubI (1, 1, 1);
      Jmp 24;
      ConYield (0, 2);
      Halt;
    |]

let fibonacci_code =
  Instr.
    [|
      Load (1, 10);
      Load (2, 0);
      Load (3, 1);
      Load (4, 0);
      Lt (5, 4, 1);
      Jz (5, 72);
      Add (6, 2, 3);
      Mov (2, 3);
      Mov (3, 6);
      AddI (4, 4, 1);
      Jmp 32;
      ConYield (0, 2);
      Halt;
    |]

let scheduler_code =
  Instr.
    [|
      LoadK (0, 1);
      LoadK (1, 2);
      ConNew (2, 0);
      ConNew (3, 1);
      LoadNil 4;
      ConResume (5, 2, 4);
      ConResume (6, 3, 4);
      Halt;
    |]

let program : Serializer.program =
  {
    edition = Serializer.edition;
    imports = [||];
    constants = [||];
    funcs =
      [|
        { name = "scheduler"; arity = 0; code = scheduler_code };
        { name = "factorial"; arity = 0; code = factorial_code };
        { name = "fibonacci"; arity = 0; code = fibonacci_code };
      |];
    debug = [||];
  }

let () =
  let module H = Heap.Make (Heap.Tracing) in
  let module VM = Vm.Make (H) (Vm.Tracing) in
  let module L = Loader.Make (H) (VM) in
  ignore (Serializer.serialize_to_file "_fiber_demo.mapv" program);
  let loaded = Serializer.deserialize_from_file "_fiber_demo.mapv" in
  Array.iter
    (fun (f : Serializer.func) ->
      Printf.printf "%s (arity=%d):\n" f.name f.arity;
      Instr.inspect f.code;
      print_newline ())
    loaded.funcs;
  let heap_ctx =
    Heap.Tracing.make ~max_chunks:Config.default.heap.max_chunks
      ~chunk_size:Config.default.heap.chunk_size ~sample_rate:10
  in
  let vm_ctx = Vm.Tracing.make () in
  let vm = L.load_file "_fiber_demo.mapv" Config.default heap_ctx vm_ctx in
  (match VM.run vm with
  | () -> ()
  | exception Exception.Signal s ->
      Printf.printf "uncaught signal: %s\n" (Exception.signal_message s);
      exit 1
  | exception Exception.Panic p ->
      Printf.printf "panic: %s\n" (Exception.panic_to_string p);
      exit 1);
  Printf.printf "status: %s\n" (VM.get_status vm);
  let check name got expected =
    Printf.printf "%s = %d  [expected %d]  %s\n" name got expected
      (if got = expected then "PASS" else "FAIL")
  in
  check "factorial(10)" (Value.to_int (VM.get_reg vm 5)) 3628800;
  check "fibonacci(10)" (Value.to_int (VM.get_reg vm 6)) 55;
  Trace.Serializer.serialize_to_file "_fiber_demo.mapvt" vm_ctx heap_ctx;
  Printf.printf "trace written to _fiber_demo.mapvt\n";
  Viz.Renderer.run "_fiber_demo.mapvt"
