open Core

module Heap = struct
  type t = { chunk_size : int; young_limit : int; max_chunks : int }

  let default = { chunk_size = 4096; young_limit = 1024; max_chunks = 1024 }

  let validate t =
    if t.chunk_size land (t.chunk_size - 1) <> 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry
              "Config.Heap: chunk_size must be a power of two"));
    if t.young_limit <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Heap: young_limit must be > 0"));
    if t.max_chunks <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Heap: max_chunks must be > 0"))
end

module Gc = struct
  type t = { major_threshold : int; major_growth_factor : float }

  let default = { major_threshold = 4096; major_growth_factor = 2.0 }

  let validate t =
    if t.major_threshold <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Gc: major_threshold must be > 0"));
    if t.major_growth_factor <= 1.0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry
              "Config.Gc: major_growth_factor must be > 1.0"))
end

module Fiber = struct
  type t = { max_fibers : int; call_depth : int }

  let default = { max_fibers = 1024; call_depth = 512 }

  let validate t =
    if t.max_fibers <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Fiber: max_fibers must be > 0"));
    if t.call_depth <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Fiber: call_depth must be > 0"))
end

module Vm = struct
  type t = { call_depth : int; exception_depth : int }

  let default = { call_depth = 512; exception_depth = 64 }

  let validate t =
    if t.call_depth <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Vm: call_depth must be > 0"));
    if t.exception_depth <= 0 then
      raise
        (Exception.Panic
           (Exception.Invalid_registry "Config.Vm: exception_depth must be > 0"))
end

type t = {
  heap : Heap.t;
  gc : Gc.t;
  vm : Vm.t;
  fiber : Fiber.t;
  registry : Exception.Registry.t;
}

let default =
  {
    heap = Heap.default;
    gc = Gc.default;
    vm = Vm.default;
    fiber = Fiber.default;
    registry = Exception.Registry.empty;
  }

let validate t =
  Heap.validate t.heap;
  Gc.validate t.gc;
  Vm.validate t.vm;
  Fiber.validate t.fiber
