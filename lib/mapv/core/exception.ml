type panic =
  | Alloc_error of string
  | Stack_overflow of string
  | Invalid_registry of string
  | Native_panic of exn

type signal =
  | Type_error of string
  | Bounds_error of string
  | Div_by_zero of string
  | Arity_error of string
  | Custom of int * string

exception Panic of panic
exception Signal of signal

let panic_to_string = function
  | Alloc_error msg -> Format.asprintf "AllocError: %s" msg
  | Stack_overflow msg -> Format.asprintf "StackOverflow: %s" msg
  | Invalid_registry msg -> Format.asprintf "InvalidRegistry: %s" msg
  | Native_panic e -> Format.asprintf "NativePanic: %s" (Printexc.to_string e)

let builtin_type_error = 0
let builtin_bounds_error = 1
let builtin_div_by_zero = 2
let builtin_arity_error = 3
let reserved_codes = 100

let signal_code = function
  | Type_error _ -> builtin_type_error
  | Bounds_error _ -> builtin_bounds_error
  | Div_by_zero _ -> builtin_div_by_zero
  | Arity_error _ -> builtin_arity_error
  | Custom (code, _) -> code

let signal_message = function
  | Type_error msg -> msg
  | Bounds_error msg -> msg
  | Div_by_zero msg -> msg
  | Arity_error msg -> msg
  | Custom (_, msg) -> msg

let signal_to_pair s = (signal_code s, signal_message s)

module type ERROR = sig
  val name : string
  val code : int
end

module Registry = struct
  module IntMap = Map.Make (Int)

  type entry = { name : string; code : int }
  type t = entry IntMap.t

  let empty =
    List.fold_left
      (fun acc (name, code) -> IntMap.add code { name; code } acc)
      IntMap.empty
      [
        ("TypeError", builtin_type_error);
        ("BoundsError", builtin_bounds_error);
        ("DivByZero", builtin_div_by_zero);
        ("ArityError", builtin_arity_error);
      ]

  let register (module E : ERROR) registry =
    if E.code < reserved_codes then
      raise
        (Panic
           (Invalid_registry
              (Format.asprintf "error code %d is reserved (must be >= %d)"
                 E.code reserved_codes)));
    if IntMap.mem E.code registry then
      raise
        (Panic
           (Invalid_registry
              (Format.asprintf "error code %d already registered" E.code)));
    IntMap.add E.code { name = E.name; code = E.code } registry

  let name_of registry code =
    IntMap.find_opt code registry |> Option.map (fun e -> e.name)

  let throw code msg = raise (Signal (Custom (code, msg)))
end
