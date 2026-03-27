type 'a native_resource = {
  value : 'a;
  tag : 'a Type.Id.t;
  finalizer : ('a -> unit) option;
}

type native_ptr = NativeResource : 'a native_resource -> native_ptr

type t =
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | Ptr of int
  | NativePtr of native_ptr
  | NativeFun of (t array -> t)

let type_nil = 0
let type_bool = 1
let type_int = 2
let type_float = 3
let type_ptr = 4
let type_fun = 5
let bytecode_id : Instr.t array Type.Id.t = Type.Id.make ()

let make_native : type a.
    tag:a Type.Id.t -> finalizer:(a -> unit) option -> a -> t =
 fun ~tag ~finalizer value ->
  NativePtr (NativeResource { value; tag; finalizer })

let get_native : type a. a Type.Id.t -> t -> a option =
 fun tag v ->
  match v with
  | NativePtr (NativeResource r) ->
      begin match Type.Id.provably_equal tag r.tag with
      | Some Type.Equal -> Some r.value
      | None -> None
      end
  | _ -> None

let to_native : type a. a Type.Id.t -> t -> a =
 fun tag v ->
  match get_native tag v with
  | Some x -> x
  | None -> raise Exception.(Signal (Type_error "expected NativePtr"))

let call_finalizer (NativeResource r) =
  match r.finalizer with Some f -> f r.value | None -> ()

let type_of = function
  | Nil -> type_nil
  | Bool _ -> type_bool
  | Int _ -> type_int
  | Float _ -> type_float
  | Ptr _ -> type_ptr
  | NativePtr _ -> type_ptr
  | NativeFun _ -> type_fun

let is_nil = function Nil -> true | _ -> false
let is_bool = function Bool _ -> true | _ -> false
let is_int = function Int _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false
let is_ptr = function Ptr _ -> true | _ -> false
let is_native = function NativePtr _ -> true | _ -> false
let is_fun = function NativeFun _ -> true | _ -> false
let is_gc_root = function Ptr _ | NativePtr _ -> true | _ -> false

let to_bool = function
  | Bool b -> b
  | _ -> raise Exception.(Signal (Type_error "expected Bool"))

let to_int = function
  | Int n -> n
  | _ -> raise Exception.(Signal (Type_error "expected Int"))

let to_float = function
  | Float f -> f
  | _ -> raise Exception.(Signal (Type_error "expected Float"))

let to_ptr = function
  | Ptr p -> p
  | _ -> raise Exception.(Signal (Type_error "expected Ptr"))

let to_bool_opt = function Bool b -> Some b | _ -> None
let to_int_opt = function Int n -> Some n | _ -> None
let to_float_opt = function Float f -> Some f | _ -> None
let to_ptr_opt = function Ptr p -> Some p | _ -> None

let equal a b =
  match (a, b) with
  | Nil, Nil -> true
  | Bool a, Bool b -> a = b
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Ptr a, Ptr b -> a = b
  | _, _ -> false

let pp fmt = function
  | Nil -> Format.fprintf fmt "Nil"
  | Bool b -> Format.fprintf fmt "Bool(%b)" b
  | Int n -> Format.fprintf fmt "Int(%d)" n
  | Float f -> Format.fprintf fmt "Float(%g)" f
  | Ptr p -> Format.fprintf fmt "Ptr(0x%x)" p
  | NativePtr _ -> Format.fprintf fmt "NativePtr(<opaque>)"
  | NativeFun _ -> Format.fprintf fmt "NativeFun(<opaque>)"

let to_string v =
  pp Format.str_formatter v;
  Format.flush_str_formatter ()
