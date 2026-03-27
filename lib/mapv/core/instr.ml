type reg = int

type t =
  | Nop
  | Halt
  | Mov of reg * reg
  | Load of reg * int
  | LoadF of reg * float
  | LoadB of reg * bool
  | LoadNil of reg
  | LoadK of reg * int
  | LoadS of reg * int64
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Mod of reg * reg * reg
  | AddI of reg * reg * int
  | SubI of reg * reg * int
  | MulI of reg * reg * int
  | AddF of reg * reg * reg
  | SubF of reg * reg * reg
  | MulF of reg * reg * reg
  | DivF of reg * reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Xor of reg * reg * reg
  | Shl of reg * reg * reg
  | Shr of reg * reg * reg
  | ShrU of reg * reg * reg
  | ShlI of reg * reg * int
  | ShrI of reg * reg * int
  | ShrUI of reg * reg * int
  | Eq of reg * reg * reg
  | Ne of reg * reg * reg
  | Lt of reg * reg * reg
  | LtU of reg * reg * reg
  | Lte of reg * reg * reg
  | LteU of reg * reg * reg
  | EqF of reg * reg * reg
  | NeF of reg * reg * reg
  | LtF of reg * reg * reg
  | LteF of reg * reg * reg
  | I2F of reg * reg
  | F2I of reg * reg
  | TypeOf of reg * reg
  | Alloc of reg * int * int
  | GetField of reg * reg * int
  | SetField of reg * int * reg
  | GetTag of reg * reg
  | Len of reg * reg
  | Jmp of int
  | Jz of reg * int
  | Jnz of reg * int
  | Call of int * reg * reg * reg
  | TailCall of int * reg * reg
  | DynCall of reg * reg * reg * reg
  | TailDynCall of reg * reg * reg
  | Ret of reg
  | Try of int * reg
  | Throw of reg
  | EndTry
  | ConNew of reg * reg
  | ConYield of reg * reg
  | ConResume of reg * reg * reg
  | ConStatus of reg * reg

let pp fmt = function
  | Nop -> Format.fprintf fmt "Nop"
  | Halt -> Format.fprintf fmt "Halt"
  | Mov (d, s) -> Format.fprintf fmt "Mov        r%d <- r%d" d s
  | Load (d, n) -> Format.fprintf fmt "Load       r%d <- %d" d n
  | LoadF (d, f) -> Format.fprintf fmt "LoadF      r%d <- %g" d f
  | LoadB (d, b) -> Format.fprintf fmt "LoadB      r%d <- %b" d b
  | LoadNil d -> Format.fprintf fmt "LoadNil    r%d" d
  | LoadK (d, i) -> Format.fprintf fmt "LoadK      r%d <- k[%d]" d i
  | LoadS (d, id) -> Format.fprintf fmt "LoadS      r%d <- sym[0x%Lx]" d id
  | Add (d, a, b) -> Format.fprintf fmt "Add        r%d <- r%d + r%d" d a b
  | Sub (d, a, b) -> Format.fprintf fmt "Sub        r%d <- r%d - r%d" d a b
  | Mul (d, a, b) -> Format.fprintf fmt "Mul        r%d <- r%d * r%d" d a b
  | Div (d, a, b) -> Format.fprintf fmt "Div        r%d <- r%d / r%d" d a b
  | Mod (d, a, b) -> Format.fprintf fmt "Mod        r%d <- r%d %% r%d" d a b
  | AddI (d, a, i) -> Format.fprintf fmt "AddI       r%d <- r%d + %d" d a i
  | SubI (d, a, i) -> Format.fprintf fmt "SubI       r%d <- r%d - %d" d a i
  | MulI (d, a, i) -> Format.fprintf fmt "MulI       r%d <- r%d * %d" d a i
  | AddF (d, a, b) -> Format.fprintf fmt "AddF       r%d <- r%d +. r%d" d a b
  | SubF (d, a, b) -> Format.fprintf fmt "SubF       r%d <- r%d -. r%d" d a b
  | MulF (d, a, b) -> Format.fprintf fmt "MulF       r%d <- r%d *. r%d" d a b
  | DivF (d, a, b) -> Format.fprintf fmt "DivF       r%d <- r%d /. r%d" d a b
  | And (d, a, b) -> Format.fprintf fmt "And        r%d <- r%d & r%d" d a b
  | Or (d, a, b) -> Format.fprintf fmt "Or         r%d <- r%d | r%d" d a b
  | Xor (d, a, b) -> Format.fprintf fmt "Xor        r%d <- r%d ^ r%d" d a b
  | Shl (d, a, b) -> Format.fprintf fmt "Shl        r%d <- r%d << r%d" d a b
  | Shr (d, a, b) -> Format.fprintf fmt "Shr        r%d <- r%d >> r%d" d a b
  | ShrU (d, a, b) -> Format.fprintf fmt "ShrU       r%d <- r%d >>> r%d" d a b
  | ShlI (d, a, i) -> Format.fprintf fmt "ShlI       r%d <- r%d << %d" d a i
  | ShrI (d, a, i) -> Format.fprintf fmt "ShrI       r%d <- r%d >> %d" d a i
  | ShrUI (d, a, i) -> Format.fprintf fmt "ShrUI      r%d <- r%d >>> %d" d a i
  | Eq (d, a, b) -> Format.fprintf fmt "Eq         r%d <- r%d == r%d" d a b
  | Ne (d, a, b) -> Format.fprintf fmt "Ne         r%d <- r%d != r%d" d a b
  | Lt (d, a, b) -> Format.fprintf fmt "Lt         r%d <- r%d < r%d" d a b
  | LtU (d, a, b) -> Format.fprintf fmt "LtU        r%d <- r%d < r%d (u)" d a b
  | Lte (d, a, b) -> Format.fprintf fmt "Lte        r%d <- r%d <= r%d" d a b
  | LteU (d, a, b) ->
      Format.fprintf fmt "LteU       r%d <- r%d <= r%d (u)" d a b
  | EqF (d, a, b) -> Format.fprintf fmt "EqF        r%d <- r%d == r%d" d a b
  | NeF (d, a, b) -> Format.fprintf fmt "NeF        r%d <- r%d != r%d" d a b
  | LtF (d, a, b) -> Format.fprintf fmt "LtF        r%d <- r%d < r%d" d a b
  | LteF (d, a, b) -> Format.fprintf fmt "LteF       r%d <- r%d <= r%d" d a b
  | I2F (d, s) -> Format.fprintf fmt "I2F        r%d <- (float)r%d" d s
  | F2I (d, s) -> Format.fprintf fmt "F2I        r%d <- (int)r%d" d s
  | TypeOf (d, s) -> Format.fprintf fmt "TypeOf     r%d <- typeof(r%d)" d s
  | Alloc (d, t, s) -> Format.fprintf fmt "Alloc      r%d tag=%d size=%d" d t s
  | GetField (d, o, f) -> Format.fprintf fmt "GetField   r%d <- r%d[%d]" d o f
  | SetField (o, f, s) -> Format.fprintf fmt "SetField   r%d[%d] <- r%d" o f s
  | GetTag (d, o) -> Format.fprintf fmt "GetTag     r%d <- tag(r%d)" d o
  | Len (d, o) -> Format.fprintf fmt "Len        r%d <- len(r%d)" d o
  | Jmp t -> Format.fprintf fmt "Jmp        -> %d" t
  | Jz (r, t) -> Format.fprintf fmt "Jz         r%d -> %d" r t
  | Jnz (r, t) -> Format.fprintf fmt "Jnz        r%d -> %d" r t
  | Call (t, s, e, r) ->
      Format.fprintf fmt "Call       pc=%d args=[r%d..r%d] ret->r%d" t s e r
  | TailCall (t, s, e) ->
      Format.fprintf fmt "TailCall   pc=%d args=[r%d..r%d]" t s e
  | DynCall (f, s, e, r) ->
      Format.fprintf fmt "DynCall    r%d args=[r%d..r%d] ret->r%d" f s e r
  | TailDynCall (f, s, e) ->
      Format.fprintf fmt "TailDynCall r%d args=[r%d..r%d]" f s e
  | Ret r -> Format.fprintf fmt "Ret        r%d" r
  | Try (h, c) -> Format.fprintf fmt "Try        handler=%d catch->r%d" h c
  | Throw r -> Format.fprintf fmt "Throw      r%d" r
  | EndTry -> Format.fprintf fmt "EndTry"
  | ConNew (d, s) -> Format.fprintf fmt "ConNew     r%d <- fiber(r%d)" d s
  | ConYield (d, v) -> Format.fprintf fmt "ConYield   r%d <- yield(r%d)" d v
  | ConResume (d, f, v) ->
      Format.fprintf fmt "ConResume  r%d <- resume(r%d, r%d)" d f v
  | ConStatus (d, f) -> Format.fprintf fmt "ConStatus  r%d <- status(r%d)" d f

let to_string i =
  pp Format.str_formatter i;
  Format.flush_str_formatter ()

let inspect program =
  let len = Array.length program in
  let width = String.length (string_of_int (max 0 (len - 1))) in
  Array.iteri
    (fun i instr -> Format.printf "%0*d: %a@." width i pp instr)
    program
