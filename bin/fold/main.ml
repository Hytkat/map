open Mapv.Core
open Mapv.Bytecode

let () =
  let all_instructions =
    Instr.
      [|
        Nop;
        Halt;
        Mov (0, 1);
        Load (2, 42);
        LoadF (3, 3.14159);
        LoadB (4, true);
        LoadNil 5;
        LoadK (6, 128);
        LoadS (7, 0xDEADBEEFL);
        Add (8, 9, 10);
        Sub (11, 12, 13);
        Mul (14, 15, 16);
        Div (17, 18, 19);
        Mod (20, 21, 22);
        AddI (23, 24, 100);
        SubI (25, 26, 200);
        MulI (27, 28, 300);
        AddF (29, 30, 31);
        SubF (32, 33, 34);
        MulF (35, 36, 37);
        DivF (38, 39, 40);
        And (41, 42, 43);
        Or (44, 45, 46);
        Xor (47, 48, 49);
        Shl (50, 51, 52);
        Shr (53, 54, 55);
        ShrU (56, 57, 58);
        ShlI (59, 60, 4);
        ShrI (61, 62, 2);
        ShrUI (63, 64, 1);
        Eq (65, 66, 67);
        Ne (68, 69, 70);
        Lt (71, 72, 73);
        LtU (74, 75, 76);
        Lte (77, 78, 79);
        LteU (80, 81, 82);
        EqF (83, 84, 85);
        NeF (86, 87, 88);
        LtF (89, 90, 91);
        LteF (92, 93, 94);
        I2F (95, 96);
        F2I (97, 98);
        TypeOf (99, 100);
        Alloc (101, 1, 16);
        GetField (102, 103, 2);
        SetField (104, 3, 105);
        GetTag (106, 107);
        Len (108, 109);
        Jmp 0;
        Jz (110, 5);
        Jnz (111, 10);
        Call (120, 0, 2, 112);
        TailCall (130, 3, 5);
        DynCall (113, 6, 8, 114);
        TailDynCall (115, 9, 11);
        Ret 116;
        Try (200, 117);
        Throw 118;
        EndTry;
        ConNew (119, 120);
        ConYield (121, 122);
        ConResume (123, 124, 125);
        ConStatus (126, 127);
      |]
  in

  let prog_to_file : Serializer.program =
    {
      edition = Serializer.edition;
      imports = [| { symbol_id = 1L; name = "test.internal" } |];
      constants = [| Value.Int 42; Value.Float 2.718 |];
      funcs = [| { name = "full_test"; arity = 0; code = all_instructions } |];
      debug = [||];
    }
  in
  Instr.inspect all_instructions;

  let filename = "_full_test.mapv" in

  let serialized_bytes = Serializer.serialize_to_file filename prog_to_file in
  Printf.printf "\nBinary Size: %d bytes\n" (Bytes.length serialized_bytes);

  let prog_from_file = Serializer.deserialize_from_file filename in
  Printf.printf "Read Function: %s\n\n" prog_from_file.funcs.(0).name;

  Instr.inspect prog_from_file.funcs.(0).code;

  let original_len = Array.length all_instructions in
  let read_len = Array.length prog_from_file.funcs.(0).code in

  if original_len <> read_len then
    Printf.printf "\nFAIL: Instruction count mismatch (%d vs %d)\n" original_len
      read_len
  else Printf.printf "\nSUCCESS: Instruction count match (%d)\n" read_len
