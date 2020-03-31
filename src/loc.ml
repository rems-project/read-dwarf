(** This module represent architectural locations *)

open Fun

open Logs.Logger (struct
  let str = "Loc"
end)

(** The type of a dwarf location stack operation *)
type dwop = Dwarf.operation

(** The type of a location, as static as possible *)
type t =
  | Register of Reg.path  (** In the register *)
  | RegisterOffset of Reg.path * int  (** At register + offset address *)
  | StackFrame of int  (** On the stackFrame with offset *)
  | Global of Elf.SymTbl.sym_offset  (** Global variable *)
  | Dwarf of dwop list  (** Uninterpreted dwarf location *)

(** The type of a location in linksem format *)
type linksem_t = dwop list

(** The integer value of the DW_OP_addr constant in DWARF standard

    TODO this should come from LinkSem's dwarf *)
let vDW_OP_addr : int = 0x03

(** The integer value of the DW_OP_reg0 constant in DWARF standard *)
let vDW_OP_reg0 : int = Z.to_int Dwarf.vDW_OP_reg0

(** The integer value of the DW_OP_breg0 constant in DWARF standard *)
let vDW_OP_breg0 : int = Z.to_int Dwarf.vDW_OP_breg0

(** Convert a linksem location description into a {!Loc.t}

    Very naive for now : If the list has a single element that we can translate directly, we do,
    otherwise, we dump it into the Dwarf constructor
*)
let of_linksem ?(amap = Arch.dwarf_reg_map ()) (elf : Elf.File.t) : linksem_t -> t =
  let int_of_oav : Dwarf.operation_argument_value -> int = function
    | OAV_natural n -> Z.to_int n
    | OAV_integer i -> Z.to_int i
    | _ -> failwith "Expected integer argument"
  in
  function
  (* Register *)
  | [({ op_semantics = OpSem_reg; _ } as op)] ->
      let reg_num = Z.to_int op.op_code - vDW_OP_reg0 in
      if reg_num >= Array.length amap then
        failwith
          (Printf.sprintf "Loc.of_linksem: register number %d unknown, code %x, name %s" reg_num
             (Z.to_int op.op_code) op.op_string)
      else Register amap.(reg_num)
  (* RegisterOffset *)
  | [({ op_semantics = OpSem_breg; op_argument_values = [arg]; _ } as op)] ->
      let reg_num = Z.to_int op.op_code - vDW_OP_breg0 in
      if reg_num >= Array.length amap then
        failwith
          (Printf.sprintf "Loc.of_linksem: register number %d unknown, code %x, name %s" reg_num
             (Z.to_int op.op_code) op.op_string)
      else RegisterOffset (amap.(reg_num), int_of_oav arg)
  (* StackFrame *)
  | [({ op_semantics = OpSem_fbreg; op_argument_values = [arg]; _ } as op)] ->
      StackFrame (int_of_oav arg)
  (* Global *)
  | [({ op_semantics = OpSem_lit; op_code = code; op_argument_values = [arg]; _ } as op)] as ops
    when Z.to_int code = vDW_OP_addr -> (
      try Global (Elf.SymTbl.of_addr_with_offset elf.symbols @@ int_of_oav arg)
      with Not_found ->
        warn "Symbol at 0x%x not found in Loc.of_linksem" (int_of_oav arg);
        Dwarf ops
    )
  (* Other *)
  | ops -> Dwarf ops

(** Convert the location to a string. This is not reversible *)
let to_string = function
  | Register reg -> Reg.path_to_string reg
  | RegisterOffset (reg, off) -> Printf.sprintf "[%s+%x]" (Reg.path_to_string reg) off
  | StackFrame off -> Printf.sprintf "[frame+%x]" off
  | Global symoff -> Elf.SymTbl.string_of_sym_offset symoff
  | Dwarf ops -> Dwarf.pp_operations ops

(** Compare two location. Loc.t is not compatible with polymorphic compare *)
let compare l1 l2 =
  (* I love writing those functions *)
  match (l1, l2) with
  | (Register r1, Register r2) -> compare r1 r2
  | (Register _, _) -> -1
  | (_, Register _) -> 1
  | (RegisterOffset (r1, o1), RegisterOffset (r2, o2)) -> compare (r1, o1) (r2, o2)
  | (RegisterOffset (_, _), _) -> -1
  | (_, RegisterOffset (_, _)) -> 1
  | (StackFrame off1, StackFrame off2) -> compare off1 off2
  | (StackFrame _, _) -> -1
  | (_, StackFrame _) -> 1
  | (Global (sym1, off1), Global (sym2, off2)) ->
      Pair.compare ~fst:Elf.Sym.compare (sym1, off1) (sym2, off2)
  | (Global (_, _), _) -> -1
  | (_, Global (_, _)) -> 1
  | (Dwarf ops1, Dwarf ops2) -> compare ops1 ops2

(** Pretty-print the location *)
let pp = to_string %> PP.string
