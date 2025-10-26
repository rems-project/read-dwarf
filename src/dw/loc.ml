(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module represent architectural locations. Locations in DWARF
    information are represented as a little stack language (represented as
    {!dwop}[ list]) which describes how to compute the target value from the
    current concrete state. This computation can be arbitrarily complex, for
    example take half of the value from the high bits of a register and then the
    other half from memory at a specific address coming from another register.

    Those expression are not directly usable to guide the type inference, as
    we would much more like simple direct information like "This value is in that
    register". To solve this, there is a custom type {!t} that interpret simple
    static locations directly and leaves more complex location uninterpreted.

    This interpretation is currently very basic. It remains to be seen if it
    actually need to be improved.*)

open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

(** The type of a dwarf location stack operation *)
type dwop = Dwarf.operation

(** The type of a location, as static as possible *)
type t =
  | Register of State.Reg.t  (** In the register *)
  | RegisterOffset of State.Reg.t * int  (** At register + offset address *)
  | StackFrame of int  (** On the stackFrame with offset *)
  | Global of Elf.SymTable.sym_offset  (** Global variable with an offset *)
  | Const of Sym.t
  | Dwarf of dwop list  (** Uninterpreted dwarf location *)

(** The type of a location in linksem format *)
type linksem_t = dwop list

(** The integer value of the DW_OP_addr constant in DWARF standard

    TODO this should come from LinkSem's dwarf *)
let vDW_OP_addr : int = 0x03

(** The integer value of the DW_OP_reg0 constant in DWARF standard *)
let vDW_OP_reg0 : int = Sym.to_int Dwarf.vDW_OP_reg0

(** The integer value of the DW_OP_breg0 constant in DWARF standard *)
let vDW_OP_breg0 : int = Sym.to_int Dwarf.vDW_OP_breg0

(** Convert a linksem location description into a {!Loc.t}

    Very naive for now : If the list has a single element that we can translate
   directly, we do. Otherwise, we dump it into the {!t.Dwarf} constructor *)
let of_linksem ?(amap = Arch.dwarf_reg_map ()) (elf : Elf.File.t) : linksem_t -> t =
  let sym_of_oav : Dwarf.operation_argument_value -> Sym.t = function
  | OAV_natural n -> n
  | OAV_integer i -> i
  | _ -> failwith "Expected integer argument"
  in
  let int_of_oav oav = oav |> sym_of_oav |> Sym.to_int in
  function
  (* Register *)
  | [({ op_semantics = OpSem_reg; _ } as op)] ->
      let reg_num = Sym.to_int op.op_code - vDW_OP_reg0 in
      if reg_num >= Array.length amap then
        failwith
          (Printf.sprintf "Loc.of_linksem: register number %d unknown, code %x, name %s" reg_num
             (Sym.to_int op.op_code) op.op_string)
      else Register amap.(reg_num)
  (* RegisterOffset *)
  | [({ op_semantics = OpSem_breg; op_argument_values = [arg]; _ } as op)] ->
      let reg_num = Sym.to_int op.op_code - vDW_OP_breg0 in
      if reg_num >= Array.length amap then
        failwith
          (Printf.sprintf "Loc.of_linksem: register number %d unknown, code %x, name %s" reg_num
             (Sym.to_int op.op_code) op.op_string)
      else RegisterOffset (amap.(reg_num), int_of_oav arg)
  (* StackFrame *)
  | [{ op_semantics = OpSem_fbreg; op_argument_values = [arg]; _ }] -> StackFrame (int_of_oav arg)
  (* Global *)
  | [{ op_semantics = OpSem_lit; op_code = code; op_argument_values = [arg]; _ }] as ops
    when Sym.to_int code = vDW_OP_addr -> (
      let addr = Addr.of_sym @@ sym_of_oav arg in
      try Global (Elf.SymTable.of_addr_with_offset elf.symbols @@ addr)
      with Not_found ->
        warn "Symbol at %t not found in Loc.of_linksem" (Pp.top Sym.pp (sym_of_oav arg));
        Dwarf ops
    )
  (* Other *)
  | [{ op_semantics = OpSem_lit; op_argument_values = [arg]; _ }; { op_semantics = OpSem_stack_value; _ }] ->
      let value = sym_of_oav arg in 
      Const value
  | ops -> Dwarf ops

(** Convert the location to a string. This is not reversible *)
let to_string = function
  | Register reg -> State.Reg.to_string reg
  | RegisterOffset (reg, off) -> Printf.sprintf "[%s+%x]" (State.Reg.to_string reg) off
  | StackFrame off -> Printf.sprintf "[frame+%x]" off
  | Global symoff -> Elf.SymTable.string_of_sym_offset symoff
  | Const x -> Sym.to_string x
  | Dwarf ops -> Dwarf.pp_operations ops

(** Compare two location. Loc.t is not compatible with polymorphic compare *)
let compare l1 l2 =
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
      Pair.compare ~fst:Elf.Symbol.compare (sym1, off1) (sym2, off2)
  | (Global (_, _), _) -> -1
  | (_, Global (_, _)) -> 1
  | (Const (Absolute x), Const (Absolute y)) -> Z.compare x y
  | (Const (Offset(s,x)), Const (Offset(t,y))) -> Pair.compare ~snd:Z.compare (s,x) (t,y)
  | (Const (Absolute _), Const (Offset _)) -> -1
  | (Const (Offset _), Const (Absolute _)) -> 1
  | (Const _, _) -> -1
  | (_, Const _) -> 1
  | (Dwarf ops1, Dwarf ops2) -> compare ops1 ops2

(** Pretty-print the location *)
let pp = to_string %> Pp.string
