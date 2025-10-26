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

(** This module defines a new simplified kind of trace to replace Isla traces
    in the later stages of the instruction semantics processing.

    The traces are even simpler and more easily typable.
    The possible events are in the type {!event} and traces ({!t}) are just list
    of them.

    Compared to Isla, the concept of reading a register do not exist anymore. Nor
    the concept of pure symbolic variable or Sail structured values.
    Instead expression can contain only registers and results of previous memory read
    as decribe in the type {!Var.t}. All writing event directly write an entire expression.
    There are no intermediary variable definitions.

    This raise a problem that if an isla trace reads a register after having written
    to it, then this is ambiguous to represent.

    Thus a partially monadic representation has been chosen:
    - The effect of writes on registers are delayed to the end, which means that
      registers variable value in every expression is the value of that register
      at the beginning of the trace. In particular, if there are two write to
      the register, only the last one have any effect, the other can be deleted.
    - The memory operation behave in normal monadic way, in particular
      a memory read can read the value written by a previous write in the same
      instruction, even if this is very unlikely.

    In the end, both assertion and register write to different registers can be
    reordered at will.

    TODO: To make it more clean, getting register writes and assertions out of the trace
    would make sense like:
    {[
    type mem_event = Read of ... | Write of ...
    type t = { asserts : exp list; reg_writes : (Reg.t * exp) list; mem: mem_event list }
    ]}

    For all those reason, concatenating two trace semantically is very different that concatenating
    list of event, and is not implemented yet.

    The important functions are {!of_isla} to convert and Isla traces
    and {!simplify} for simplify traces.
*)

(** This module contains variable used in traces *)
module Var = struct
  module Reg = State.Reg

  (** A trace variable *)
  type t =
    | Register of Reg.t  (** The value of the register at the beginning of the trace *)
    | Read of int * Ast.Size.t  (** The result of that memory reading operation *)
    | NonDet of int * Ast.Size.t  (** Variable representing non-determinism in the spec *)
    | Segment of string * int  (** Variable representing symbolic segment in the opcode *)

  (** Convert the variable to the string encoding. For parsing infractructure reason,
      the encoding must always contain at least one [:]. *)
  let to_string = function
    | Register r -> Printf.sprintf "reg:%s" (Reg.to_string r)
    | Read (num, size) ->
        if size = Ast.Size.B64 then Printf.sprintf "read:%i" num
        else Printf.sprintf "read:%i:%dbits" num (Ast.Size.to_bits size)
    | NonDet (num, size) ->
        if size = Ast.Size.B64 then Printf.sprintf "nondet:%i" num
        else Printf.sprintf "nondet:%i:%dbits" num (Ast.Size.to_bits size)
    | Segment (name, bits) ->
        Printf.sprintf "segment:%s:%dbits" name bits

  (** Inverse of {!to_string} *)
  let of_string s =
    match String.split_on_char ':' s with
    | ["reg"; reg] -> Register (Reg.of_string reg)
    | ["read"; num] -> Read (int_of_string num, Ast.Size.B64)
    | ["read"; num; size] ->
        let size = Scanf.sscanf size "%dbits" Ast.Size.of_bits in
        Read (int_of_string num, size)
    | ["nondet"; num] -> NonDet (int_of_string num, Ast.Size.B64)
    | ["nondet"; num; size] ->
        let size = Scanf.sscanf size "%dbits" Ast.Size.of_bits in
        NonDet (int_of_string num, size)
    | ["segment"; name; bits] ->
        let bits = Scanf.sscanf bits "%dbits" Fun.id in
        Segment (name, bits)
    | _ -> Raise.inv_arg "%s is not a Base.Var.t" s

  (** Pretty prints the variable *)
  let pp v = v |> to_string |> Pp.string

  let equal = ( = )

  let hash = Hashtbl.hash

  let ty = function
    | Register reg -> Reg.reg_type reg
    | Read (_, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)
    | NonDet (_, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)
    | Segment (_, bits) -> Ast.Ty_BitVec bits

  let of_reg reg = Register reg
end

(** A trace expression. No let bindings, no memory operations *)
module ExpPp = Exp.Pp

module Typed = Exp.Typed

module Exp = struct
  include Exp.Make (Var)

  let of_reg reg = Var.of_reg reg |> of_var
end

type exp = Exp.t

(** The event type. See the {{!Trace}module description} for more details *)
type event =
  | WriteReg of { reg : State.Reg.t; value : exp }
  | ReadMem of { addr : exp; value : int; size : Ast.Size.t }
  | WriteMem of { addr : exp; value : exp; size : Ast.Size.t }
  | Assert of exp

type t = event list

let iter_var f =
  List.iter (fun event ->
      let exps =
        match event with
        | WriteReg { value; reg = _ } -> [value]
        | ReadMem { addr; value = _; size = _ } -> [addr]
        | WriteMem { addr; value; size = _ } -> [addr; value]
        | Assert exp -> [exp]
      in
      List.iter (Ast.Manip.exp_iter_var f) exps)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

(** Pretty print an expression *)
let pp_exp e = ExpPp.pp_exp Var.pp e

(** Pretty print an event *)
let pp_event =
  let open Pp in
  function
  | WriteReg { reg; value } ->
      dprintf "Write |reg:%s| with " (State.Reg.to_string reg) ^^ nest 4 (pp_exp value)
  | ReadMem { addr; value; size } ->
      dprintf "Read |read:%d| of %dbits from " value (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr)
  | WriteMem { addr; value; size } ->
      dprintf "Write %dbits at " (Ast.Size.to_bits size)
      ^^ nest 4 (pp_exp addr ^^ !^" with " ^^ pp_exp value)
  | Assert exp -> !^"Assert " ^^ nest 4 (pp_exp exp)

(** Pretty print a trace *)
let pp events = Pp.separate_map Pp.hardline pp_event events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Isla to Trace conversion }

    This section perform the conversion from Isla trace to the
    traces of this module.

    The conversion is generrally obvious, however there is subtlety: If the Isla
    trace reads a register after having written it, then the read produce the written
    expression instead of just the symbolic value of that register. That why there is
    a [written_registers] parameter to some function of this section.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Throw an error in case of local conversion error.
    Normally a type-checked Isla trace should not fail in this section *)
exception OfIslaError

(** The context mapping Isla variable numbers to trace expression *)
type value_context = exp HashVector.t

(** Get the exression of the variable at the index.
    Throw {!OfIslaError} if the variable is not bound *)
let get_var vc i =
  match HashVector.get_opt vc i with
  | Some exp -> exp
  | None ->
      warn "Could not find the variable v%d" i;
      raise OfIslaError

(** Convert an Isla expression to a [Trace] expression by replacing all Isla variable
    by their value in the context. Throw {!OfIslaError} if the substitution fails *)
let exp_conv_subst (vc : value_context) (exp : Isla.rexp) : exp =
  let vconv i _ = get_var vc i in
  Isla.Conv.exp_add_type_var_subst vconv exp

(** Convert an {!Isla.valu} in a expression *)
let exp_of_valu l vc : Isla.valu -> exp = function
  | RegVal_Base (Val_Symbolic i) -> get_var vc i
  | RegVal_Base (Val_Bool b) -> Typed.bool b
  | RegVal_Base (Val_Bits bv) -> Typed.bits_smt bv
  | RegVal_I (int, size) -> Typed.bits_int ~size int
  | RegVal_Base (Val_Enum (n, a)) -> Typed.enum (n, a)
  | valu ->
      Raise.fail "%t Can't convert %t to a trace expression" (Pp.tos Ast.pp_lrng l)
        (Pp.tos Isla.pp_valu valu)

(** Write an expression to an {!Isla.valu} *)
let write_to_valu vc valu exp =
  match valu with Isla.(RegVal_Base (Val_Symbolic i)) -> HashVector.set vc i exp | _ -> ()

(** Convert an isla event to Trace events, most events are deleted *)
let events_of_isla ~segments_map ~written_registers ~read_counter ~(vc : value_context) :
    Isla.revent -> event list = function
  | Smt (DeclareConst (i, ty), _) ->
      ( match HashVector.get_opt segments_map i with
        | Some (name, size) ->
            let ty_match = match ty with
            | Ty_BitVec sz -> size = sz
            | _ -> false
            in
            if not ty_match then fatal "Variable type doesn't match instruction segment %t and %d" (Pp.top Isla.pp_ty ty) size
            else
              HashVector.set vc i (Exp.of_var (Var.Segment (name, size)))
        | None ->
            try
              match ty with
              | Ty_BitVec ((8 | 16 | 32 | 64 | 128) as size) ->
                  HashVector.set vc i (Exp.of_var (Var.NonDet (i, Ast.Size.of_bits size)))
              | Ty_BitVec _ | Ty_Bool | Ty_Enum _ | Ty_Array (_, _) ->
                  debug "Unimplemented: ignoring non-det variable %i of type %t" i
                    (Pp.top Isla.pp_ty ty)
            with OfIslaError -> warn "not setting nondet:%d" i
      );
      []
  | Smt (DefineConst (i, e), _) ->
      ( try HashVector.set vc i (exp_conv_subst vc e)
        with OfIslaError -> warn "not setting v%d" i
      );
      []
  | Smt (Assert e, _) -> [Assert (exp_conv_subst vc e)]
  | Smt (DefineEnum _, _) -> []
  | ReadReg (name, al, valu, _) ->
      let string_path = Isla.Manip.string_of_accessor_list al in
      let valu = Isla.Manip.valu_get valu string_path in
      Isla.Manip.iter_valu_path (fun string_path valu ->
          let reg = State.Reg.of_path (name :: string_path) in
          (* If the register was already written, we use that value, otherwise, we read a
             symbolic variable *)
          ( match Hashtbl.find_opt written_registers reg with
            | Some exp -> write_to_valu vc valu exp
            | None -> write_to_valu vc valu (Exp.of_reg reg)
          )
        ) string_path valu;
      []
  | WriteReg (name, al, valu, l) ->
      let string_path = Isla.Manip.string_of_accessor_list al in
      let valu = Isla.Manip.valu_get valu string_path in
      Isla.Manip.map_valu_path (fun string_path valu ->
          let reg = State.Reg.of_path (name :: string_path) in
          let value = exp_of_valu l vc valu in
          Hashtbl.add written_registers reg value;
            WriteReg { reg; value }
        ) string_path valu
  | ReadMem (result, _kind, addr, size, _tag_opt, l) ->
      let addr = exp_of_valu l vc addr |> Typed.extract ~last:(Arch.address_size - 1) ~first:0 in
      let size = Ast.Size.of_bytes size in
      let value = Counter.get read_counter in
      write_to_valu vc result (Exp.of_var @@ Var.Read (value, size));
      [ReadMem { addr; size; value }]
  | WriteMem (_success, _kind, addr, data, size, _tag_opt, l) ->
      let addr = exp_of_valu l vc addr |> Typed.extract ~last:(Arch.address_size - 1) ~first:0 in
      let size = Ast.Size.of_bytes size in
      let value = exp_of_valu l vc data in
      [WriteMem { addr; size; value }]
  | Cycle _ -> []
  | Branch _ -> []
  | BranchAddress _ -> []
  | WakeRequest _ -> []
  | MarkReg _ -> []
  | SleepRequest _ -> []
  | Sleeping _ -> []
  | Instr _ -> []
  | Barrier _ -> []
  | CacheOp _ -> []
  (* Not currently used in read-dwarf (but are in isla-coq) *)
  | AssumeReg _ -> []
  | Assume _ -> []
  (* Not currently used in read-dwarf (not sure if in isla-coq) *)
  | FunAssume _ -> []
  | UseFunAssume _ -> []
  | AbstractCall _ -> []
  | AbstractPrimop _ -> []

(** Top level function to convert an isla trace to one of this module *)
let of_isla (segments: Isla.segment list) (Trace events : Isla.rtrc) : t =
  let written_registers = Hashtbl.create 10 in
  let read_counter = Counter.make 0 in
  let vc = HashVector.empty () in
  let segments_map = HashVector.empty () in
  List.iter (fun (Isla.Segment (name, size, var)) -> 
    HashVector.set segments_map var (name, size)
  ) segments;
  List.concat_map (events_of_isla ~segments_map  ~written_registers ~read_counter ~vc) events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace simplification } *)

(** A instance of {!Z3.ContextCounter}. *)
module SimpContext = Z3.ContextCounter (struct
  let str = "Trace simplification"
end)

module Z3Tr = Z3.Make (Var)
module VarTbl = Hashtbl.Make (Var)

let declare_non_det serv events =
  let declared = VarTbl.create 10 in
  iter_var
    (function
      | Register _ | Read _ -> ()
      | NonDet _ | Segment _ as var ->
          if not @@ VarTbl.mem declared @@ var then begin
            Z3Tr.declare_var_always serv var;
            VarTbl.add declared var ()
          end)
    events

(** Simplify a trace by using Z3. Perform both local expression simplification and
    global assertion removal (when an assertion is always true) *)
let simplify events =
  let serv = Z3.ensure_started_get () in
  let exp_simplify exp : exp = Z3Tr.simplify serv exp in
  let event_simplify = function
    | WriteReg wr -> Some (WriteReg { wr with value = exp_simplify wr.value })
    | ReadMem rm ->
        Z3Tr.declare_var_always serv (Read (rm.value, rm.size));
        Some (ReadMem { rm with addr = exp_simplify rm.addr })
    | WriteMem wm ->
        Some (WriteMem { wm with addr = exp_simplify wm.addr; value = exp_simplify wm.value })
    | Assert exp -> (
        let nexp = exp_simplify exp in
        match Z3Tr.check_both serv nexp with
        | Some true -> None
        | Some false -> Raise.fail "TODO implement trace deletion in that case"
        | _ ->
            Z3Tr.send_assert serv nexp;
            Some (Assert nexp)
      )
  in
  SimpContext.openc ();
  (* declare all registers *)
  State.Reg.iter (fun _ reg _ -> Z3Tr.declare_var_always serv (Register reg));
  declare_non_det serv events;
  let events = List.filter_map event_simplify events in
  SimpContext.closec ();
  events
