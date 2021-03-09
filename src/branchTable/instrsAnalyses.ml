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

(** This module is contains some analyses that you can perform on the
    instructions of a [Runner.t]. *)

open Logs.Logger (struct
  let str = __MODULE__
end)

module Runner = Run.Runner
module Reg = State.Reg
module Value = Exp.Value
module ConcreteEval = Exp.ConcreteEval
module Instr = Trace.Instr

type instructions = (int, Runner.slot) Hashtbl.t

type simple_branches = (int * bool * int) list

type reg_branches = (int * Reg.t list) list

type 'a branch = Simple of int * 'a | Reg of Reg.t list

let try_eval_target addr pc value =
  let pc_bv = Value.bv @@ BitVec.of_int ~size:64 addr in
  let module M = struct
    exception RegExn
  end in
  let ctxt = function
    | Trace.Var.Register reg -> if reg = pc then pc_bv else raise M.RegExn
    | Trace.Var.(NonDet _ | Read _) -> raise ConcreteEval.Symbolic
  in
  try
    (* PC-relative and absolute addresses are considered simple... *)
    Simple (ConcreteEval.eval ~ctxt value |> Value.expect_bv |> BitVec.to_int, ())
    (* ...all others considred indirect *)
  with M.RegExn ->
    let regs = ref [] in
    Ast.Manip.exp_iter_var
      (function Trace.Var.Register reg -> regs := reg :: !regs | Read _ | NonDet _ -> ())
      value;
    Reg !regs

let looks_like_a_branch { Instr.traces = trace_metas; length = _; read = _; written = _; opcode }
    addr =
  if Arch.is_ret opcode then None
  else
    let pc = Arch.pc () in
    let falls_thru =
      List.exists (fun tm -> Option.is_none tm.Instr.jump_target) trace_metas
      || (Option.is_some @@ Arch.is_bl opcode)
    in
    match
      trace_metas
      |> List.filter_map (fun tm -> tm.Instr.jump_target)
      |> List.map (try_eval_target addr pc)
    with
    | [] -> None
    | [Simple (int, ())] -> Some (Simple (int, falls_thru))
    | [Reg regs] -> Some (Reg regs)
    | _ :: _ :: _ -> fail "More than one write to PC across traces of instruction."

(** TODO Add tests *)
let find_branches instructions =
  let collect_simp_or_reg_branch addr slot ((simp_branches, reg_branches) as branches) =
    match slot with
    | Runner.Special _ | Nocode | IslaFail _ -> branches
    | Normal instr -> (
        match looks_like_a_branch instr addr with
        | Some (Simple (target, falls_thru)) ->
            ((addr, falls_thru, target) :: simp_branches, reg_branches)
        | Some (Reg regs) -> (simp_branches, (addr, regs) :: reg_branches)
        | None -> branches
      )
  in
  let (s, r) = Hashtbl.fold collect_simp_or_reg_branch instructions ([], []) in
  debug "BR: %t" Pp.(top (list ptr) @@ List.map fst r);
  (s, r)

(** TODO Add tests *)
let compute_comes_before instructions simp_branches =
  let comes_before = Hashtbl.create 10 in
  let cons_value_if_key_in_range key value =
    if Hashtbl.mem instructions key then
      match Hashtbl.find_opt comes_before key with
      | None -> Hashtbl.add comes_before key (value, [])
      | Some (x, xs) -> Hashtbl.replace comes_before key (value, x :: xs)
  in
  let add_next addr length =
    let next_instr_addr = addr + length in
    match List.find_opt (fun (addr', _, _) -> addr = addr') simp_branches with
    (* No simple branches at this address, falls-through to next instruction. *)
    | None -> cons_value_if_key_in_range next_instr_addr addr
    (* There is a simple branch at this address, it may fall-through to next instruction. *)
    | Some (_, falls_thru, target) ->
        cons_value_if_key_in_range target addr;
        if falls_thru then cons_value_if_key_in_range next_instr_addr addr
  in
  let add_targets_of addr slot =
    match slot with
    | Runner.Nocode -> ()
    | Special length | IslaFail length -> add_next addr length
    | Normal { traces = _; length; read = _; written = _; opcode } ->
        if not @@ Arch.is_ret opcode then add_next addr length
  in
  Hashtbl.iter add_targets_of instructions;
  comes_before

(** TODO Add tests *)
let track_spills instructions simp_branches =
  let next =
    let visited = Hashtbl.create 10 in
    let get addr len =
      match List.find_opt (fun (addr', _, _) -> addr = addr') simp_branches with
      | None -> [addr + len]
      | Some (_, falls_thru, other) -> if falls_thru then [addr + len; other] else [other]
    in
    fun addr len ->
      if Hashtbl.mem visited addr then []
      else (
        Hashtbl.add visited addr ();
        get addr len
      )
  in
  let is_an sps reg = List.mem Reg.( = ) reg sps in
  let is_reg = function Ast.Var (Trace.Var.Register reg, _) -> Some reg | _ -> None in
  let is_read = function Ast.Var (Trace.Var.Read (num, _), _) -> Some num | _ -> None in
  let is_sp sps exp = Option.fold ~none:false ~some:(is_an sps) (is_reg exp) in
  let is_stack_offset sps = function
    | Ast.Manyop
        ( Bvmanyarith Bvadd,
          [Bits (offset, _); Unop (Extract (_, _), Var (Trace.Var.Register reg, _), _)],
          _ ) ->
        if is_an sps reg then Some offset else None
    | Ast.Unop (Extract (_, _), Var (Trace.Var.Register reg, _), _) ->
        if is_an sps reg then Some (BitVec.one ~size:1) else None
    | _ -> None
  in
  let sp = Arch.sp () in
  let result = Hashtbl.create 10 in
  let rec loop sps spilled addr =
    let recurse_on length sps spilled = List.iter (loop sps spilled) @@ next addr length in
    match Hashtbl.find_opt instructions addr with
    | None | Some Runner.Nocode -> ()
    | Some (Special len | IslaFail len) -> recurse_on len sps spilled
    | Some (Normal { Instr.traces; length; read = _; written = _; opcode = _ }) -> (
        match traces with
        | [] | _ :: _ :: _ -> recurse_on length sps spilled
        | [{ Instr.trace; _ }] ->
            (* I do this to enforce mutual-exclusion of the 6 cases
               1. instruction does nothing related to spilling
               2. stack-pointer is copied to another register
               3. a (non-SP) register which previously held a stack-pointer is clobbered
                  (by a non-stack-pointer value)
               4. a register is loading from a stack offset that was previously spilled to
               5. a register is spilling to a stack offset (and its old value, if any, is clobbered)
               6. a stack offset that was spilled to is being clobbered *)
            let (set, get) =
              let set = ref false in
              let sps_spilled = ref (sps, spilled) in
              ( (fun x ->
                  assert (not !set);
                  sps_spilled := x),
                fun () -> !sps_spilled )
            in
            let reads = Hashtbl.create 10 in
            List.iter
              (function
                | Trace.ReadMem { addr; value; size = _ } ->
                    (* track mem reads for later reg writes *) Hashtbl.add reads value addr
                | WriteReg { reg; value = exp } ->
                    if is_sp sps exp then
                      (* 2. stack-pointer is copied to another register *)
                      set (reg :: sps, spilled)
                    else if Reg.(reg <> sp) && List.mem Reg.( = ) reg sps then
                      (* 3. a (non-SP) register which previously held a stack-pointer
                            is clobbered (by a non-stack-pointer value) *)
                      set (List.filter (Reg.( <> ) reg) sps, spilled)
                    else
                      ignore
                      @@ Option.(
                           let* num = is_read exp in
                           let* read_exp = Hashtbl.find_opt reads num in
                           let* offset = is_stack_offset sps read_exp in
                           let* (spill_addr, _, _) =
                             List.find_opt (fun (_, _, offset') -> offset = offset') spilled
                           in
                           (* 4. a register is loading from a stack offset that was
                                 previously spilled to *)
                           some @@ Hashtbl.add result addr spill_addr)
                | WriteMem { addr = loc; value; size = _ } -> (
                    match (is_stack_offset sps loc, is_reg value) with
                    | (Some stack_offset, Some reg) ->
                        (* 5. a register is spilling to a stack offset (and its
                              old value, if any, is clobbered) *)
                        set
                          ( sps,
                            (addr, reg, stack_offset)
                            :: List.filter (fun (_, _, x) -> x <> stack_offset) spilled )
                    | (Some stack_offset, None) ->
                        (* 6. a stack offset that was spilled to is being clobbered *)
                        set (sps, List.filter (fun (_, _, x) -> x <> stack_offset) spilled)
                    | (None, (None | Some _)) -> ()
                  )
                | Assert _ -> ())
              trace;
            let (sps, spilled) = get () in
            recurse_on length sps spilled
      )
  in
  let start_instr =
    Hashtbl.fold (fun key _ min -> if key < min then key else min) instructions Int.max_int
  in
  loop [Arch.sp ()] [] start_instr;
  debug "Spills:";
  Hashtbl.iter (fun ld spill -> debug "  load %#x from spill %#x" ld spill) result;
  result
