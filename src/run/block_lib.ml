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

(** This module provides a representation of a complex block of code.

    The end of the block is decided by [endpred], an arbitrary predicate on the
    pc. In particular if the PC is symbolic the execution is stopped anyway. This
    means that we either reached the top level function return or an unresolved
    branch table.

    To generate easily end predicates, there is {!gen_endpred}.*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** [endpred pc_exp] gives when to stop *)
type t = { runner : Runner.t; start : Elf.Address.t; endpred : State.exp -> string option }

(** Build a complex block starting from [start] in [sym] and ending when [endpred] says so.
    [endpred] is a predicate on the symbolic PC expression *)
let make ~runner ~start ~endpred = { runner; start; endpred }

(** The labels on tree node at the output of {!run} *)
type label =
  | Start  (** Root node of the tree *)
  | End of string
      (** Lead node of the tree, the string describe which end condition has be triggered *)
  | BranchAt of Elf.Address.t  (** A Branching node at a given PC *)
  | NormalAt of Elf.Address.t  (** A normal instruction at PC. Exists only if [every_instruction] is true *)

let label_to_string = function
  | Start -> "Start"
  | End s -> Printf.sprintf "End (%s)" s
  | BranchAt pc -> Printf.sprintf "Branch at %t" Pp.(tos Elf.Address.pp pc)
  | NormalAt pc -> Printf.sprintf "Normal at %t" Pp.(tos Elf.Address.pp pc)

let pp_label label = label |> label_to_string |> Pp.string

(** Run the block an return a state tree indexed by the addresses of the
    branches.

    When [every_instruction] is true, It will make a snapshot of the state i.e a
    tree node at each instruction. By default it will only make a Tree node on
    branching points.

    The output is a tree because state merging is not implemented so if we are
    going twice on the same PC, the whole thing will be run twice separately in
    two separate tree branches. *)
let run ?(every_instruction = false) ?relevant (b : t) (start : State.t) : label State.Tree.t =
  let pcreg = Arch.pc () in
  assert (State.is_locked start);
  let rec run_from state =
    let pc_exp = State.get_reg_exp state pcreg in
    State.Simplify.ctxfull state;
    if State.is_possible state then
      match b.endpred pc_exp with
      | Some endmsg ->
          info "Stopped at pc %t because %s" (Pp.top State.Exp.pp pc_exp) endmsg;
          (* State.Simplify.ctxfull state; *)
          State.lock state;
          State.Tree.{ state; data = End endmsg; rest = [] }
      | None -> (
          (* let prelock state = State.Simplify.ctxfull state in *)
          if every_instruction then begin
            (* prelock state; *)
            State.lock state
          end;
          let states =
            let pc = State.Exp.expect_sym_address pc_exp in
            if Option.fold ~none:true ~some:(Fun.flip Hashtbl.mem pc) relevant then (
              info "Running pc %t" (Pp.top State.Exp.pp pc_exp);
              Runner.run ~prelock:ignore b.runner state
            )
            else (
              info "Skipping pc %t" (Pp.top State.Exp.pp pc_exp);
              Runner.skip b.runner state
            )
          in
          debug "After locking";
          match states with
          | [] -> Raise.fail "Reached a exceptional instruction"
          | [state] when not every_instruction -> run_from state
          | [nstate] when every_instruction ->
              let rest = [run_from nstate] in
              { state; data = NormalAt (State.Exp.expect_sym_address pc_exp); rest }
          | states ->
              let rest = List.map run_from states in
              State.Tree.
                { state; data = BranchAt (State.Exp.expect_sym_address pc_exp); rest }
        )
    else begin
      info "Reached dead code at %t" (Pp.top State.Exp.pp pc_exp);
      (* State.Simplify.ctxfull state; *)
      State.lock state;
      State.Tree.{ state; data = End "Reached dead code"; rest = [] }
    end
  in
  let state = State.copy start in
  State.set_pc_sym ~pc:pcreg state b.start;
  let rest = [run_from state] in
  State.Tree.{ state = start; data = Start; rest }

(** Generic end predicate. Will stop if:
    - pc below [min]
    - pc above [max]
    - pc is one of [brks]
    - pc has be seen more than [loop]
*)
let gen_endpred ?min ?max ?loop ?(brks = []) () : State.exp -> string option =
  let endnow fmt = Printf.ksprintf Option.some fmt in
  let pchtbl = Hashtbl.create 10 in
  let loop_str =
    match loop with
    | Some x when x <= 0 -> Raise.fail "Block.gen_endpred: non-positive loop number"
    | Some 1 -> "once"
    | Some 2 -> "twice"
    | Some n -> Printf.sprintf "%d times" n
    | None -> ""
  in
  fun pc_exp ->
    ( try
      Some (State.Exp.expect_sym_address pc_exp)
    with
      _ -> None
    ) |> Option.map (fun pc ->
      debug "enpred: Evaluating PC %t" (Pp.top Elf.Address.pp pc);
      match (min, max, loop) with
      | (Some min, _, _) when Elf.Address.(pc < min) <> Some false -> endnow "PC %t was below min %t" Pp.(tos Elf.Address.pp pc) Pp.(tos Elf.Address.pp min)
      | (_, Some max, _) when Elf.Address.(pc >= max) <> Some false -> endnow "PC %t was above max %t" Pp.(tos Elf.Address.pp pc) Pp.(tos Elf.Address.pp max)
      | _ when List.exists (( = ) pc) brks -> endnow "PC %t hit a breakpoint" Pp.(tos Elf.Address.pp pc)
      | (_, _, Some loop) ->
          let current_num = Hashtbl.find_opt pchtbl pc |> Option.value ~default:0 in
          if current_num >= loop then endnow "PC %t had been seen more than %s" Pp.(tos Elf.Address.pp pc) loop_str
          else begin
            Hashtbl.replace pchtbl pc (current_num + 1);
            None
          end
      | _ -> None
    ) |> Option.value_fun ~default:(fun () -> endnow "PC %t is symbolic" Pp.(tos State.Exp.pp pc_exp))
