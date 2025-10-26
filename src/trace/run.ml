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

(** This module is for running trace from {!Trace} like {!Isla.Run} runs Isla traces.

    Due to the semantic of a register access being the register at the beginning of the trace,
    all register writes are not done immediately but delayed and stored in the {!context}.

    Typing is enabled if {!Context.typing_enabled} returns true for
    functions that take a context. For other functions, typing is enabled if the
    [dwarf] optional argument is passed
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module Ctxt = Context

type ctxt = Ctxt.t

(** Expand a {!Trace} expression to a {!State} expression, using the context *)
let expand ~(ctxt : ctxt) (exp : Base.exp) : State.exp =
  Ast.Manip.exp_var_subst (Ctxt.expand_var ~ctxt) exp

let expand_simplify ~(ctxt : ctxt) (exp : Base.exp) : State.exp =
  exp |> expand ~ctxt |> Context.simplify ~ctxt

(** Expand a Trace expression to a typed State expression, using the context.

    If the context enables typing, the expression will actually be typed,
    otherwise the type will be [None] *)
let expand_tval ~(ctxt : ctxt) (exp : Base.exp) : State.tval =
  let sexp = expand_simplify ~ctxt exp in
  if Ctxt.typing_enabled ~ctxt then
    let ctyp = Typer.expr ~ctxt exp in
    { ctyp; exp = sexp }
  else
    { ctyp = None; exp = sexp }

(** Run the event.
    The modified state is the one inside [ctxt]. *)
let event_mut ~(ctxt : ctxt) (event : Base.event) =
  debug "Running: %t with typing %s" (Pp.top Base.pp_event event)
    (if Ctxt.typing_enabled ~ctxt then "on" else "off");
  match event with
  | WriteReg { reg; value } -> Vec.add_one ctxt.reg_writes (reg, expand_tval ~ctxt value)
  | ReadMem { addr; value; size } ->
      let naddr = expand_simplify ~ctxt addr in
      debug "naddr: %t" (Pp.top State.Exp.pp naddr);
      let ptrtype = Typer.expr ~ctxt addr in
      debug "ptrtype: %t" Pp.(top (optional Ctype.pp) ptrtype);
      let tval =
        match ctxt.dwarf with
        | Some dwarf ->
            Typer.read ~dwarf ctxt.state ?ptrtype ~addr:naddr ~size
        | None ->
            State.read_noprov ctxt.state ~addr:naddr ~size |> State.Tval.of_exp
      in
      debug "read value: %t" Pp.(top State.Tval.pp tval);
      HashVector.set ctxt.mem_reads value tval
  | WriteMem { addr; value; size } -> (
      let naddr = expand_simplify ~ctxt addr in
      debug "naddr: %t" (Pp.top State.Exp.pp naddr);
      let ptrtype = Typer.expr ~ctxt addr in
      debug "ptrtype: %t" Pp.(top (optional Ctype.pp) ptrtype);
      match ctxt.dwarf with
      | Some dwarf ->
          let ptrtype = Typer.expr ~ctxt addr in
          debug "Typed write mem with ptr:%t" (Pp.top (Pp.opt Ctype.pp) ptrtype);
          let value = expand_tval ~ctxt value in
          debug "written value: %t" Pp.(top State.Tval.pp value);
          Typer.write ~dwarf ctxt.state ?ptrtype ~addr:naddr ~size value
      | None ->
          let value = expand_simplify ~ctxt value in
          State.write_noprov ctxt.state ~addr:naddr ~size value
    )
  | Assert exp -> State.push_assert ctxt.state (expand_simplify ~ctxt exp)

(** Run a trace on the provided state by mutation. Enable typing if [dwarf] is provided *)
let trace_mut ?dwarf ?relocation (state : State.t) (events : Base.t) : unit =
  assert (not @@ State.is_locked state);
  info "Running trace with typing %s" (if dwarf <> None then "on" else "off");
  let ctxt = Context.make_context ?dwarf ?relocation state in
  List.iter (event_mut ~ctxt) events;
  Vec.iter (fun (reg, tval) -> State.Reg.Map.set state.regs reg tval) ctxt.reg_writes

(** Run a trace on the provided state by returning an updated copy.*)
let trace ?dwarf ?relocation (start : State.t) (events : Base.t) : State.t =
  let state = State.copy start in
  trace_mut ?dwarf ?relocation state events;
  State.lock state;
  state

(** Run a trace by mutating the provided state including it's PC.
    If the trace modified the PC then nothing is done otherwise [next] is added to it.

    Thus this function automatically handle moving the PC for fall-through instruction
*)
let trace_pc_mut ?dwarf ?relocation ~(next : int) (state : State.t) (events : Base.t) : unit =
  let pc = Arch.pc () in
  let rec is_touching_pc : Base.t -> bool = function
    | [] -> false
    | WriteReg { reg; _ } :: _ when reg = pc -> true
    | _ :: l -> is_touching_pc l
  in
  trace_mut ?dwarf ?relocation state events;
  if is_touching_pc events then State.concretize_pc ~pc state else State.bump_pc ~pc state next
