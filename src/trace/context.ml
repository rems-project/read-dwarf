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

(** This module provide the type for a context to run a trace

    Any information that should be required to run a trace but is not part of the state itself
    should be added here
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module SMap = Map.Make (String)

(** The context to run a trace *)
type t = {
  reg_writes : (State.Reg.t * State.tval) Vec.t;  (** Stores the delayed register writes *)
  mem_reads : State.tval HashVector.t;  (** Stores the result of memory reads *)
  nondets : State.var HashVector.t;  (** Stores the mapping of nondet variables *)
  state : State.t;
  segments : State.exp SMap.t;
  asserts: State.exp list;
  dwarf : Dw.t option;  (** Optionally DWARF information. If present, typing is enabled *)
}

(** Build a {!context} from a state *)
let make_context ?dwarf ?relocation state =
  let reg_writes = Vec.empty () in
  let mem_reads = HashVector.empty () in
  let nondets = HashVector.empty () in

  let segments, asserts = relocation
    |> Option.map (fun relocation ->
      let State.Relocation.{value;asserts;target} = State.Relocation.of_elf relocation in
      List.iter (State.push_relocation_assert state) asserts;

      (target
      |> Isla.Relocation.segments_of_reloc
      |> SMap.of_list
      |> SMap.map (fun (first, last) -> Exp.Typed.extract ~first ~last value),
      asserts)
      )
    |> Option.value ~default:(SMap.empty, [])
  in
  { state; reg_writes; mem_reads; nondets; dwarf; segments; asserts }

(** Expand a Trace variable to a State expression, using the context *)
let expand_var ~(ctxt : t) (v : Base.Var.t) (a : Ast.no Ast.ty) : State.exp =
  assert (Base.Var.ty v = a);
  match v with
  | Register reg -> State.get_reg_exp ctxt.state reg
  | NonDet (i, sz) -> HashVector.get_opt ctxt.nondets i
    |> Option.value_fun ~default:(fun () -> 
      Fun.tee (HashVector.add ctxt.nondets i) (State.Var.new_nondet sz)
    )
    |> State.Exp.of_var
  | Read (i, _) -> (HashVector.get ctxt.mem_reads i).exp (* TODO is the NonDet case correct *)
  | Segment (name, _) -> SMap.find name ctxt.segments (*TODO put the actual value there*)
  (* | Segment (name, sz) -> Exp.Typed.extract ~first:0 ~last:(sz-1) (State.Exp.of_var (State.Var.Section name)) TODO put the actual value there *)

(** Tell if typing should enabled with this context *)
let typing_enabled ~(ctxt : t) = ctxt.dwarf <> None

module Z3St = State.Simplify.Z3St

let simplify ~(ctxt : t) (exp : State.exp) : State.exp =
  debug "Before simplification: %t" (Pp.top State.Exp.pp exp);
  debug "Before simplification: %t" (Pp.top State.Exp.pp (Z3St.simplify_full exp));
  exp
  |> Z3St.simplify_subterms_full ~hyps:ctxt.asserts
  |> Z3St.simplify_full
  |> Fun.tee (fun e -> debug "After simplification: %t" (Pp.top State.Exp.pp e))