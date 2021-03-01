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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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

(** This module provide facility to run {!Isla} trace over {{!State}states}

    The main functions are {!trc} for pure interface and {!trc_mut} for imperative interface.

    {!RunError} will be thrown when something goes wrong.

    It is for testing purpose only, otherwise use {!Trace.Run}. Typing does not work,
    and some other expected behavior may not work either.

    This module can be considered deprecated/legacy.*)

[@@@ocaml.deprecated "Should not be used by new modules"]

open Base

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Exception that represent an Isla runtime error which should not happen *)
exception RunError of lrng * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | RunError (l, s) ->
        Some Pp.(sprint @@ prefix 2 1 (Ast.pp_lrng l ^^ !^": ") (!^"RunError: " ^^ !^s))
    | _ -> None)

(** Throw a run error with the string part as formated by the format string *)
let run_error l fmt = Printf.ksprintf (fun s -> raise (RunError (l, s))) fmt

(** The contex of value that associate isla variable numbers to state expression *)
type value_context = State.exp HashVector.t

(** Get the expression associated to the free variable.
    Throw {!RunError}, if the variable is no bound.
    The {!lrng} is for error reporting. *)
let get_var l vc i =
  match HashVector.get_opt vc i with
  | Some exp -> exp
  | None -> run_error l "v%d is not bound in %t" i (Pp.tos (HashVector.pp State.Exp.pp) vc)

(** Convert an {!Isla} expression to an {!Ast} by substituing
    all free variable with the bound expression in the {!value_context}.

    If a variable is not bound, throw {!RunError} *)
let exp_conv_subst (vc : value_context) (exp : rexp) : State.exp =
  let vconv i l = get_var l vc i in
  Conv.exp_add_type_var_subst vconv exp

(** Give the {!State.exp} that represents the input {!valu}.

    A symbolic variable i is represented by the expression bound to it
    in the provided {!value_context}.

    Newly created expression are located with the provided {!lrng}.

    If the value is not convertible to a state expression, throw a {!RunError} *)
let rec exp_of_valu l vc : valu -> State.exp = function
  | Val_Symbolic i -> get_var l vc i
  | Val_Bool b -> Exp.Typed.bool b
  | Val_Bits bv -> Exp.Typed.bits_smt bv
  | Val_I (int, size) -> Exp.Typed.bits_int ~size int
  | Val_Enum (n, a) -> Exp.Typed.enum (n, a)
  | Val_Vector list -> Exp.Typed.vec @@ List.map (exp_of_valu l vc) list
  | valu -> run_error l "Can't convert %t to a state expression" (Pp.tos pp_valu valu)

(** This function write an expression to symbolic variable.
    The write is ignored if the variable was already set because
    isla guarantee that it would be the same value (Trusting Isla here) *)
let write_to_var _ vc var exp = HashVector.set vc var exp

(** This function write an expression to an {!valu}.

    If the valu is a variable, it is added to the context, otherwise nothing happens. *)
let write_to_valu l vc valu exp =
  match valu with Val_Symbolic i -> write_to_var l vc i exp | _ -> ()

(** Run an event on {!State.t} and a {!value_context} by mutating both *)
let event_mut (vc : value_context) (state : State.t) : revent -> unit =
  let module Reg = State.Reg in
  function
  | Smt (DeclareConst (i, ty), l) ->
    (match ty with
     | Ty_BitVec (8 | 16 | 32 | 64 as size) ->
       write_to_var l vc i State.(Exp.of_var (Var.NonDet (i, Ast.Size.of_bits size)))
     | Ty_BitVec _|Ty_Bool|Ty_Enum _|Ty_Array (_, _) -> ())
  | Smt (DefineConst (i, e), l) -> (
      debug "Defining v%i with %t" i (Pp.top pp_exp e);
      (* If the vc_subst_full fails, that means that a variable was not defined,
           Which means a non-determinism exists in the spec (no uni-valued type supported).
           As we don't support non-determinism, we just also won't define the current variable
           that depend on non determinism. If a non deterministic value was written to
           a register or memory, then the system would actually fail at that point *)
      try write_to_var l vc i (exp_conv_subst vc e) with RunError _ -> ()
    )
  | Smt (Assert e, _) -> State.push_assert state (exp_conv_subst vc e)
  | Smt (DefineEnum _, _) -> ()
  | ReadReg (name, al, valu, l) ->
      debug "Reading Reg %s at %t from %t" name Pp.(top pp_accessor_list al) Pp.(top pp_valu valu);
      let string_path = Manip.string_of_accessor_list al in
      let valu = Manip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      let e : State.exp = (Reg.Map.get state.regs reg).exp in
      write_to_valu l vc valu e
  | WriteReg (name, al, valu, l) ->
      debug "Writing Reg %s at %t from %t" name Pp.(top pp_accessor_list al) Pp.(top pp_valu valu);
      let string_path = Manip.string_of_accessor_list al in
      let valu = Manip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      let exp : State.exp = exp_of_valu l vc valu in
      Reg.Map.set state.regs reg (State.Tval.make exp)
  | ReadMem (result, _kind, addr, size, _tag_opt, l) ->
      debug "Reading Mem";
      (* TODO stop ignoring kind and tag_opt *)
      let addr =
        exp_of_valu l vc addr |> Exp.Typed.extract ~last:(Arch.address_size - 1) ~first:0
      in
      let size = Ast.Size.of_bytes size in
      write_to_valu l vc result (State.read_noprov state ~addr ~size)
  | WriteMem (_success, _kind, addr, data, size, _tag_opt, l) ->
      debug "Writing Mem";
      (* TODO stop ignoring kind *)
      let addr =
        exp_of_valu l vc addr |> Exp.Typed.extract ~last:(Arch.address_size - 1) ~first:0
      in
      let size = Ast.Size.of_bytes size in
      let data = exp_of_valu l vc data in
      State.write_noprov state ~addr ~size data
  | Cycle _ -> () (* Nothing happens here, this is just a marker *)
  | Branch _ -> () (* Nothing happens here, this is just a marker *)
  | BranchAddress _ -> () (* Nothing happens here, this is just a marker *)
  | WakeRequest _ -> ()
  | SleepRequest _ -> ()
  | MarkReg _ -> ()
  | Sleeping _ -> ()
  | Barrier _ -> ()
  | CacheOp _ -> ()
  | Instr _ -> ()

(* I don't need that information at that stage *)

(** This function run an isla trace on a state by mutation.
    If a [vc] is provided, then it is used and mutated according to the trace.

    Any encountered branch are ignored and their assertion are added to the state *)
let trc_mut ?(vc = HashVector.empty ()) (state : State.t) (trc : rtrc) =
  assert (not @@ State.is_locked state);
  let (Trace events) = trc in
  List.iter (event_mut vc state) events

(** This function run an isla trace on a state and return the end state as a new state

    It is just a wrapper of {!run_trc_mut} that remove the imperative interface
    The new state is fresh and locked.
*)
let trc start trc =
  let state = State.copy start in
  trc_mut state trc;
  State.lock state;
  state
