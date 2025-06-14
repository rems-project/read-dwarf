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

(** This module provide code to manipulate basic block and run them.

    This is only for use by {!BB} and debugging. I don't think this should be used
    for anything else. {!Block} should generally be used instead.*)

open Logs.Logger (struct
  let str = __MODULE__
end)

type trc = Trace.t

type state = State.t

(** Type of a basic block.

    The main part is the traces of all the non-branching instruction *)
type t = { main : trc array }

(** Take a binary block and call isla on all the instruction to get traces
    Also does the typing of traces for register discovery.
    TODO Support variable length instructions
*)
let from_binary (_code : BytesSeq.t) : t =
  Raise.todo()
  (* let num = BytesSeq.length code / 4 in
  (* TODO fix fixed size instructions *)
  if BytesSeq.length code != num * 4 then
    failwith "BB.from_binary: The specified range cuts an instruction";
  let process (code : BytesSeq.t) : trc =
    let get_normal : Isla.rtrc list -> trc = function
      | [] -> failwith "BB.from_binary: no normal path"
      | [trc] ->
          Isla.Type.type_trc trc |> ignore;
          Trace.of_isla trc
      | _ ->
          failwith
            "BB.from_binary: Multiple path instruction.\n\
             If this is not a branching instruction, try `run-block --linear'."
    in
    (code, None) |> Isla.Cache.get_traces |> get_normal (*TODO relocs *)
  in
  let main = code |> BytesSeq.to_listbs ~len:4 |> List.map process |> Array.of_list in
  { main } *)

(* Sequence of the second test:
mpool.c:116.6  (mpool_fini) 40012240:  37000049  tbnz
mpool.c:117.3  (mpool_fini) 40012244:  14000038  b
*)

(*$T from_binary
     (try ignore @@ from_binary @@ BytesSeq.of_hex "000"; false with Failure _ ->  true)
     (try ignore @@ from_binary @@ BytesSeq.of_hex ("37000049" ^ "14000038"); false with Failure _ -> true)
*)

(** Simplifies the traces in the basic block *)
let simplify_mut (bb : t) = Array.map_mut Trace.simplify bb.main

(** Run a linear basic block on a state by mutation.

    If [dwarf] is provided, the run is typed.*)
let run_mut ?dwarf state (bb : t) : unit = Array.iter (Trace.Run.trace_mut ?dwarf state) bb.main

(** Run a linear basic block on a trace and return a new state

    If [dwarf] is provided, the run is typed.*)
let run ?dwarf start (bb : t) : state =
  let state = State.copy start in
  run_mut ?dwarf state bb;
  State.lock state;
  state

(** Pretty print the basic block (The traces) *)
let pp (bb : t) = bb.main |> Array.to_list |> Pp.(separate_map (hardline ^^ hardline) Trace.pp)
