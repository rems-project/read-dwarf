(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   *)
(*  BSD 2-Clause License. This license does not apply to files outside these        *)
(*  directories.                                                                    *)
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

(* The documentation is in the mli file *)

type 'a t = 'a option Vec.t

let mem hv k =
  if k < 0 then Raise.inv_arg "HashVector.mem: negative index %d" k;
  k < Vec.length hv && Vec.unsafe_get hv k != None

let copy = Vec.copy

exception Exists

let set hv k v : unit =
  if k < 0 then Raise.inv_arg "HashVector.set: negative index %d" k;
  if k >= Vec.length hv then Vec.ensure hv (k + 1) None;
  Vec.unsafe_set hv k (Some v)

let clear hv k =
  if k < 0 then Raise.inv_arg "HashVector.clear: negative index %d" k;
  if k < Vec.length hv then Vec.unsafe_set hv k None

let add (hv : 'a t) (k : int) (v : 'a) : unit = if mem hv k then raise Exists else set hv k v

let get_opt hv k =
  if k < 0 then Raise.inv_arg "HashVector.get_opt: negative index %d" k;
  if k >= Vec.length hv then None else Vec.unsafe_get hv k

let get hv k = match get_opt hv k with None -> raise Not_found | Some v -> v

let empty () : 'a t = Vec.empty ()

let bindings hv =
  Vec.foldi_right (fun i v l -> match v with Some vv -> (i, vv) :: l | None -> l) hv []

let of_seq (seq : (int * 'a) Seq.t) : 'a t =
  let res = empty () in
  let rec of_seq_aux seq =
    match seq () with
    | Seq.Nil -> res
    | Seq.Cons ((i, v), seq) ->
        add res i v;
        of_seq_aux seq
  in
  of_seq_aux seq

let pp conv hv = hv |> bindings |> List.map (Pair.map Pp.int conv) |> Pp.mapping "hv"
