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

(** This module is for extending the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Seq.html} Seq}
    module of the standard library *)

include Stdlib.Seq

(** Add a counter starting at [start] (default 0) in front of each element of the sequence *)
let add_count ?(start = 0) (s : 'a t) : (int * 'a) t =
  let rec aux i s () =
    match s () with Nil -> Nil | Cons (v, s') -> Cons ((i, v), aux (i + 1) s')
  in
  aux start s

(** Generate an integer sequence up to [len]. Optionally may start at [start] instead of 0 *)
let iota ?(start = 0) len : int t =
  let rec aux endi cur () = if cur < endi then Cons (cur, aux endi (cur + 1)) else Nil in
  aux (start + len) start

(** Generate an integer sequence up to [endi] by stepping [step].
    Optionally may start at [start] instead of 0 *)
let iota_step_up ?(start = 0) ~step ~endi : int t =
  let rec aux step endi cur () =
    if cur < endi then Cons (cur, aux step endi (cur + step)) else Nil
  in
  aux step endi start

(** Make the sequence stop when the condition is met *)
let rec stop_at f (s : 'a t) () =
  match s () with
  | Nil -> Nil
  | Cons (a, _) when f a -> Nil
  | Cons (a, s') -> Cons (a, stop_at f s')

(** Add a new element in front of the sequence. That element will appear first before
    the rest of the sequence.

    Added to [Stdlib] in Ocaml 4.11 *)
let cons (v : 'a) (s : 'a t) () = Cons (v, s)

(** Applies the specified function to the elements of the sequence in order,
    and returns the first result of the form [Some v], or [None] if no such result was returned.

    See {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALfind_map} [List.find_map]}
*)
let rec find_map f seq =
  match seq () with
  | Nil -> None
  | Cons (a, seq) ->
      let v = f a in
      if v = None then find_map f seq else v
