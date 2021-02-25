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

(** This module is for extending the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html}[Array]}
    module of the standard library *)

include Stdlib.Array

(** Mutate the array by replacing each value [x] by [f x] *)
let map_mut f arr =
  let len = length arr in
  for i = 0 to len - 1 do
    unsafe_get arr i |> f |> unsafe_set arr i
  done

(** [of_list_mapi f l = of_list (List.mapi f i l) = mapi f (of_list l)] *)
let of_list_mapi f = function
  | [] -> [||]
  | a :: t as l ->
      let first = f 0 a in
      let res = make (Stdlib.List.length l) first in
      let rec fill_from i = function
        | [] -> ()
        | a :: l ->
            unsafe_set res i (f i a);
            fill_from (i + 1) l
      in
      fill_from 1 t;
      res

(** [of_list_map f l = of_list (List.map f l) = map f (of_list l)] *)
let of_list_map f = of_list_mapi (Fun.const f)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Array scanning } *)

(** Find the first value satisfying the predicate and return it with its index.
    Throw [Not_found] if no value satisfies the predicate *)
let find_pair (pred : 'a -> bool) (arr : 'a array) : int * 'a =
  let len = length arr in
  let rec from n =
    if n >= len then raise Not_found
    else
      let v = unsafe_get arr n in
      if pred v then (n, v) else from (n + 1)
  in
  from 0

(** Find the first value satisfying the predicate.
    Throw [Not_found] if no value satisfies the predicate *)
let find pred arr = find_pair pred arr |> snd

(** Find the first index whose value satisfies the predicate.
    Throw [Not_found] if no value satisfies the predicate *)
let find_index pred arr = find_pair pred arr |> fst

(** Find all the values satisfying the predicate and return them with their index.*)
let find_all_pairs (pred : 'a -> bool) (arr : 'a t) : (int * 'a) list =
  let len = length arr in
  let res = ref [] in
  for i = len - 1 downto 0 do
    let v = unsafe_get arr i in
    if pred v then res := (i, v) :: !res
  done;
  !res

(** Find all the values satisfying the predicate*)
let find_all pred (arr : 'a t) : 'a list = find_all_pairs pred arr |> List.map snd

(** Find all the indices whose value satisfies the predicate*)
let find_all_indices pred (arr : 'a t) : int list = find_all_pairs pred arr |> List.map fst
