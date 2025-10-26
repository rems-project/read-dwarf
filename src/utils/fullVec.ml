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

(* The documentation is in the mli file *)

open Fun

type 'a t = { vec : 'a Vec.t; mutable gen : int -> 'a }

let make gen =
  let vec = Vec.empty () in
  { vec; gen }

let copy fv = { fv with vec = Vec.copy fv.vec }

(* Fill vec until the index included *)
let rec fill_until fv n =
  let len = Vec.length fv.vec in
  if len <= n then begin
    Vec.add_one fv.vec (fv.gen len);
    fill_until fv n
  end
  else ()

let get fv i =
  if i < 0 then Raise.inv_arg "FullVector.get: Negative index";
  fill_until fv i;
  Vec.get fv.vec i

(*$Q get
   Q.small_nat (fun i -> let fv = make Fun.id in get fv i = i)
*)

let set fv i v =
  if i < 0 then Raise.inv_arg "FullVector.set: Negative index";
  if Vec.length fv.vec <= i then begin
    fill_until fv (i - 1);
    assert (Vec.length fv.vec = i);
    Vec.add_one fv.vec v
  end
  else Vec.set fv.vec i v

(*$Q set
    (Q.pair Q.pos_int Q.small_nat) (fun (v, i) -> let fv = make Fun.id in set fv i v; get fv i = v)
    Q.small_nat (fun i -> let fv = make Fun.id in set fv (i+1) 42; get fv i = i)
*)

let set_after fv i ngen =
  fill_until fv i;
  Vec.keep fv.vec i;
  fv.gen <- ngen

(*$Q set_after
    Q.small_nat (fun i -> let fv = make Fun.id in set_after fv i ((+) 1); get fv i = (i+1))
    Q.small_nat (fun i -> let fv = make Fun.id in set_after fv (i+1) ((+) 1); get fv i = i)
*)

let get_vec fv = fv.vec

let get_vec_until fv n =
  fill_until fv (n - 1);
  fv.vec

let map f fv =
  let vec = Vec.map f fv.vec in
  let gen = fv.gen %> f in
  { vec; gen }

let mapi f fv =
  let vec = Vec.mapi f fv.vec in
  let gen = (fun i -> fv.gen i |> f i) in
  { vec; gen }

let map_mut f fv =
  Vec.map_mut f fv.vec;
  fv.gen <- fv.gen %> f

let map_mut_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.map_mut f fv.vec

let iter_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.iter_until ~limit f fv.vec

let iteri_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.iteri_until ~limit f fv.vec

let pp conv fv =
  let open Pp in
  Vec.to_listi fv.vec |> List.map (Pair.map int conv) |> mapping ""
