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

include Stdlib.Option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Utility } *)

let take_first o o' = match o with Some _ -> o | None -> o'

let ( ||| ) = take_first

let take_first_list ol = List.fold_left ( ||| ) None ol

let take_all o o' = match (o, o') with (Some v, Some v') -> Some (v, v') | _ -> None

let ( &&& ) = take_all

let value_fail o fmt =
  match o with Some o -> Printf.ikfprintf (fun _ -> o) () fmt | None -> Raise.fail fmt

let value_fun o ~default:f = match o with Some o -> o | None -> f ()

let of_bool ~some b = if b then Some some else None

let of_bool_fun ~some b = if b then Some (some ()) else None

let for_all p = function None -> true | Some a -> p a

let exists p = function None -> false | Some a -> p a

let guard b v = if b then Some v else None

let guardn b v = if b then None else Some v

let ( let+ ) o f = map f o

let ( and+ ) = ( &&& )

let ( let+! ) o f = iter f o

let ( let* ) o b = bind o b

let ( and* ) = ( and+ )

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Lists } *)

let lift l =
  List.fold_right
    (fun e l ->
      let+ e = e and+ l = l in
      e :: l)
    l (Some [])

let map_lift f l =
  List.fold_right
    (fun e l ->
      let+ fe = f e and+ l = l in
      fe :: l)
    l (Some [])

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pairs } *)

(** Lift a pair of option to an option of pair *)
let lift_pair (a, b) =
  let+ a = a and+ b = b in
  (a, b)

(** Unlift an option of pair to a pair of option *)
let unlift_pair = function None -> (None, None) | Some (a, b) -> (Some a, Some b)
