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

(** This module contain random utility functions dealing with pairs *)

(** Map each function on one side of the pair *)
val map : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd

(** Iter each function on one side of the pair *)
val iter : ('a -> unit) -> ('b -> unit) -> 'a * 'b -> unit

(** Swap the element of a pair *)
val swap : 'a * 'b -> 'b * 'a

(** Compare a pair using provided comparison function. Both [fst] and [snd]
   default to the polymorphic compare *)
val compare : ?fst:('a -> 'a -> int) -> ?snd:('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int

(** Test pair equality using provided equality function. Both [fst] and [snd]
   default to the polymorphic equality *)
val equal : ?fst:('a -> 'a -> bool) -> ?snd:('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool

(** Just build the pair, not useful on it's own but [List.combine] is just
   [List.map2 Pair.make], and there are some other cases where it is handy for
   high-order programming *)
val make : 'a -> 'b -> 'a * 'b

(** Just build a pair with twice the element. Useful in high order programming *)
val split : 'a -> 'a * 'a

(** Check that both individual predicates hold *)
val for_all : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool

(** Check that at least one of the individual predicates holds *)
val exists : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool
