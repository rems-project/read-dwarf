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

(** An hash vector allow a vector to behave as hash map indexed by small integers.
    It is hash map with the identity hash function.

    If all goes well it has the same API as [(int, 'a) Hashtbl.t] for positive integers.
    Any attempt to use a negative integer will raise [Invalid_argument].
*)

(** The type of the hash vector *)
type 'a t

(** Check if an element is set *)
val mem : 'a t -> int -> bool

(** Copy the HashVector *)
val copy : 'a t -> 'a t

exception Exists

(** Set a value. Create a new binding if necessary *)
val set : 'a t -> int -> 'a -> unit

(** Clear a binding, do nothing is the value is not bound. *)
val clear : 'a t -> int -> unit

(** Create a new binding. Throws {!Exists} if a binding already exists *)
val add : 'a t -> int -> 'a -> unit

(** Return [Some v] if v is bound to the integer in the hash vector.
    Return [None] if nothing is bound *)
val get_opt : 'a t -> int -> 'a option

(** Return the value bound to the integer. Throw [Not_found] if the integer is not bound *)
val get : 'a t -> int -> 'a

(** Create an empty hashVector.t *)
val empty : unit -> 'a t

(** Returns a list of bindings in the hash vector *)
val bindings : 'a t -> (int * 'a) list

(** Fill the hashVector from a sequence *)
val of_seq : (int * 'a) Seq.t -> 'a t

(** Pretty print a hashVector *)
val pp : ('a -> Pp.document) -> 'a t -> Pp.document
