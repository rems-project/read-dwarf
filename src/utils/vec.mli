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

(** This module provide a resizable array. It is a rename of Res.array with added features *)

(** The vector type *)
type 'a t

val length : 'a t -> int

val empty : unit -> 'a t

val mem : 'a -> 'a t -> bool

val get : 'a t -> int -> 'a

val unsafe_get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val unsafe_set : 'a t -> int -> 'a -> unit

val update : 'a t -> int -> ('a -> 'a) -> unit

val unsafe_update : 'a t -> int -> ('a -> 'a) -> unit

val copy : 'a t -> 'a t

val for_all : ('a -> bool) -> 'a t -> bool

val exists : ('a -> bool) -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** Iterates until the [limit]. If the vector is shorter, this is a plain {!iter} *)
val iter_until : limit:int -> ('a -> unit) -> 'a t -> unit

(** Same as {!iter_until} but with the index *)
val iteri_until : limit:int -> (int -> 'a -> unit) -> 'a t -> unit

(** Same as {!iteri} but starting at the end *)
val iteri_rev : (int -> 'a -> unit) -> 'a t -> unit

val to_list : 'a t -> 'a list

val to_listi : 'a t -> (int * 'a) list

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val foldi_right : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val foldi_left : (int -> 'b -> 'a -> 'b) -> 'a t -> 'b -> 'b

(** Add a new element at the end of the vector *)
val add_one : 'a t -> 'a -> unit

(** Remove the last element of the vector *)
val remove_one : 'a t -> unit

(** Remove the last n elements of the vector *)
val remove_n : 'a t -> int -> unit

val to_array : 'a t -> 'a array

val of_array : 'a array -> 'a t

(** Ensure that the vector has size at least the int by resizing if needed
    The value provided will be use to set the newly created values.*)
val ensure : 'a t -> int -> 'a -> unit

(** Keep only the specified number of element. If the vector was smaller, do nothing. *)
val keep : 'a t -> int -> unit

(** Resize the vector to the specified size.
    The value provided will be use to set the newly created values if relevant *)
val resize : 'a t -> int -> 'a -> unit

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** Map the function on the vector mutably by replacing each cell by the result of the function *)
val map_mut : ('a -> 'a) -> 'a t -> unit

(** Same as {!map_mut} but stop at [limit].
    If the vector is shorter than [limit] this is a plain {!map_mut} *)
val map_mut_until : limit:int -> ('a -> 'a) -> 'a t -> unit

(** Write the value in all the cells of the vector *)
val fill_all : 'a t -> 'a -> unit

(** Insert an element at the specified position that may be one past the end *)
val insert : 'a t -> int -> 'a -> unit

val to_seq_sub : 'a t -> pos:int -> len:int -> 'a Seq.t

val to_seq : 'a t -> 'a Seq.t

val to_seqi_sub : 'a t -> pos:int -> len:int -> (int * 'a) Seq.t

val to_seqi : 'a t -> (int * 'a) Seq.t

(** Vector pretty printer *)
val pp : ('a -> Pp.document) -> 'a t -> Pp.document

(** Vector pretty printer that also prints the index of each element using {!Pp.mapping} *)
val ppi : ('a -> Pp.document) -> 'a t -> Pp.document
