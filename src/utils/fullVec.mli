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

(** A full vector is a vector in which all non-negative integer are bound.

    This consist in a normal vector and a function and all the bindings of value after
    the vector end are the result of the function call on that integer.

    It is guaranted that the function will be called lazily on integer when required
    in stricly increasing order (and thus never twice on the same integer).
    However an integer may be skipped (When manually set). That means the function can have
    side effect if necessary. However if the structure is copied ({!map} or {!copy}),
    The generator may be called multiple time on some integer in the different copies.

    Both set and get may generate calls to the generator if required.

    Any attempt to use a negative integer will raise [Invalid_argument].

    A negative integer will never be passed to the generator.
*)

type 'a t

(** Create a full vector from a generator *)
val make : (int -> 'a) -> 'a t

(** Make a copy that can be mutated separately *)
val copy : 'a t -> 'a t

(** Set the binding of that integer *)
val set : 'a t -> int -> 'a -> unit

(** Set the binding of all integer after this value by supplying a new generator.
    The former generator is discarded. The bindings before the value
    keep their value (if there were not generated, they will be before discarding
    the old generator *)
val set_after : 'a t -> int -> (int -> 'a) -> unit

(** Get the binding of that integer *)
val get : 'a t -> int -> 'a

(** Get the underlying vector *)
val get_vec : 'a t -> 'a Vec.t

(** Get a vector containing at least the elements until the specified value excluded *)
val get_vec_until : 'a t -> int -> 'a Vec.t

(** Map the function over the fullvec. Postcompose the map on the generator *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Same as {!map} but with the index *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** Map the function over the fullvector by mutation. Postcompose the map on the generator.contents
    Warning, a lot of {!map_mut} may make the generator big and slow.
    Maybe try to use {!set_after} to reset it when required.*)
val map_mut : ('a -> 'a) -> 'a t -> unit

(** Map the function over the fullvector until the limit. The rest is unchanged *)
val map_mut_until : limit:int -> ('a -> 'a) -> 'a t -> unit

(** Iterate until the specified value (excluded).
    (The FullVec is infinite so you can't iter on all of it) *)
val iter_until : limit:int -> ('a -> unit) -> 'a t -> unit

(** Same as {!iter_until} but with the index *)
val iteri_until : limit:int -> (int -> 'a -> unit) -> 'a t -> unit

(** Only prints the non-default values *)
val pp : ('a -> Pp.document) -> 'a t -> Pp.document
