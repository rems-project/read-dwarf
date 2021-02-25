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

(** Like bytes, but for bit level manipulation.
    The underlying type is still bytes and thus the size has to be a multiple of 8.

    The indexing is little endian: bit 9 is least significant bit of byte 1
*)

module Int = IntBits

(** [check_index length i] Check that the index [i] is valid to index an bytes of length [length].
    Throw [Invalid_argument] if not *)
val check_index : int -> int -> unit

(** [check_range length i l] Check that the range [\[i;i+l)] is inside a bytes of length [length].
    Throw [Invalid_argument] if not *)
val check_range : int -> int -> int -> unit

(** Gives the length of a [bytes] in bits *)
val length : bytes -> int

(** Create a [bytes] large enough to store the specified amount of bits *)
val create : int -> bytes

(** Create a [bytes] large enough to store the specified amount of bits and
    initialize them as specified by the boolean *)
val make : int -> bool -> bytes

(** Unsafe version of {!get} *)
val unsafe_get : bytes -> int -> bool

(** Get a bit at a specific index. See {!unsafe_get} *)
val get : bytes -> int -> bool

(** Unsafe version of {!set} *)
val unsafe_set : bytes -> int -> unit

(** Set a bit at a specific index. See {!unsafe_set} *)
val set : bytes -> int -> unit

(** Unsafe version of {!clear} *)
val unsafe_clear : bytes -> int -> unit

(** Clear a bit at a specific index. See {!unsafe_clear} *)
val clear : bytes -> int -> unit

(** Unsafe version of {!setb} *)
val unsafe_setb : bytes -> int -> bool -> unit

(** Set a bit at a specific index according to a boolean. See {!unsafe_setb} *)
val setb : bytes -> int -> bool -> unit

(** Unsafe version of {!blit_to_int} *)
val unsafe_blit_to_int : bytes -> int -> Int.t -> int -> int -> Int.t

(** [blit_to_int src isrc dest idest len] blits the bits in range [\[isrc;isrc+len)] of [src]
    to the range [\[idest;idest + len)] of [dest] and returns the result.
    See {!unsafe_blit_to_int}.*)
val blit_to_int : bytes -> int -> Int.t -> int -> int -> Int.t

(** Unsafe version of {!blit_of_int}*)
val unsafe_blit_of_int : Int.t -> int -> bytes -> int -> int -> unit

(** [blit_of_int src isrc dest idest len] blits the bits in range [\[isrc;isrc+len)] or src
    to the range [\[idest;idest + len)] of dest by mutation.
    See {!unsafe_blit_of_int}.*)
val blit_of_int : Int.t -> int -> bytes -> int -> int -> unit
