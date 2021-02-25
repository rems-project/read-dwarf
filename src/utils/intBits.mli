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

(** Manipulate an int as bitfield of size 31 or 63.

    I'm tired of having to think about bit shifts and bitwise operations when I do that stuff

    Little endian indexing (0 is the least significant bit)

    Ranges are specified with and index and a length. The index must be in [\[0,length)] and the
    length must be in [(0,length\]]. Those conditions will be named "valid range".
    Any range specified in that way can go after the end.
    When reading, it will behave as if it was zeroes,
    and on writes, all bits after the end are discarded.

    All unsafe function implicitely assume that all indexes and ranges are valid.
    All safe functions throw if those conditions are not met.
*)

(** The type of a int as a bitfield. This is just to make signatures clearer *)
type t = int

(** The length of an integer in bits ([Sys.int_size]) *)
val length : int

(** The last index in an integer *)
val back : int

(** Check that the index is valid to index an integer.

    Throw [Invalid_argument] if the index is not valid *)
val check_index : int -> unit

(** Check that the range is valid to index an integer.
    See module documentation ({!IntBits}) for the definition of a valid range.

    Throw [Invalid_argument] if the range is not valid. *)
val check_range : int -> int -> unit

(** Initialize an int with all zeros or all ones depending on the boolean *)
val init : bool -> t

(** Get a bit at a specific index. See {!unsafe_get} *)
val get : t -> int -> bool

(** Unsafe version of {!get} *)
val unsafe_get : t -> int -> bool

(** Set a bit at a specific index. See {!unsafe_set} *)
val set : t -> int -> t

(** Unsafe version of {!set} *)
val unsafe_set : t -> int -> t

(** Clear a bit at a specific index. See {!unsafe_clear} *)
val clear : t -> int -> t

(** Unsafe version of {!clear} *)
val unsafe_clear : t -> int -> t

(** Set a bit at a specific index according to a boolean. See {!unsafe_setb} *)
val setb : t -> int -> bool -> t

(** Unsafe version of {!setb} *)
val unsafe_setb : t -> int -> bool -> t

(** [mask i l] creates a mask stating at i of length l.
    This means that the bits of the output in the range [\[i; i+l)] are ones and the others are 0.

    See {!unsafe_mask}. *)
val mask : int -> int -> t

(** Unsafe version of {!mask} *)
val unsafe_mask : int -> int -> t

(** [set_range bf i l] sets the range [\[i; i+l)] to ones in [bf]. See {!unsafe_set_range}*)
val set_range : t -> int -> int -> t

(** Unsafe version of {!set_range} *)
val unsafe_set_range : t -> int -> int -> t

(** [clear_range bf i l] sets the range [\[i; i+l)] to zeroes in [bf]. See {!unsafe_set_range}*)
val clear_range : t -> int -> int -> t

(** Unsafe version of {!clear_range} *)
val unsafe_clear_range : t -> int -> int -> t

(** [sub bf i l] outputs the range [\[i; i+l)] of bf.
    The bits above l of the result are zeroes.

    See {!unsafe_sub} *)
val sub : t -> int -> int -> t

(** Unsafe version of {!sub} *)
val unsafe_sub : t -> int -> int -> t

(** [set_sub bf i l data] sets the [\[i; i+l)] range of [bf] to [data].
    The bits above [l] of [data] are ignored.

    See {!unsafe_set_sub} *)
val set_sub : t -> int -> int -> t -> t

(** Unsafe version of {!set_sub}. However the bits above l of data must be zeroes *)
val unsafe_set_sub : t -> int -> int -> t -> t

(** [unsafe_blit src isrc dest idest len] copies
    [\[isrc; isrc+len)] of [src] into [\[idest; idest +l)] of [dest].

    See {!unsafe_blit} *)
val blit : t -> int -> t -> int -> int -> t

(** Unsafe version of {!blit} *)
val unsafe_blit : t -> int -> t -> int -> int -> t
