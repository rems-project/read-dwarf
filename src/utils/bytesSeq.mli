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

(** This module represent a byte sub view on a [bytes] object.
    Contrary to [Bytes] it is a non-owning immutable view.
    It do not prevent the original bytes from being modified,
    and the changes will be propagated in the view.
    It is additional sugar on top of Linksem's [Byte_sequence_wrapper]

    About all the suffixed function:
    - All iteration function without suffix do the expected operation on char (as single bytes)
    - All iteration function with suffix nle do the expected operation on a sequence of
      integers of n bits as read in little endian.
    - All iteration function with suffix nbe do the expected operation on a sequence of
      integers of n bits as read in big endian.
    - All iteration function with suffix bs do the expected operation on a sequence of
      BytesSeq.t of specified length.
    - All iteration function with suffix bvle do the expected operation on a sequence of
      {!BitVec} the specified [size] as read in little endian.

*)

(** Type inherited from [linksem] *)
type t = Byte_sequence_wrapper.byte_sequence

(** Get the length of the byteseq in bytes *)
val length : t -> int

(** Check if two byte sequence are equal byte for byte *)
val equal : t -> t -> bool

(** The size in bytes of an Ocaml int *)
val int_bytes : int

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Hexadecimal conversions } *)

(** Convert the byte sequence to an hexadecimal string *)
val to_hex : t -> string

(** Convert the byte sequence to an reversed hexadecimal string.
    This will print it like a big-endian integer. *)
val to_hex_rev : t -> string

(** Parse the string as hexadecimal like A4B767DF and create a bytes of this a
    binary data and then a bytesSeq view of it *)
val of_hex : string -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Cutting the view } *)

(** [sub bs start len] Extract a sub range [\[start:start+len)] of a byte sequence. This is O(1) *)
val sub : t -> int -> int -> t

(** [front i bs] Take the first [i] bytes of [bs] and discard the rest.
    Equivalent to [sub bs 0 i] *)
val front : int -> t -> t

(** [back i bs] Take the last [i] bytes of [bs] and discard the rest.
    Equivalent to [sub bs i (length bs - i)] *)
val back : int -> t -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Interaction with [bytes] and raw string } *)

(** [blit src srcoff dst dstoff len] copies [len] bytes from bytes sequence
   [src], starting at index [srcoff], to bytes [dst], starting at index
   [dstoff].

    See
   {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Bytes.html#VALblit}[Bytes.blit]}.
*)
val blit : t -> int -> bytes -> int -> int -> unit

(** Create a view of the whole bytes *)
val of_bytes : bytes -> t

(** Create a view of the whole string as raw bytes *)
val of_string : string -> t

(** Create a copy of the view in a string *)
val to_string : t -> string

(** Create a byte sequence view of a specified range of a bytes. See {!sub}*)
val bytes_sub : bytes -> int -> int -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Array conversions } *)

(** Convert to a char array *)
val to_array : t -> char array

(** Convert from a char array *)
val of_array : char array -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Getters } *)

(** Get the bytes at the offset in the byte sequence *)
val get : t -> int -> char

(** Unsafe version of {!get} *)
val unsafe_get : t -> int -> char

(** Get a 16 bit integer at the offset in the byte sequence as little endian *)
val get16le : t -> int -> int

(** Get a 16 bit integer at the offset in the byte sequence as big endian *)
val get16be : t -> int -> int

(** Get a 32 bit integer at the offset in the byte sequence as little endian *)
val get32le : t -> int -> int32

(** Get a 32 bit integer at the offset in the byte sequence as big endian *)
val get32be : t -> int -> int32

(** Get a 64 bit integer at the offset in the byte sequence as little endian *)
val get64le : t -> int -> int64

(** Get a 64 bit integer at the offset in the byte sequence as big endian *)
val get64be : t -> int -> int64

(** Get an Ocaml int at the offset in the byte sequence as little endian.
    The number of bytes read is 4 if [Sys.int_size] is 31 and 8 if [Sys.int_size] is 63 *)
val getintle : t -> int -> int

(** Get a byte sequence of length [len] at the offset in another byte sequence *)
val getbs : len:int -> t -> int -> t

(** Get a {!BitVec} of size [size] at the offset in the byte sequence as little endian *)
val getbvle : size:int -> t -> int -> BitVec.t

(** Get an Ocaml int at the offset in the byte sequence as little endian.
    The number of bytes read is 4 if [Sys.int_size] is 31 and 8 if [Sys.int_size] is 63.
    If the read goes beyond the end of the sequence, instead of failing, zeros are read.*)
val getintle_ze : t -> int -> int

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Iterators }

    Iterators over a byte sequence. If the length of the byte sequence is not a
    multiple of the step of the iteration then the trailing odd bytes are not
    iterated over.*)

val iter : (char -> unit) -> t -> unit

val iter16le : (int -> unit) -> t -> unit

val iter16be : (int -> unit) -> t -> unit

val iter32le : (int32 -> unit) -> t -> unit

val iter32be : (int32 -> unit) -> t -> unit

val iter64le : (int64 -> unit) -> t -> unit

val iter64be : (int64 -> unit) -> t -> unit

(** Iterate over the byte sequence by bytesequence of length [len]. If the total byte sequence
    is not of length a multiple of [len] then that iterated value will be shorter *)
val iterbs : len:int -> (t -> unit) -> t -> unit

val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a

val fold_left16le : ('a -> int -> 'a) -> 'a -> t -> 'a

val fold_left16be : ('a -> int -> 'a) -> 'a -> t -> 'a

val fold_left32le : ('a -> int32 -> 'a) -> 'a -> t -> 'a

val fold_left32be : ('a -> int32 -> 'a) -> 'a -> t -> 'a

val fold_left64le : ('a -> int64 -> 'a) -> 'a -> t -> 'a

val fold_left64be : ('a -> int64 -> 'a) -> 'a -> t -> 'a

val fold_leftbs : len:int -> ('a -> t -> 'a) -> 'a -> t -> 'a

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 List conversions } *)

val to_list : t -> char list

val to_list16le : t -> int list

val to_list16be : t -> int list

val to_list32le : t -> int32 list

val to_list32be : t -> int32 list

val to_list64le : t -> int64 list

val to_list64be : t -> int64 list

(** Cut a byte sequence into a list of byte sequences of length [len],
    (and a shorter last one if the total len is not a multiple of len) *)
val to_listbs : len:int -> t -> t list

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Binary IO } *)

(** Output the raw data of the byte sequence on the output channel *)
val output : out_channel -> t -> unit

(** Output the raw date of the byte sequence of the input channel *)
val input : in_channel -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty Printing } *)

(** Pretty print a byte sequence as space separated bytes like [ab cd ef].

    Here "ab" is the byte number 0 and "ef" is the byte number 2.
*)
val pp : t -> Pp.document

(** Pretty print a byte sequence as an hexadecimal string like [abcdef]

    Here "ab" is the byte number 0 and "ef" is the byte number 2.

    This can also be seen as printing the bytesequence as a single integer encoded
    in big endian format.
*)
val ppc : t -> Pp.document

(** Pretty print a byte sequence as an hexadecimal integer (in little endian).
    The byte order is reversed compared to {!ppc}

    For example the byte sequence [ab cd ef] will be printed as [efcdab]
    where "ab" is the byte number 0 and "ef" is the byte number 2.*)
val ppint : t -> Pp.document

(** Pretty print a byte sequence by step of [by] bytes. Each block is pretty
   printed as an hex string like {!ppc} and blocks are separated by spaces. *)
val ppby : by:int -> t -> Pp.document

(** Pretty print a byte sequence by step of [by] bytes. Each block is pretty
    printed as a reversed hex string i.e like an integer of length [by].
    Thus each block will printed like with {!ppcint}
    Blocks are separated by spaces.

    For example to print a byte sequence as a space separated list of little-endian integers do:

    {[ppbyint ~by:4 bs]} *)
val ppbyint : by:int -> t -> Pp.document
