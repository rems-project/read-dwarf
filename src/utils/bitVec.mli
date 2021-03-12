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

(** This module provides an interface for a bit vector of dynamic size.

    For now this is entirely based on [zarith].

    TODO: It could be nice to export this as a separate library on opam at some point

    The value of type t is semantically pure and can be compare with polymorphic operators.
    It will compare the size first, then the value.

    The size of bit vectors must always be strictly positive.
*)

(** The type of a bitvector *)
type t

(** Raise when the runtime size do not match on operation that require so (like {!add}) *)
exception SizeMismatch of int * int

(** The size of the bitvector *)
val size : t -> int

(** The bitvector representing 0 of specified size *)
val zero : size:int -> t

(** The bitvector representing 1 of specified size *)
val one : size:int -> t

(** The bitvector representing -1 of specified size *)
val minus_one : size:int -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Integer conversions } *)

(** To a signed big integer *)
val to_z : t -> Z.t

(** To an unsigned big integer *)
val to_uz : t -> Z.t

(** Of bit integer. Wrapped modulo [2^size]. *)
val of_z : size:int -> Z.t -> t

(** To a signed integer.
    Fail if it doesn't fit *)
val to_int : t -> int

(** To an unsigned integer.
    Fail if it doesn't fit without wrapping i.e the result is still positive *)
val to_uint : t -> int

(** Of integer. Wrapped modulo [size]. *)
val of_int : size:int -> int -> t

(** Convert a one size bitvector to bool.
    Throw {!SizeMismatch} if the bitvector is not one-sized *)
val to_bool : t -> bool

(** Create a one-sized bitvector representing the boolean *)
val of_bool : bool -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bytes conversions } *)

(** Return the shortest bytes that represent the bitvector in little-endian.
    There may be extra bits (if size is not a multiple of 8) which are zeros.

    This bytes may be shorter that the bitvector size, for example the bitvector
    1 of size 64bits, will still be returned by this function as a single byte 1.
    For another behavior, see {!to_bytes_exact}.*)
val to_bytes : t -> bytes

(** Return a bytes representation of mininal length to encompass the whole bitvector size.
    Extra bits (if size is not a multiple of 8) are zeros.*)
val to_bytes_exact : t -> bytes

(** Store the bitvector in the bytes at the specified offset in little endian.
    The bitvector size must be a multiple of 8 or [Invalid_argument] is thrown *)
val bytes_store : bytes -> int -> t -> unit

(** Read a bitvector from a bytes data (little endian)*)
val of_bytes : size:int -> bytes -> t

(** Load a bitvector of [size] bits from the bytes at the specified offset (little endian).
    [size] must be a multiple of 8 or [Invalid_argument] is thrown *)
val bytes_load : size:int -> bytes -> int -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 String conversions, printing } *)

(** Parse a string with specified [base] (10 if unspecified) and return a bitvector of size [size].
    If the string is too big, the integer is still parsed and then wrapped modulo [2^size] *)
val of_string : ?base:int -> size:int -> string -> t

(** Same as {!of_string} but on the substring starting at [pos] of length [len]. *)
val of_substring : ?base:int -> size:int -> pos:int -> len:int -> string -> t

(** Convert the value to a string representation in the specified [base].

    [base] can only be 2, 8, 10 or 16, otherwise the function fails.

    Set [unsigned] to true to have unsigned values (signed by default).

    Set [prefix] to true to have the [0x/0o/0b] prefix (no prefix by default)

    Set [force_width] to false to not have a digit length matching the bitvector length,
    otherwise leading zeros will be inserted to match the length.
*)
val to_string : ?base:int -> ?unsigned:bool -> ?force_width:bool -> ?prefix:bool -> t -> string

(** Convert a bitvector in the SMTLib format to a {!t} *)
val of_smt : string -> t

(** Convert a bitvector to the SMTLib format *)
val to_smt : t -> string

(** Print a bitvector with the SMTLib format *)
val pp_smt : t -> Pp.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Arithmetic } *)

(** Add the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val add : t -> t -> t

(** Subtract the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val sub : t -> t -> t

(** Negate the value.
    Wrap if the value is the smaller integer (It will stay the smallest integer) *)
val neg : t -> t

(** Multiply the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val mul : t -> t -> t

(** Divide the values as signed integers. Result is of the same size as the inputs.

    It rounds the result toward zero.

    Throw {!SizeMismatch} if sizes differ.

    Throw {!Division_by_zero} if there is a division by zero.
*)
val sdiv : t -> t -> t

(** Take the remainder of the signed division. Result is of the same size as the inputs.

    [ a = sdiv a b * b + srem a b ]

    Throw {!SizeMismatch} if sizes differ.

    Throw {!Division_by_zero} if there is a division by zero.
*)
val srem : t -> t -> t

(** Take the signed modulo. The result has the sign of the divisor.
    Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ.

    Throw {!Division_by_zero} if there is a division by zero.
*)
val smod : t -> t -> t

(** Divide the values as unsigned integers. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ

    Throw {!Division_by_zero} if there is a division by zero.
*)
val udiv : t -> t -> t

(** Get the remainder of the unsigned division. Result if of the same size as the inputs.

    [ a = udiv a b * b + urem a b ]

    Throw {!SizeMismatch} if sizes differ

    Throw {!Division_by_zero} if there is a division by zero.
*)
val urem : t -> t -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bit manipulation } *)

(** Bitwise and of the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logand : t -> t -> t

(** Bitwise or of the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logor : t -> t -> t

(** Bitwise xor of the values. Result is of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logxor : t -> t -> t

(** Bitwise not of the value. Result is of the same size as the input.*)
val lognot : t -> t

(** Do an or of all the bits in the bitvector *)
val redor : t -> bool

(** Do an and of all the bits in the bitvector *)
val redand : t -> bool

(** Do a left shift. The second argument must be non-negative *)
val shift_left : t -> int -> t

(** Same as {!shift_left} but the second argument is also a bitvector of any
    size interpreted as unsigned*)
val shift_left_bv : t -> t -> t

(** Do an arithmetic right shift (copy the sign bit). The second argument must be non-negative *)
val shift_right_arith : t -> int -> t

(** Same as {!shift_right_arith} but the second argument is also a bitvector of
    any size interpreted as unsigned*)
val shift_right_arith_bv : t -> t -> t

(** Do an logical right shift (insert zeroes). The second argument must be non-negative *)
val shift_right_logic : t -> int -> t

(** Same as {!shift_right_logic} but the second argument is also a bitvector of
    any size interpreted as unsigned*)
val shift_right_logic_bv : t -> t -> t

(** Concatenates the bitvectors *)
val concat : t -> t -> t

(** [extract bv a b] extract bits [a] to [b] included from [bv]. Indices start at 0 *)
val extract : int -> int -> t -> t

(** Add the second argument of zeroes to the left *)
val zero_extend : int -> t -> t

(** Copy the bit sign as much as specified by the integer on the left *)
val sign_extend : int -> t -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Infix operators }

    Divisions do not have any operators because signed and unsigned division
    have different semantics *)

(** {!add} *)
val ( + ) : t -> t -> t

(** {!sub} *)
val ( - ) : t -> t -> t

(** {!mul} *)
val ( * ) : t -> t -> t

(** {!neg} *)
val ( ~- ) : t -> t

(** {!shift_left_bv} *)
val ( lsl ) : t -> t -> t

(** {!shift_left_bv} *)
val asl : t -> t -> t

(** {!shift_right_logic_bv} *)
val ( lsr ) : t -> t -> t

(** {!shift_right_arith_bv} *)
val ( asr ) : t -> t -> t

(** {!lognot} *)
val lnot : t -> t

(** {!logand} *)
val ( land ) : t -> t -> t

(** {!logor} *)
val ( lor ) : t -> t -> t

(** {!logxor} *)
val ( lxor ) : t -> t -> t
