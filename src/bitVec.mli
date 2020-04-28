(** This module provides an interface for a bit vector of dynamic size

    TODO: It could be nice to export this as a separate library on opam at some point

    The value of type t is semantically pure.
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

(** The empty bitvector of size 0 *)
val empty : t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Integer conversions } *)

(** To a signed big integer *)
val to_z : t -> Z.t

(** To an unsigned big integer *)
val to_uz : t -> Z.t

(** Of bit integer. Wrapped modulo [size]. *)
val of_z : size:int -> Z.t -> t

(** To a signed integer.
    Fail if it doesn't fit *)
val to_int : t -> int

(** To an unsigned integer.
    Fail if it doesn't fit without wrapping i.e the result is still positive *)
val to_uint : t -> int

(** Of integer. Wrapped modulo [size]. *)
val of_int : size:int -> int -> t

(** Convert a one size bitvector to bool. Throw {!SizeMismatch} if the bitvector is not one-sized *)
val to_bool : t -> bool

(** Create a one-sized bitvector representing the boolean *)
val of_bool : bool -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bytes conversions } *)

(** Return the shortest bytes that represent the bitvector. missing bytes are zeros *)
val to_bytes : t -> bytes

(** Return a bytes representation of mininal length to encompass the whole bitvector size *)
val to_bytes_exact : t -> bytes

(** Read a bitvector from a bytes data *)
val of_bytes : size:int -> bytes -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 String conversions, printing } *)

(** Parse a string with specified [base] (10 if unspecified) and return a bitvector of size [size].
    If the string is too big, the integer is still parsed and then wrapped *)
val of_string : ?base:int -> size:int -> string -> t

(** Same as {!of_string} but on the substring starting at [pos] of length [len]. *)
val of_substring : ?base:int -> size:int -> pos:int -> len:int -> string -> t

(** Convert the value to a string representation in specified [base].

    [base] can only be 2, 8, 10 or 16, otherwise the function fails.

    Set [unsigned] to true to have unisigned values (signed by default).

    Set [prefix] to true to have the [0x/0o/0b] prefix (no prefix by default)
*)
val to_string : ?base:int -> ?unsigned:bool -> ?prefix:bool -> t -> string

(** Convert a bitvector in the SMTLib format to a {!t} *)
val of_smt : string -> t

(** Print a bitvector to the SMTLib format *)
val to_smt : t -> string

(** Print a bitvector to the SMTLib format *)
val pp_smt : t -> PP.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Arithmetic } *)

(** Add the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val add : t -> t -> t

(** Subtract the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val sub : t -> t -> t

(** Negate the value. Wrap if the value is the smaller integer *)
val neg : t -> t

(** Multiply the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val mul : t -> t -> t

(** Divide the value as signed integers. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ.

    Return 0 in case of division by 0 as it is undefined.
*)
val sdiv : t -> t -> t

(** Divide the value as unsigned integers. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val udiv : t -> t -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bit manipulation } *)

(** Bitwise and of the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logand : t -> t -> t

(** Bitwise or of the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logor : t -> t -> t

(** Bitwise xor of the values. Result if of the same size as the inputs.

    Throw {!SizeMismatch} if sizes differ
*)
val logxor : t -> t -> t

(** Bitwise not of the value. Result if of the same size as the input.*)
val lognot : t -> t

(** Do an or of all the bits in the bitvector *)
val redor : t -> bool

(** Do an and of all the bits in the bitvector *)
val redand : t -> bool

(** Do a left shift. The second argument must be non-negative *)
val shift_left : t -> int -> t

(** Same as {!shift_left} but the second argument is also a bitvector
    (of any size, but must fit on an int) *)
val shift_left_bv : t -> t -> t

(** Do an arithmetic right shift (copy the sign bit). The second argument must be non-negative *)
val shift_right_arith : t -> int -> t

(** Same as {!shift_right_arith} but the second argument is also a bitvector
    (of any size, but must fit on an int) *)
val shift_right_arith_bv : t -> t -> t

(** Do an logical right shift (insert zeroes). The second argument must be non-negative *)
val shift_right_logic : t -> int -> t

(** Same as {!shift_right_logic} but the second argument is also a bitvector
    (of any size, but must fit on an int) *)
val shift_right_logic_bv : t -> t -> t

(** Concatenates the bitvectors *)
val concat : t -> t -> t

(** [extract bv a b] extract bits a to b included from bv. Indexs starts at 0 *)
val extract : int -> int -> t -> t

(** Add the second argument of zero to the left *)
val zero_extend : int -> t -> t

(** Copy the bit sign as much as specified by the integer on the left *)
val sign_extend : int -> t -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Infix operators }

    Division do not have a operators because signed and unsigned division have different semantics
*)

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
