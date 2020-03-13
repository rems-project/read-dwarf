(** Like bytes, but for bit level manipulation.
    The underlying type is still bytes and thus the size has to be a multiple of 8.

    The indexing is little endian: bit 9 is least significant bit of byte 1
*)

module Int = IntBits

(** [check_index length i] Check that the index [i] is valid to index an bytes of length [length].
    Throw {!invalid_argument} if not *)
val check_index : int -> int -> unit

(** [check_range length i l] Check that the index [i;i+l) is valid in an bytes of length [length].
    Throw {!invalid_argument} if not *)
val check_range : int -> int -> int -> unit

(** Gives the length of a [bytes] in bits *)
val length : bytes -> int

(** Create a [bytes] large enough to store the specified amount of bits *)
val create : int -> bytes

(** Create a [bytes] large enough to store the specified amount of bits and
    initialize them as the boolean *)
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

(** [blit_to_int src isrc dest idest len] blits the bits in range [isrc;isrc+len] or src
    to the range [idest;idest + len) of dest and returns the result.
    See {!unsafe_blit_to_int}.*)
val blit_to_int : bytes -> int -> Int.t -> int -> int -> Int.t

(** Unsafe version of {!blit_of_int}*)
val unsafe_blit_of_int : Int.t -> int -> bytes -> int -> int -> unit

(** [blit_of_int src isrc dest idest len] blits the bits in range [isrc;isrc+len] or src
    to the range [idest;idest + len) of dest by mutation.
    See {!unsafe_blit_o_int}.*)
val blit_of_int : Int.t -> int -> bytes -> int -> int -> unit
