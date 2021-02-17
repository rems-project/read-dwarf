include Ott

(** Generic empty type. This kind of type
    is explained {{:http://caml.inria.fr/pub/docs/manual-ocaml/emptyvariants.html}here}.
    In particular, when this type appear in a match case,
    the match case can contain a refutation pattern [.] to indicate that this impossible.

    A refutation pattern also work with higher-level constructors, for example in this case:
    {[
    type a = A of int | B of no * int
    let f : a -> int = function
      | A i -> i
      | B _ -> .
    ]}

    It also work in product types:

    {[ let f : no * int -> unit = function _ -> . ]}

    However it doesn't work for sum types: the [no] type need to appear directly in the pattern:
    {[
    type complex_empty = A of no | B of no
    let f : complex_empty = function _ -> . (* does not compile *)
    let f : complex_empty = function A _ | B _ -> . (* compiles *)
    ]}

    In case this behavior is needed, there {!Ast.destr} in {!Ast}. See this
    section to see how they are used. *)
type no = |

(** {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html#TYPEposition}
    [Lexing.position]} *)
type loc = Lexing.position

(** The type of an expression range. This imported from Isla to avoid having two
    incompatible types.

    This represent a range in a source file, so generally this is a pair of {!loc},
    but it may also be [Unknown].*)
type lrng = Isla_lang.AST.lrng

module Size = struct
  (** The possible sizes for memory accesses. It may be necessary to add [B128] at some point *)
  type t = B8 | B16 | B32 | B64

  (** Create a size value from a valid size in byte *)
  let of_bytes = function
    | 1 -> B8
    | 2 -> B16
    | 4 -> B32
    | 8 -> B64
    | bytes -> Raise.inv_arg "%d bytes is not a valid memory size" bytes

  (** Create a size value from a valid size in bits *)
  let of_bits = function
    | 8 -> B8
    | 16 -> B16
    | 32 -> B32
    | 64 -> B64
    | bits -> Raise.inv_arg "%d bits is not a valid memory size" bits

  (** Get the byte size corresponding to that value *)
  let to_bytes = function B8 -> 1 | B16 -> 2 | B32 -> 4 | B64 -> 8

  (** Get the bits size corresponding to that value *)
  let to_bits size = 8 * to_bytes size

  let equal = ( = )

  (** Pretty-print a size as just the byte number *)
  let pp_bytes s = s |> to_bytes |> Pp.int

  (** Pretty print a size at "16bits" for example *)
  let pp_bits s = Pp.(dprintf "%dbits" (to_bits s))
end

(** Raw expression coming out of the parser *)
type rexp = (lrng, string, string, Size.t) exp

(** Raw type coming out of the parser *)
type rty = Size.t ty

(** Raw SMT command coming out of the parser *)
type rsmt = (lrng, string, string, Size.t) smt

(** Raw SMT answer coming out of the parser *)
type rsmt_ans = (lrng, string, string, Size.t) smt_ans
