include AstOtt

(** Generic empty type *)
type no = |

type loc = Lexing.position

type lrng = Isla.lrng

module Size = struct
  type t = B8 | B16 | B32 | B64

  let of_bytes = function
    | 1 -> B8
    | 2 -> B16
    | 4 -> B32
    | 8 -> B64
    | bytes -> Raise.fail "%d bytes is not a valid memory size" bytes

  let to_bytes = function B8 -> 1 | B16 -> 2 | B32 -> 4 | B64 -> 8

  let to_bits size = 8 * to_bytes size

  let pp_bytes s = s |> to_bytes |> PP.int
end

type rexp = (lrng, string, string, Size.t, Size.t) exp

type rsmt = (lrng, string, string, Size.t, Size.t) smt

type rsmts = rsmt list

type rsmt_ans = (lrng, string, string, Size.t, Size.t) smt_ans
