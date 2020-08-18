(** This module provide a type to represent concrete values.

    There is not concrete value for memory yet, but maybe there should be at some point to
    be able to fully support concrete evaluation without external tool*)

(** The type of concrete values *)
type t = Bool of bool | Enum of Ast.enum | Bv of BitVec.t

(** String representation of concrete values *)
let to_string = function
  | Bool true -> "true"
  | Bool false -> "false"
  | Enum (a, n) -> Printf.sprintf "enum%d/%d" a n
  | Bv bv -> BitVec.to_smt bv

(** Pretty printer for concrete values *)
let pp r = r |> to_string |> PP.string

(** {!Bool} constructor *)
let bool b = Bool b

(** {!Enum} constructor *)
let enum enum = Enum enum

(** {!Bv} constructor *)
let bv bv = Bv bv

(** Extract a boolean or fail *)
let expect_bool = function
  | Bool bool -> bool
  | r -> Raise.fail "Expected boolean but got %s" (to_string r)

(** Extract an enumeration or fail *)
let expect_enum = function
  | Enum enum -> enum
  | r -> Raise.fail "Expected enumeration but got %s" (to_string r)

(** Extract a bit vector or fail. If [size] is specified, then it fail if the size don't match *)
let expect_bv ?size r =
  match (size, r) with
  | (Some size, Bv bv) when BitVec.size bv = size -> bv
  | (None, Bv bv) -> bv
  | (Some size, _) -> Raise.fail "Expected bit vector of size %d but got %s" size (to_string r)
  | (None, _) -> Raise.fail "Expected bit vector but got %s" (to_string r)

(** Convert to a constant expression *)
let to_exp : t -> ('v, 'm) ExpTyped.t = function
  | Bool b -> ExpTyped.bool b
  | Enum enum -> ExpTyped.enum enum
  | Bv bv -> ExpTyped.bits bv
