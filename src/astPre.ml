(** Generic empty type *)
type no = |

module Size = struct
  type t = B8 | B16 | B32 | B64

  let of_bytes = function
    | 1 -> B8
    | 2 -> B16
    | 4 -> B32
    | 8 -> B64
    | bytes -> Raise.fail "%d bytes is not a valid memory size" bytes
end

(* (\*****************************************************************************\)
 * (\*****************************************************************************\)
 * (\*****************************************************************************\)
 * (\** {1 Examples } *\)
 *
 * let annot_exp : ('a, 'v, 'b, 'e, 'p, 'm) exp -> 'a = function
 *   | Var (_, a) -> a
 *   | Bound (_, a) -> a
 *   | Bits (_, a) -> a
 *   | Bool (_, a) -> a
 *   | Enum (_, a) -> a
 *   | Unop (_, _, a) -> a
 *   | Binop (_, _, _, a) -> a
 *   | Manyop (_, _, a) -> a
 *   | Ite (_, _, _, a) -> a
 *   | Let (_, _, _, a) -> a
 *
 * let rec unfold_let : ('a, 'v, 'b, 'e, 'p, 'm) exp -> ('a, 'v, no, 'e, 'p, 'm) exp = function
 *   | Bound _ -> failwith "finding into table"
 *   | Let _ -> failwith "Recording the var in the table"
 *   | Var _ as var -> var
 *   | Bits _ as b -> b
 *   | Bool _ as b -> b
 *   | Enum _ as e -> e
 *   | Unop (u, e, a) as un -> Unop (u, unfold_let e, a)
 *   | Binop (b, e, e', a) -> Binop (b, unfold_let e, unfold_let e', a)
 *   | Manyop (m, el, a) -> Manyop (m, List.map unfold_let el, a)
 *   | Ite (c, e, e', a) -> Ite (unfold_let c, unfold_let e, unfold_let e', a)
 *
 * let allow_let : ('a, 'v, no, 'e, 'p, 'm) exp -> ('a, 'v, int, 'e, 'p, 'm) exp = Obj.magic *)
