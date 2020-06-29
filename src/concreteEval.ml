(** This module provides a way of making concrete evaluation of an expression.
    The only required thing is a {!context}.
*)

(** A map from variables to concrete values *)
type 'v context = 'v -> Value.t

(** Thrown when trying to concretely evaluate a symbolic expression *)
exception Symbolic

(** Evaluate a concrete expression using Z3: TODO

    This is mostly for testing purposes.
*)
let eval_z3 ?(ctxt = fun _ -> failwith "novar") (e : ('a, 'v, Ast.no, Ast.no) Ast.exp) : Value.t =
  failwith "unimplemented"

let eval_unop (u : Ast.unop) v =
  match u with
  | Not -> v |> Value.expect_bool |> not |> Value.bool
  | Bvnot -> v |> Value.expect_bv |> BitVec.lognot |> Value.bv
  | Bvneg -> v |> Value.expect_bv |> BitVec.neg |> Value.bv
  | Bvredand -> v |> Value.expect_bv |> BitVec.redand |> Value.bool
  | Bvredor -> v |> Value.expect_bv |> BitVec.redor |> Value.bool
  | Extract (b, a) -> v |> Value.expect_bv |> BitVec.extract a b |> Value.bv
  | ZeroExtend m -> v |> Value.expect_bv |> BitVec.zero_extend m |> Value.bv
  | SignExtend m -> v |> Value.expect_bv |> BitVec.sign_extend m |> Value.bv

let eval_bvarith (b : Ast.bvarith) v v' =
  let open BitVec in
  match b with
  | Bvnand -> lnot (v land v')
  | Bvnor -> lnot (v lor v')
  | Bvxnor -> lnot (v lxor v')
  | Bvsub -> v - v'
  | Bvudiv -> udiv v v'
  | Bvudivi -> udiv v v'
  | Bvsdiv -> sdiv v v'
  | Bvsdivi -> sdiv v v'
  | Bvurem -> Raise.fail "unimplemented"
  | Bvsrem -> Raise.fail "unimplemented"
  | Bvsmod -> Raise.fail "unimplemented"
  | Bvshl -> v lsl v'
  | Bvlshr -> v lsr v'
  | Bvashr -> v asr v'

let eval_bvcomp (b : Ast.bvcomp) v v' =
  let open BitVec in
  match b with
  | Bvult -> to_uz v < to_uz v'
  | Bvslt -> to_z v < to_z v'
  | Bvule -> to_uz v <= to_uz v'
  | Bvsle -> to_z v <= to_z v'
  | Bvuge -> to_uz v >= to_uz v'
  | Bvsge -> to_z v >= to_z v'
  | Bvugt -> to_uz v > to_uz v'
  | Bvsgt -> to_z v > to_z v'

let eval_binop (b : Ast.no Ast.binop) v v' =
  match b with
  | Eq -> v = v' |> Value.bool
  | Neq -> v <> v' |> Value.bool
  | Bvarith bva -> eval_bvarith bva (Value.expect_bv v) (Value.expect_bv v') |> Value.bv
  | Bvcomp bvc -> eval_bvcomp bvc (Value.expect_bv v) (Value.expect_bv v') |> Value.bool
  | Binmem m -> Ast.destr_binmem m

let eval_bvmanyarith (m : Ast.bvmanyarith) bvs =
  let open BitVec in
  match m with
  | Bvand -> List.fold_left_same ( land ) bvs
  | Bvor -> List.fold_left_same ( lor ) bvs
  | Bvxor -> List.fold_left_same ( lxor ) bvs
  | Bvadd -> List.fold_left_same ( + ) bvs
  | Bvmul -> List.fold_left_same ( * ) bvs

let eval_manyop (m : Ast.manyop) vs =
  match m with
  | And -> List.for_all Value.expect_bool vs |> Value.bool
  | Or -> List.exists Value.expect_bool vs |> Value.bool
  | Concat ->
      List.fold_left (fun bv v -> v |> Value.expect_bv |> BitVec.concat bv) BitVec.empty vs
      |> Value.bv
  | Bvmanyarith bvma -> eval_bvmanyarith bvma (List.map Value.expect_bv vs) |> Value.bv

(** Evaluate a concrete expression directly without external tool *)
let rec eval_direct ?(ctxt = fun _ -> raise Symbolic) (e : ('a, 'v, Ast.no, Ast.no) Ast.exp) :
    Value.t =
  match e with
  | Var (v, _) -> ctxt v
  | Bound _ -> .
  | Bits (bv, _) -> bv |> Value.bv
  | Bool (b, _) -> b |> Value.bool
  | Enum (enum, _) -> enum |> Value.enum
  | Unop (u, v, _) -> v |> eval_direct ~ctxt |> eval_unop u
  | Binop (b, v, v', _) -> eval_binop b (eval_direct ~ctxt v) (eval_direct ~ctxt v')
  | Manyop (m, vs, _) -> vs |> List.map (eval_direct ~ctxt) |> eval_manyop m
  | Ite (c, e, e', _) ->
      let cv = eval_direct ~ctxt c in
      if cv |> Value.expect_bool then eval_direct ~ctxt e else eval_direct ~ctxt e'
  | Let _ -> .

(** Evaluate a concrete expression *)
let eval = eval_direct

(* let _ =
 *   Tests.add_test "ConcreteEval" (fun () ->
 *       let two = BitVec.of_int ~size:7 2 in
 *       let five = BitVec.of_int ~size:7 5 in
 *       let ten = BitVec.of_int ~size:7 10 in
 *       let exp = Ast.Op.(sdiv (bits two + bits ten) (bits five) = bits two) in
 *       eval_direct exp |> Value.expect_bool) *)

let rec is_concrete (exp : _ Ast.exp) : bool =
  match exp with
  | Bound _ -> false
  | Var _ -> false
  | exp -> AstManip.direct_exp_for_all_exp is_concrete exp

(** Evaluate an expression if it's concrete and returns [None] otherwise *)
let eval_if_concrete (exp : _ Ast.exp) : Value.t option =
  try eval_direct exp |> Opt.some with Symbolic -> None

(** Split the sum between a concrete bitvector term, and symbolic term *)
let sum_split_concrete ~size exp =
  let terms = AstManip.sum_split exp in
  let (symterms, concvals) = List.partition_map eval_if_concrete terms in
  let concbvs = List.map Value.expect_bv concvals in
  let concbv = List.fold_left BitVec.( + ) (BitVec.zero ~size) concbvs in
  let symterm = match symterms with [] -> None | l -> Some (Ast.Op.sum symterms) in
  (symterm, concbv)

(** Tells if an expression has a concrete term *)
let has_concrete_term exp =
  let terms = AstManip.sum_split exp in
  List.exists is_concrete terms
