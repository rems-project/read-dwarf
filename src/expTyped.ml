(** This module provide operation on typed expressions i.e expressions whose annotations are their
    SMT type ({!Ast.ty}). *)

open Ast

(** The type for a typed expression.

    - The ['v] type is the type of expression variable.
    - The ['m] type should be either {!Ast.no} if memory operation are  disabled
      or {!Ast.Size.t} if they are enabled.

    The let bindings are always disabled.*)
type ('v, 'm) t = ('m ty, 'v, no, 'm) exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Generic operation on types } *)

(** Get the type of a typed expression.
    Specialized version of {!AstManip.annot_exp} *)
let get_type : ('v, 'm) t -> 'm ty = AstManip.annot_exp

let is_bool t = t = Ty_Bool

let is_bv = function Ty_BitVec _ -> true | _ -> false

let is_enum = function Ty_Enum _ -> true | _ -> false

let expect_bool = function Ty_Bool -> () | _ -> assert false

let expect_bv = function Ty_BitVec n -> n | _ -> assert false

let expect_enum = function Ty_Enum n -> n | _ -> assert false

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Generic constructors }

    This constuctors build a new expression as required, and compute the new type.
    They assume the operation is well typed (They assert it).*)

let var ~typ (v : 'v) : ('v, 'm) t = Var (v, typ)

let bits bv : ('v, 'm) t = Bits (bv, Ty_BitVec (BitVec.size bv))

let bool b : ('v, 'm) t = Bool (b, Ty_Bool)

let enum e : ('v, 'm) t = Enum (e, Ty_Enum (fst e))

let unop op (e : ('v, 'm) t) : ('v, 'm) t =
  match op with
  | Not ->
      assert (get_type e = Ty_Bool);
      Unop (op, e, Ty_Bool)
  | Bvnot | Bvneg ->
      let ty = get_type e in
      assert (is_bv ty);
      Unop (op, e, ty)
  | Bvredand | Bvredor ->
      assert (e |> get_type |> is_bv);
      Unop (op, e, Ty_Bool)
  | Extract (last, first) ->
      assert (last >= first);
      assert (last < expect_bv (get_type e));
      Unop (op, e, Ty_BitVec (last - first + 1))
  | ZeroExtend m | SignExtend m ->
      let size = get_type e |> expect_bv in
      Unop (op, e, Ty_BitVec (size + m))

let binop op (e : ('v, 'm) t) (e' : ('v, 'm) t) : ('v, 'm) t =
  let bv_same () =
    let size = expect_bv (get_type e) in
    assert (size = expect_bv (get_type e'));
    size
  in
  match op with
  | Eq ->
      assert (get_type e = get_type e');
      Binop (op, e, e', Ty_Bool)
  | Bvarith _ -> Binop (op, e, e', Ty_BitVec (bv_same ()))
  | Bvcomp _ ->
      bv_same () |> ignore;
      Binop (op, e, e', Ty_Bool)
  | Binmem _ -> Raise.todo ()

(** In addition to well-typedness requirement, this function will
    throw [Invalid_argument] if the list is empty. If the list has a single element,
    it will just return that element instead of building the symbolic operation. *)
let manyop op (el : ('v, 'm) t list) : ('v, 'm) t =
  let check_type t = List.for_all (fun e -> t = get_type e) el in
  match el with
  | [] -> Raise.inv_arg "Can't build a manyop (%t) with an empty list" (PP.tos Ast.pp_manyop op)
  | [e] -> e
  | h :: _ -> (
      match op with
      | And | Or ->
          assert (check_type Ty_Bool);
          Manyop (op, el, Ty_Bool)
      | Bvmanyarith _ ->
          let typ = get_type h in
          assert (check_type typ);
          Manyop (op, el, typ)
      | Concat ->
          let size = List.fold_left (fun s e -> s + expect_bv (get_type e)) 0 el in
          Manyop (op, el, Ty_BitVec size)
    )

let ite ~cond e e' : ('v, 'm) t =
  assert (get_type cond |> is_bool);
  let typ = get_type e in
  assert (typ = get_type e');
  Ite (cond, e, e', typ)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Specific constructors } *)

let bits_int ~size i = bits (BitVec.of_int ~size i)

let bits_smt s = bits (BitVec.of_smt s)

let zero ~size = bits (BitVec.zero ~size)

let true_ = bool true

let false_ = bool false

let ( + ) a b = manyop (Bvmanyarith Bvadd) [a; b]

let sum el = manyop (Bvmanyarith Bvadd) el

let sub a b = binop (Bvarith Bvsub) a b

let ( - ) = sub

let ( * ) a b = manyop (Bvmanyarith Bvmul) [a; b]

let prod el = manyop (Bvmanyarith Bvmul) el

let sdiv a b = binop (Bvarith Bvsdiv) a b

let not a = unop Not a

let neg a = unop Bvneg a

let extract ~first ~last e = unop (Extract (last, first)) e

let eq a b = binop Eq a b

let ( = ) = eq

let concat el = manyop Concat el

let comp comp a b = binop (Bvcomp comp) a b

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Add types } *)

(** Replace the annotation of expression by the SMT types.
    The expression must be already well typed (you can trust the SMT solver on this one)

    It will still [assert] that an expression is well typed as a side effect.
*)
let rec add_type ~(ty_of_var : 'a -> 'v -> 'm ty) (exp : ('a, 'v, no, 'm) exp) : ('v, 'm) t =
  let at = add_type ~ty_of_var in
  match exp with
  | Var (v, a) -> var ~typ:(ty_of_var a v) v
  | Bound _ -> .
  | Bits (bv, _) -> bits bv
  | Bool (b, _) -> bool b
  | Enum (e, _) -> enum e
  | Unop (op, e, _) -> unop op (at e)
  | Binop (op, e, e', _) -> binop op (at e) (at e')
  | Manyop (op, el, _) -> manyop op (List.map at el)
  | Ite (cond, e, e', _) -> ite ~cond:(at cond) (at e) (at e')
  | Let _ -> .

(** Check if an expression is well typed *)
let is_well_typed (exp : ('v, 'm) t) : bool =
  Ast.equal_exp ~annot:Stdlib.( = )
    ~var:(fun _ _ -> true)
    exp
    (add_type ~ty_of_var:(fun a _ -> a) exp)
