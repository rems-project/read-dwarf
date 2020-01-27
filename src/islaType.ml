module OPP = PP
open Isla_lang

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type traces = term

type trace = trc

type type_context = ty hvector

(** TODO add more diagnostic into that *)
exception TypeError

let tassert b = if not b then raise TypeError

let type_unop (u : unop) t =
  match (u, t) with
  | Not, Ty_Bool -> Ty_Bool
  | Bvnot, Ty_BitVec _ | Bvneg, Ty_BitVec _ -> t
  | Bvredand, Ty_BitVec _ | Bvredor, Ty_BitVec _ -> Ty_Bool
  | Extract (b, a), Ty_BitVec n when a < b && b <= n -> Ty_BitVec (b + 1 - a)
  | ZeroExtend m, Ty_BitVec n -> Ty_BitVec (n + m)
  | SignExtend m, Ty_BitVec n -> Ty_BitVec (n + m)
  | _ -> raise TypeError

let type_binop (u : binop) t t' =
  match u with
  (* Equality *)
  | Neq when t = t' -> Ty_Bool
  (* Logic *)
  | And | Or -> if (t, t') = (Ty_Bool, Ty_Bool) then Ty_Bool else raise TypeError
  (* Arithmetic *)
  | Bvand | Bvor | Bvxor | Bvnand | Bvnor | Bvxnor | Bvadd | Bvsub | Bvmul | Bvudiv | Bvsdiv
   |Bvurem | Bvsrem | Bvsmod ->
      if t = t' && t != Ty_Bool then t else raise TypeError
  (* Comparaisons *)
  | Bvult | Bvslt | Bvule | Bvsle | Bvuge | Bvsge | Bvugt | Bvsgt ->
      if t = t' && t != Ty_Bool then Ty_Bool else raise TypeError
  (* Shifts *)
  | Bvshl | Bvlshr | Bvashr -> (
      match (t, t') with Ty_BitVec n, Ty_BitVec m -> t | _ -> raise TypeError
    )
  (* Concatenation *)
  | Concat -> (
      match (t, t') with Ty_BitVec n, Ty_BitVec m -> Ty_BitVec (n + m) | _ -> raise TypeError
    )
  | _ -> raise TypeError

let isla2reg_type : Isla_lang.ty -> Reg.typ = function
  | Ty_Bool -> Plain 1
  | Ty_BitVec n -> Plain n

let rec type_valu (cont : type_context) : valu -> Reg.typ = function
  | Val_Symbolic var -> isla2reg_type @@ HashVector.get cont var
  | Val_Bool _ -> Plain 1
  | Val_I (_, size) -> Plain size
  | Val_Bits str ->
      Plain (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2)
  | Val_Struct l ->
      let rs = Reg.make_struct () in
      List.iter
        (fun (Struct_elem (name, v)) -> ignore @@ Reg.add_field rs name (type_valu cont v))
        l;
      Struct rs
  | Val_List _ -> Warn.fatal0 "valu list not implemented"
  | Val_Vector _ -> Warn.fatal0 "valu list not implemented"
  | Val_Unit -> Warn.fatal0 "What is valu unit ?"
  | Val_Poison -> Warn.fatal0 "What is valu Poison"
  | Val_String -> Warn.fatal0 "valu string not implemented"

let rec type_expr (cont : type_context) : exp -> ty = function
  | Var (var, _) -> HashVector.get cont var
  | Bits (str, _) ->
      Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2)
  | Bool (_, _) -> Ty_Bool
  | Unop (u, e, _) -> type_unop u @@ type_expr cont e
  | Binop (b, e, e', _) -> type_binop b (type_expr cont e) (type_expr cont e')
  (* TODO move Eq into Binop *)
  | Eq (e, e', _) when type_expr cont e = type_expr cont e' -> Ty_Bool
  | Ite (c, i, e, _) ->
      let ti = type_expr cont i and te = type_expr cont e in
      tassert (type_expr cont c = Ty_Bool);
      tassert (ti = te);
      ti
  | _ -> raise TypeError

(** Add the new register found in the trace and dump old ones. *)
let type_regs (isla_trace : trace) =
  let c : type_context = HashVector.empty () in
  let (Trace (events, _)) = isla_trace in
  let process : Isla_lang.event -> unit = function
    | Smt (DeclareConst (var, typ, _), _) -> HashVector.add c var typ
    | Smt (DefineConst (var, exp, _), _) -> HashVector.add c var @@ type_expr c exp
    | Smt (Assert (exp, _), _) -> if type_expr c exp != Ty_Bool then raise TypeError
    | ReadReg (name, _, v, _) | WriteReg (name, _, v, _) ->
        let tv = type_valu c v in
        if Reg.mem_string name then
          tassert @@ Reg.type_weak_eq tv @@ Reg.reg_type (Reg.from_string name)
        else ignore @@ Reg.add_reg name tv
    | _ -> ()
  in
  List.iter process events;
  c

module PP = struct
  let tcontext = HashVector.PP.hvector Isla_lang.PP.pp_ty
end
