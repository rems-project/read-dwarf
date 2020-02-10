open Isla

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type type_context = ty hvector

(** Exception that represent an Isla typing error *)
exception TypeError of lrng * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | TypeError (l, s) ->
        Some PP.(sprint @@ prefix 2 1 (lrng l ^^ !^": ") (!^"TypeError: " ^^ !^s))
    | _ -> None)

(** Assert some properties for type correctness. Requires a lrng and a string error message *)
let tassert l s b = if not b then raise (TypeError (l, s))

let type_unop l (u : unop) t =
  match (u, t) with
  | (Not, Ty_Bool) -> Ty_Bool
  | (Bvnot, Ty_BitVec _) | (Bvneg, Ty_BitVec _) -> t
  | (Bvredand, Ty_BitVec _) | (Bvredor, Ty_BitVec _) -> Ty_Bool
  | (Extract (b, a), Ty_BitVec n) when a < b && b <= n -> Ty_BitVec (b + 1 - a)
  | (ZeroExtend m, Ty_BitVec n) -> Ty_BitVec (n + m)
  | (SignExtend m, Ty_BitVec n) -> Ty_BitVec (n + m)
  | _ -> raise (TypeError (l, "Unary operator"))

let type_binop l (u : binop) t t' =
  match u with
  (* Equality *)
  | Eq | Neq -> if t = t' then Ty_Bool else raise (TypeError (l, "Equality requires same type"))
  (* Logic *)
  | And | Or ->
      if (t, t') = (Ty_Bool, Ty_Bool) then Ty_Bool
      else raise (TypeError (l, "Boolean logic requires booleans"))
  (* Arithmetic *)
  | Bvarith _ -> if t = t' && t != Ty_Bool then t else raise (TypeError (l, "Bvarith"))
  (* Comparaisons *)
  | Bvcomp _ -> if t = t' && t != Ty_Bool then Ty_Bool else raise (TypeError (l, "Bvcomp"))
  (* Shifts *)
  | Bvshl | Bvlshr | Bvashr -> (
      match (t, t') with
      | (Ty_BitVec n, Ty_BitVec m) -> t
      | _ -> raise (TypeError (l, "Bv shifts"))
    )
  (* Concatenation *)
  | Concat -> (
      match (t, t') with
      | (Ty_BitVec n, Ty_BitVec m) -> Ty_BitVec (n + m)
      | _ -> raise (TypeError (l, "Concat "))
    )

let rec type_valu (cont : type_context) : valu -> Reg.typ = function
  | Val_Symbolic var -> Plain (HashVector.get cont var)
  | Val_Bool _ -> Plain Ty_Bool
  | Val_I (_, size) -> Plain (Ty_BitVec size)
  | Val_Bits str ->
      Plain
        (Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2))
  | Val_Struct l ->
      let rs = Reg.make_struct () in
      List.iter (fun (name, v) -> ignore @@ Reg.add_field rs name (type_valu cont v)) l;
      Struct rs
  | Val_List _ -> Warn.fatal0 "valu list not implemented"
  | Val_Vector _ -> Warn.fatal0 "valu list not implemented"
  | Val_Unit -> Warn.fatal0 "What is valu unit ?"
  | Val_Poison -> Warn.fatal0 "What is valu Poison"
  | Val_String -> Warn.fatal0 "valu string not implemented"

let rec type_expr (cont : type_context) : 'v lexp -> ty = function
  | Var (Free var, _) -> HashVector.get cont var
  | Var (_, _) -> Warn.fatal0 "Non free variable typing unimplemented"
  | Bits (str, _) ->
      Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2)
  | Bool (_, _) -> Ty_Bool
  | Unop (u, e, l) -> type_unop l u @@ type_expr cont e
  | Binop (b, e, e', l) -> type_binop l b (type_expr cont e) (type_expr cont e')
  | Ite (c, i, e, l) ->
      let ti = type_expr cont i and te = type_expr cont e in
      tassert l "If condition must be a Bool" (type_expr cont c = Ty_Bool);
      tassert l "If and else branches must have same type" (ti = te);
      ti

(** Add the new register found in the trace and returns the type context for free variables *)
let type_regs (isla_trace : 'v ltrc) =
  let c : type_context = HashVector.empty () in
  let (Trace (events, _)) = isla_trace in
  let process : 'v levent -> unit = function
    | Smt (DeclareConst (Free var, typ), _) -> HashVector.add c var typ
    | Smt (DefineConst (Free var, exp), _) -> HashVector.add c var @@ type_expr c exp
    | Smt (Assert exp, l) -> tassert l "Assertion type must be Bool" (type_expr c exp = Ty_Bool)
    | ReadReg (name, _, v, l) | WriteReg (name, _, v, l) ->
        let tv = type_valu c v in
        if Reg.mem_string name then
          tassert l "Register structure cannot change"
          @@ Reg.type_weak_eq tv (Reg.reg_type (Reg.of_string name))
        else ignore @@ Reg.add_reg name tv
    | _ -> ()
  in
  List.iter process events;
  c

let pp_tcontext = HashVector.pp PP.pp_ty
