open Isla

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type type_context = ty hvector

type lty = lrng * ty

(** Exception that represent an Isla typing error *)
exception TypeError of lrng * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | TypeError (l, s) ->
        Some PP.(sprint @@ hardline ^^ prefix 2 1 (lrng l ^^ !^": ") (!^"TypeError: " ^^ !^s))
    | _ -> None)

(** Assert some properties for type correctness. Requires a lrng and a string error message *)
let tassert l s b = if not b then raise (TypeError (l, s))

let expect_bv s (l, t) =
  match t with Ty_BitVec n -> n | _ -> raise (TypeError (l, "A bitvector was expected :" ^ s))

let expect_bool s (l, t) =
  match t with Ty_Bool -> () | _ -> raise (TypeError (l, "A boolean was expected :" ^ s))

(* TODO improve types errors *)
let type_unop l (u : unop) (lt, t) : ty =
  match (u, t) with
  | (Not, Ty_Bool) -> Ty_Bool
  | (Bvnot, Ty_BitVec _) | (Bvneg, Ty_BitVec _) -> t
  | (Bvredand, Ty_BitVec _) | (Bvredor, Ty_BitVec _) -> Ty_Bool
  | (Extract (b, a), Ty_BitVec n) when a < b && b <= n -> Ty_BitVec (b + 1 - a)
  | (ZeroExtend m, Ty_BitVec n) -> Ty_BitVec (n + m)
  | (SignExtend m, Ty_BitVec n) -> Ty_BitVec (n + m)
  | _ -> raise (TypeError (l, "Unary operator"))

(* TODO improve types errors *)
let type_binop l (u : binop) (lt, t) (lt', t') : ty =
  match u with
  (* Equality *)
  | Eq | Neq -> if t = t' then Ty_Bool else raise (TypeError (l, "Equality requires same type"))
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

let type_manyop l (m : manyop) ltl : ty =
  tassert l "Manyops must have at least 1 element" (ltl != []);
  match m with
  | And | Or ->
      List.iter (expect_bool PP.(sprintc $ !^"inside " ^^ pp_manyop m)) ltl;
      Ty_Bool
  | Bvmanyarith _ ->
      let sizes = List.map (expect_bv PP.(sprintc $ !^"inside " ^^ pp_manyop m)) ltl in
      let size = List.hd sizes in
      tassert l
        PP.(
          sprintc
          $ !^"All bitvectors in " ^^ pp_manyop m
            ^^ !^" must have same size but I got:"
            ^^ space ^^ list int sizes)
        (List.for_all (( = ) size) sizes);
      Ty_BitVec size
  | Concat ->
      let sizes = List.map (expect_bv PP.(sprintc $ !^"inside " ^^ pp_manyop m)) ltl in
      Ty_BitVec (List.fold_left ( + ) 0 sizes)

let rec type_valu loc (cont : type_context) : valu -> Reg.typ = function
  | Val_Symbolic var -> (
      try Plain (HashVector.get cont var)
      with Invalid_argument _ ->
        raise (TypeError (loc, Printf.sprintf "Variable v%d is used but never defined" var))
    )
  | Val_Bool _ -> Plain Ty_Bool
  | Val_I (_, size) -> Plain (Ty_BitVec size)
  | Val_Bits str ->
      Plain
        (Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2))
  | Val_Struct l ->
      let rs = Reg.make_struct () in
      List.iter (fun (name, v) -> ignore @@ Reg.add_field rs name (type_valu loc cont v)) l;
      Struct rs
  | Val_List _ -> Warn.fatal0 "valu list not implemented"
  | Val_Vector _ -> Warn.fatal0 "valu list not implemented"
  | Val_Unit -> Warn.fatal0 "What is valu unit ?"
  | Val_Poison -> Warn.fatal0 "What is valu Poison"
  | Val_String -> Warn.fatal0 "valu string not implemented"

let rec ltype_expr (cont : type_context) : 'v lexp -> lty = function
  | Var (Free var, l) -> (l, HashVector.get cont var)
  | Var (_, _) -> Warn.fatal0 "Non free variable typing unimplemented"
  | Bits (str, l) ->
      (l, Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2))
  | Bool (_, l) -> (l, Ty_Bool)
  | Unop (u, e, l) -> (l, type_unop l u @@ ltype_expr cont e)
  | Binop (b, e, e', l) -> (l, type_binop l b (ltype_expr cont e) (ltype_expr cont e'))
  | Manyop (m, el, l) -> (l, type_manyop l m (List.map (ltype_expr cont) el))
  | Ite (c, i, e, l) ->
      let ti = type_expr cont i and te = type_expr cont e in
      expect_bool "ite condition" @@ ltype_expr cont c;
      tassert l "If and else branches must have same type" (ti = te);
      (l, ti)

and type_expr cont expr : ty = snd (ltype_expr cont expr)

(** Add the new register found in the trace and returns the type context for free variables *)
let type_trc ?(tc = HashVector.empty ()) (isla_trace : 'v ltrc) =
  let (Trace events) = isla_trace in
  let process : 'v levent -> unit = function
    | Smt (DeclareConst (Free var, typ), _) -> HashVector.add tc var typ
    | Smt (DefineConst (Free var, exp), _) -> HashVector.add tc var @@ type_expr tc exp
    | Smt (Assert exp, l) -> tassert l "Assertion type must be Bool" (type_expr tc exp = Ty_Bool)
    | ReadReg (name, _, v, l) | WriteReg (name, _, v, l) ->
        let tv = type_valu l tc v in
        if Reg.mem_string name then
          tassert l "Register structure cannot change"
          @@ Reg.type_weak_eq tv (Reg.reg_type (Reg.of_string name))
        else ignore @@ Reg.add_reg name tv
    | _ -> ()
  in
  List.iter process events;
  tc

let pp_tcontext = HashVector.pp PP.pp_ty
