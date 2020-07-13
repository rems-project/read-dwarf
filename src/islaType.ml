open Isla

open Logs.Logger (struct
  let str = __MODULE__
end)

type 'a vector = 'a Vec.t

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

let expect_bool s (l, t) =
  match t with Ty_Bool -> () | _ -> raise (TypeError (l, "A boolean was expected :" ^ s))

let expect_bv s (l, t) =
  match t with Ty_BitVec n -> n | _ -> raise (TypeError (l, "A bitvector was expected :" ^ s))

let expect_enum s (l, t) =
  match t with Ty_Enum n -> n | _ -> raise (TypeError (l, "A enumeration was expected :" ^ s))

let type_unop l (u : unop) (ltt : lrng * ty) : ty =
  match u with
  | Not ->
      expect_bool "Not" ltt;
      Ty_Bool
  | Bvnot | Bvneg -> Ty_BitVec (expect_bv PP.(sprintc @@ pp_unop u) ltt)
  | Bvredand | Bvredor ->
      ignore (expect_bv PP.(sprintc @@ pp_unop u) ltt);
      Ty_BitVec 1
  | Extract (b, a) ->
      let n = expect_bv PP.(sprintc @@ pp_unop u) ltt in
      tassert l PP.(sprintc @@ pp_unop u ^^ !^" make no sense") (a <= b);
      tassert l
        PP.(sprintc @@ !^"Trying " ^^ pp_unop u ^^ !^" but bit vector is of size " ^^ int n)
        (b <= n);
      Ty_BitVec (b + 1 - a)
  | ZeroExtend m | SignExtend m -> Ty_BitVec (expect_bv PP.(sprintc @@ pp_unop u) ltt + m)

let type_binop l (b : binop) ((_, t) as ltt) ((_, t') as ltt') : ty =
  let bv_same () =
    let n = expect_bv PP.(sprintc @@ pp_binop b) ltt in
    let m = expect_bv PP.(sprintc @@ pp_binop b) ltt' in
    tassert l
      PP.(
        sprintc @@ !^"Operands of " ^^ pp_binop b
        ^^ dprintf " must have same size but got %d and %d" n m)
      (n = m);
    n
  in
  match b with
  | Eq -> if t = t' then Ty_Bool else raise (TypeError (l, "Equality requires same type"))
  | Bvarith _ -> Ty_BitVec (bv_same ())
  | Bvcomp _ ->
      ignore @@ bv_same ();
      Ty_Bool

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

let rec type_valu loc (cont : type_context) : valu -> (Reg.Path.t * Reg.ty) list =
  let plain ty = [([], IslaConv.ty ty)] in
  function
  | Val_Symbolic var -> (
      try plain @@ HashVector.get cont var
      with Invalid_argument _ ->
        raise (TypeError (loc, Printf.sprintf "Variable v%d is used but never defined" var))
    )
  | Val_Bool _ -> plain Ty_Bool
  | Val_I (_, size) -> plain (Ty_BitVec size)
  | Val_Bits str ->
      plain
        (Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2))
  | Val_Struct l ->
      let open List in
      let* (name, value) = l in
      let+ (path, ty) = type_valu loc cont value in
      (name :: path, ty)
  | Val_Enum (n, _) -> plain (Ty_Enum n)
  | Val_List _ -> fatal "valu list not implemented"
  | Val_Vector _ -> fatal "valu list not implemented"
  | Val_Unit -> fatal "valu unit not implemented"
  | Val_NamedUnit _ -> fatal "valu named unit not implemented"
  | Val_Poison -> fatal "Hey I got poisoned! Bad sail !"
  | Val_String _ -> fatal "valu string not implemented"

let rec ltype_expr (cont : type_context) : rexp -> lty = function
  | Var (var, l) -> (l, HashVector.get cont var)
  | Bits (str, l) ->
      (l, Ty_BitVec (if str.[1] = 'x' then 4 * (String.length str - 2) else String.length str - 2))
  | Bool (_, l) -> (l, Ty_Bool)
  | Enum ((n, _), l) -> (l, Ty_Enum n)
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
let type_trc ?(tc = HashVector.empty ()) (isla_trace : rtrc) =
  let (Trace events) = isla_trace in
  let process : revent -> unit = function
    | Smt (DeclareConst (var, typ), _) -> HashVector.add tc var typ
    | Smt (DefineConst (var, exp), _) -> HashVector.add tc var @@ type_expr tc exp
    | Smt (Assert exp, l) -> tassert l "Assertion type must be Bool" (type_expr tc exp = Ty_Bool)
    | ReadReg (name, _, v, l) | WriteReg (name, _, v, l) ->
        let open List in
        let+! (path, ty) = type_valu l tc v in
        Reg.ensure_adds (name :: path) ty
    | _ -> ()
  in
  List.iter process events;
  tc

let pp_tcontext = HashVector.pp Isla.pp_ty
