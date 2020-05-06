(** This module provide most of the C type inference utility *)

open Logs.Logger (struct
    let str = __MODULE__
  end)

open TraceContext

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Expressions}

    This section implements all the C type expression inference rules.

    C Types are always option and if any part of an expression is not typed,
    then the result is not typed.
*)

(** A typed {!Trace.exp} *)
type tval = { ctyp : Ctype.t option; exp : Trace.exp }

(** Output a Machine type of the same size at the original type.
    If [update] has a value, the size if modified by the update value. *)
let machine_of_size ?(update = 0) (typ : Ctype.t) : Ctype.t =
  let size = Ctype.sizeof typ in
  let constexpr = typ.constexpr in
  Ctype.machine ~constexpr (size + update)

let unop (u : Ast.unop) tval : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp in
  match u with
  | Not | Bvredand | Bvredor -> None
  | Bvneg | Bvnot -> machine_of_size typ |> some
  | Extract (b, a) ->
      if a = 0 && b = Arch.address_size - 1 && Ctype.is_ptr typ then tval.ctyp
      else
        let bitsize = b - a + 1 in
        let constexpr = typ.constexpr in
        if bitsize mod 8 = 0 then Ctype.machine ~constexpr (bitsize / 8) |> some else None
  | ZeroExtend m | SignExtend m ->
      if m mod 8 = 0 then machine_of_size ~update:(m / 8) typ |> some else None

let constexpr_to_int ~ctxt e =
  let vctxt v = expand_var ~ctxt v Ast.unknown |> ConcreteEval.eval_direct in
  e |> ConcreteEval.eval_direct ~ctxt:vctxt |> Value.expect_bv |> BitVec.to_int

let binop ~ctxt (b : Ast.no Ast.binop) (tval : tval) (tval' : tval) : Ctype.t option =
  let open Opt in
  let* typ = tval.ctyp and* typ' = tval'.ctyp in
  match b with
  | Eq | Neq | Bvcomp _ -> None
  | Bvarith Bvsub when Ctype.is_ptr typ ->
      if typ'.constexpr then
        let v' = constexpr_to_int ~ctxt tval'.exp in
        Ctype.ptr_update typ (-v') |> some
      else Ctype.ptr_forget typ |> some
  | Bvarith _ ->
      let constexpr = typ.constexpr && typ'.constexpr in
      Ctype.machine ~constexpr (Ctype.sizeof typ) |> some
  | Binmem b -> Ast.destr_binmem b

let manyop ~ctxt (m : Ast.manyop) (tvals : tval list) : Ctype.t option =
  let open Opt in
  match m with
  | Concat | And | Or -> None
  | Bvmanyarith Bvadd -> (
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let (ptrs, rest) = List.partition Ctype.is_ptr typs in
      match ptrs with
      | [] ->
          let constexpr = List.for_all Ctype.is_constexpr typs in
          let size = Ctype.sizeof @@ List.hd typs in
          Ctype.machine ~constexpr size
      | [ptr] ->
          let constexpr = List.for_all Ctype.is_constexpr rest in
          if constexpr then
            let v =
              List.fold_left
                (fun v tval ->
                  let c = Option.get tval.ctyp in
                  if c.constexpr then v + constexpr_to_int ~ctxt tval.exp else v)
                0 tvals
            in
            Ctype.ptr_update ptr v
          else Ctype.ptr_forget ptr
      | _ ->
          warn "Multiple pointer in addition, returnining Machine";
          Ctype.machine Ctype.ptr_size
    )
  | Bvmanyarith _ ->
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let constexpr = List.for_all Ctype.is_constexpr typs in
      let size = Ctype.sizeof @@ List.hd typs in
      Ctype.machine ~constexpr size

(** Stage 1 expression typer *)
let rec expr ~ctxt  (exp : Trace.exp) : Ctype.t option =
  match exp with
  | Var (Register reg, l) -> State.get_reg ctxt.state reg |> State.get_ctyp
  | Var (Read r, l) -> HashVector.get ctxt.mem_reads r |> State.get_ctyp
  | Bits (bv, l) ->
      let size = BitVec.size bv in
      if size mod 8 = 0 || size = Arch.address_size then
        Ctype.machine ~constexpr:true (size / 8) |> Opt.some
      else None
  | Bool (_, l) -> None
  | Enum (_, l) -> None
  | Unop (u, e, l) -> expr_tval ~ctxt e |> unop u
  | Binop (b, e, e', l) ->
      let te = expr_tval ~ctxt e in
      let te' = expr_tval ~ctxt e' in
      binop ~ctxt b te te'
  | Manyop (m, el, l) -> List.map (expr_tval ~ctxt) el |> manyop ~ctxt m
  | Ite (c, e, e', l) -> None
  | Bound _ -> .
  | Let _ -> .

and expr_tval ~ctxt exp = { exp; ctyp = expr ~ctxt exp }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Memory} *)

(** Does the same as {!State.read}, but additionally take care of reading the type from a fragment
    and marking the type of the read variable. *)
let read ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) (mb : State.Mem.block) : State.tval =
  match ptrtype with
  | Some { unqualified = Ptr { fragment; offset }; _ } ->
      let fenv = s.fenv in
      let size = State.Mem.Size.to_bytes mb.size in
      let ctyp = Fragment.ptr_deref ~env:dwarf.tenv ~fenv ~size fragment offset in
      let exp = State.read ?ctyp s mb in
      { exp; ctyp }
  | Some _ ->
      warn "Reading from non-ptr unimplemented for now";
     State.read s mb |> State.make_tval
  | None -> State.read s mb |> State.make_tval

(** Does the same as {!State.write}, but additionally take care of writing the type
    if the write is on a {!Ctype.FreeFragment}.*)
let write ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) (mb : State.Mem.block) (value : State.tval) : unit =
  match (ptrtype, value.ctyp) with
  | (Some { unqualified = Ptr { fragment; offset }; _ }, Some ctyp) ->
      let fenv = s.fenv in
      let size = State.Mem.Size.to_bytes mb.size in
      let ctyp = Fragment.ptr_write ~env:dwarf.tenv ~fenv ~ctyp fragment offset in
      State.write s mb value.exp
  | _ -> State.write s mb value.exp
