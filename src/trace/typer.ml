(* ================================================================================== *)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module provide most of the C type inference logic

    An important remark about this logic is that all types are optional. The
    absence of types is like a poison that propagates to the end, meaning that if
    any value in expression does not have a C type, then the whole expression won't
    have one. This behavior may need to be thought of again.

    The second important remark is that we don't really care about
    the typing of non-pointer values. Whether an plain integer
    is an [int] or a [long] is completely irrelevant to us.
    On the other hand, whether a pointer is a [int*] or a [long*] is
    very important. That's why more non-pointer types will
    decay to {!Ctype.Machine} in all rules.

    The type inference is decomposed in two parts:
    - First, we have to type {{!exp} expressions} themselves as if they
      were expressions on C values and not machine values.
    - Second, we have to type effects like {{!read}reading} and {{!write}writing}
      to memory *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Context
open Fun
module Value = Exp.Value
module ConcreteEval = Exp.ConcreteEval
module Typed = Exp.Typed
module Fragment = State.Fragment

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:exp Expressions}

    This section implements all the C type expression inference rules.

    C Types are always optional and if any part of an expression is not typed,
    then the result is not typed.
*)

(** A typed {!Base.exp} *)
type tval = { ctyp : Ctype.t option; exp : Base.exp }

let get_ctypo tval = tval.ctyp

let get_ctyp tval = tval.ctyp |> Option.get

(** Output a Machine type of the same size at the original type.
    If [update] has a value, the size if modified by the update value. *)
let machine_of_size ?(update = 0) (typ : Ctype.t) : Ctype.t =
  let size = Ctype.sizeof typ in
  let constexpr = typ.constexpr in
  Ctype.machine ~constexpr (size + update)

let is_const tval =
  tval.ctyp
  |> Option.map Ctype.is_constexpr
  |> Option.value_fun ~default:(fun () -> Exp.ConcreteEval.is_concrete tval.exp)

let constexpr_of_exp e =
  let ty = Exp.Typed.get_type e in
  if Exp.Typed.is_bv ty then
    let bitsize = Exp.Typed.expect_bv ty in
    Option.some @@
    if bitsize mod 8 = 0 || bitsize = Arch.address_size then
      Ctype.machine ~constexpr:true (bitsize / 8)
    else
      Ctype.Bits |> Ctype.qual ~constexpr:true
  else
    None


let unop (u : Ast.unop) tval : Ctype.t option =
  let open Option in
  let* typ = tval.ctyp in
  match u with
  | Not | Bvredand | Bvredor -> None
  | Bvneg | Bvnot -> machine_of_size typ |> some
  | Extract (b, a) ->
      debug "Extracting from type %t" Pp.(top (opt Ctype.pp) tval.ctyp);
      if (* HACK for adrp: a = 0 && b = Arch.address_size - 1 &&*) Ctype.is_ptr typ then
        tval.ctyp
      else
        let bitsize = b - a + 1 in
        let constexpr = typ.constexpr in
        if bitsize mod 8 = 0 then Ctype.machine ~constexpr (bitsize / 8) |> some else None
  | ZeroExtend m | SignExtend m ->
      if m mod 8 = 0 then machine_of_size ~update:(m / 8) typ |> some else None

let constexpr_to_int ~ctxt e =
  try
    let vctxt v = expand_var ~ctxt v (Base.Var.ty v) |> ConcreteEval.eval in
    e |> ConcreteEval.eval ~ctxt:vctxt |> Value.expect_bv |> BitVec.to_int
  with ConcreteEval.Symbolic ->
    err "Expression %t was typed as constexpr but is not constant" (Pp.top Base.pp_exp e);
    Raise.again ConcreteEval.Symbolic

let binop ~ctxt (b : Ast.no Ast.binop) (tval : tval) (tval' : tval) : Ctype.t option =
  let open Option in
  let* typ = tval.ctyp and* typ' = tval'.ctyp in
  match b with
  | Eq | Bvcomp _ -> None
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
  let open Option in
  match m with
  | And | Or -> None
  | Bvmanyarith Bvadd -> (
      if List.exists (fun tval -> tval.ctyp = None) tvals then None
      else
        let typs = List.map get_ctyp tvals in
        let (ptrs, rest) = List.partition (get_ctyp %> Ctype.is_ptr) tvals in
        Option.some
        @@
        match ptrs with
        | [] ->
            let constexpr = List.for_all Ctype.is_constexpr typs in
            let size = Ctype.sizeof @@ List.hd typs in
            Ctype.machine ~constexpr size
        | [ptr] ->
            let constexpr = List.for_all (get_ctyp %> Ctype.is_constexpr) rest in
            if constexpr then (
              let v = List.fold_left (fun v tval -> v + constexpr_to_int ~ctxt tval.exp) 0 rest in
              debug "Update by %t" Pp.(top shex v);
              let t = Ctype.ptr_update (get_ctyp ptr) v in
              debug "to get %t" Pp.(top Ctype.pp t);
              t
            )
            else Ctype.ptr_forget (get_ctyp ptr)
        | _ ->
            warn "Multiple pointers in an addition, returning Machine";
            Ctype.machine Ctype.ptr_size
    )
  | Bvmanyarith _ ->
      let+ typs = map_lift (fun tval -> tval.ctyp) tvals in
      let constexpr = List.for_all Ctype.is_constexpr typs in
      let size = Ctype.sizeof @@ List.hd typs in
      Ctype.machine ~constexpr size
  | Concat -> (
      (* HACK If concatenating a extracted pointer to thing on the right,
         then it stays a pointer.*)
      match List.hd tvals with
      | {
       exp = Unop (Extract (_, _), _, _);
       ctyp = Some ({ unqualified = Ptr { fragment = Global _; offset; _ }; _ } as ctyp);
      } -> (
          match offset with
          | Somewhere -> Some ctyp
          | Const _ -> (
              try
                let new_int =
                  constexpr_to_int ~ctxt (Typed.concat (List.map (fun t -> t.exp) tvals))
                in
                debug "concat hack: %x = %t" new_int
                  (Pp.top Base.pp_exp (Typed.concat (List.map (fun t -> t.exp) tvals)));
                Some (Ctype.ptr_set ctyp new_int)
              with ConcreteEval.Symbolic -> Ctype.ptr_forget ctyp |> Option.some
            )
        )
      | _ -> None
    )

(** Stage 1 expression typer *)
let rec expr ~ctxt (exp : Base.exp) : Ctype.t option =
  let ctyp =
    match exp with
    | Var (Register reg, _) -> State.get_reg ctxt.state reg |> State.Tval.ctyp
    | Var (Read (r, _), _) -> HashVector.get ctxt.mem_reads r |> State.Tval.ctyp
    | Var (NonDet _, _) -> None
    | Var (Segment _, _) -> None (* TODO? *)
    | Bits (bv, _) ->
        let size = BitVec.size bv in
        if size mod 8 = 0 || size = Arch.address_size then
          Ctype.machine ~constexpr:true (size / 8) |> Option.some
        else None
    | Bool _ -> None
    | Enum _ -> None
    | Vec _ -> None
    | Unop (u, e, _) ->
        let tval = expr_tval ~ctxt e in
        Option.(
          unop u tval
          |||
          if is_const tval then
            constexpr_of_exp exp
          else
            None
        )
    | Binop (b, e, e', _) ->
        let te = expr_tval ~ctxt e in
        let te' = expr_tval ~ctxt e' in
        Option.(
          binop ~ctxt b te te'
          |||
          if is_const te && is_const te' then
            constexpr_of_exp exp
          else
            None
        )
    | Manyop (m, el, _) ->
        let tvals = List.map (expr_tval ~ctxt) el in
        Option.(
          manyop ~ctxt m tvals
          |||
          if List.for_all is_const tvals then
            constexpr_of_exp exp
          else
            None
        )
    | Ite _ -> None
    | Bound _ -> .
    | Let _ -> .
  in
  debug "Typing %t with %t" Pp.(top Base.pp_exp exp) Pp.(top (opt Ctype.pp) ctyp);
  ctyp

(** Same as {!expr} but return the expression in a {!Tval} *)
and expr_tval ~ctxt exp =
  let ctyp = expr ~ctxt exp in
  { exp; ctyp }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:read Memory Read}

    When reading memory there are two main cases:
    - The address has a pointer type, in which case we figure out the type of
      the read according to that pointer type.
    - The address do not have a pointer type, so we try to do an untyped read
      with {!State.read_noprov} which may fail.*)

let fragment_at ~(dwarf : Dw.t) ~fenv ~size (frag : Ctype.fragment) at : Ctype.t option =
  let open Option in
  let env = dwarf.tenv in
  match frag with
  | Unknown -> Ctype.machine size |> Option.some
  | Single t -> Ctype.type_at ~env ~size t at
  | DynArray t ->
      let at = at mod Ctype.sizeof t in
      (* TODO fix for padding *)
      Ctype.type_at ~env ~size t at
  | DynFragment i ->
      let frag = Fragment.Env.get fenv i in
      let* (typ, off) = Fragment.at_off_opt frag at in
      Ctype.type_at ~env ~size typ off
  | Global s -> (
      match Elf.SymTable.of_addr_with_offset_opt dwarf.elf.symbols Elf.Address.{ section = s; offset = at } with
      | Some (sym, offset) -> (
          match Hashtbl.find_opt dwarf.vars sym.name with
          | Some v -> Ctype.type_at ~env ~size v.ctype offset
          | None -> Ctype.machine size |> Option.some
        )
      | None -> Ctype.machine size |> Option.some
    )

let ptr_deref ~dwarf ~fenv ~size frag (offset : Ctype.offset) : Ctype.t option =
  match offset with
  | Const at -> fragment_at ~dwarf ~fenv ~size frag at
  | Somewhere -> Ctype.machine size |> Option.some

(** Does the same as {!State.read}, but additionally take care of reading the type from a fragment
    and marking the type of the read variable. *)
let[@warning "-16"] read ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) ~addr ~size : State.tval =
  match ptrtype with
  | Some { unqualified = Ptr { fragment; offset; provenance }; _ } ->
      let fenv = s.fenv in
      let bsize = Ast.Size.to_bytes size in
      let ctyp = ptr_deref ~dwarf ~fenv ~size:bsize fragment offset in
      let exp = State.read ~provenance ?ctyp s ~addr ~size in
      { exp; ctyp }
  | Some _ ->
      warn "Reading from non-ptr unimplemented for now";
      State.read_noprov s ~addr ~size |> State.Tval.of_exp
  | None -> State.read_noprov s ~addr ~size |> State.Tval.of_exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:write Memory Write} *)

let fragment_write_at ~dwarf:(_ : Dw.t) ~fenv ~(ctyp : Ctype.t) (frag : Ctype.fragment) at : unit
    =
  match frag with
  | DynFragment i ->
      debug "Writing at %t in %d: %t" (Pp.top Pp.shex at) i (Pp.top Ctype.pp ctyp);
      let original = Fragment.Env.get fenv i in
      let cleared = Fragment.clear original ~pos:at ~len:(Ctype.sizeof ctyp) in
      let newfrag = Fragment.add cleared at ctyp in
      Fragment.Env.set fenv i newfrag
  | _ -> ()

let ptr_write ~dwarf ~fenv ~ctyp frag (offset : Ctype.offset) : unit =
  match offset with Const at -> fragment_write_at ~dwarf ~fenv ~ctyp frag at | Somewhere -> ()

(** Does the same as {!State.write}, but additionally take care of writing the type
    if the write is on a {!Ctype.FreeFragment}.*)
let write ~(dwarf : Dw.t) (s : State.t) ?(ptrtype : Ctype.t option) ~addr ~size
    (value : State.tval) : unit =
  match ptrtype with
  | Some { unqualified = Ptr { fragment; offset; provenance }; _ } ->
      let fenv = s.fenv in
      Option.iter (fun ctyp -> ptr_write ~dwarf ~fenv ~ctyp fragment offset) value.ctyp;
      State.write ~provenance s ~addr ~size value.exp
  | _ ->
      warn "Writing without provenance";
      State.write_noprov s ~addr ~size value.exp
