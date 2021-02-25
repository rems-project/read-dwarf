(*==================================================================================*)
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

(** This module try to provide generic generators and arbitrary to work with expressions.

    For now expression do not use enumeration since the Z3 back-end don't support enumeration yet.

*)

open Common

type ('v, 'm) exp = ('v, 'm) Exp.Typed.t

(** Test variables to instantiate variable dependent functors for testing. *)
module Var = struct
  type t = int * Ast.no Ast.ty

  let ty (_, ty) = ty

  let pp ((num, ty) : t) =
    let open Ast in
    let string_of_ty = function
      | Ty_Bool -> "bool"
      | Ty_BitVec n -> string_of_int n
      | _ -> assert false
    in
    Pp.dprintf "%d:%s" num (string_of_ty ty)

  let equal = ( = )

  let hash = Hashtbl.hash

  let of_string s : t =
    let open Ast in
    match String.split_on_char ':' s with
    | [num; "bool"] -> (int_of_string num, Ty_Bool)
    | [num; size] -> (int_of_string num, Ty_BitVec (int_of_string size))
    | _ -> Raise.inv_arg "Invalid test variable: %s" s
end

module ExpT = Exp.Make (Var)
module Z3 = Z3.Make (Var)

module Gen = struct
  open Gen
  open Ast

  type ('v, 'm) exp = ('v, 'm) Exp.Typed.t

  let typ =
    oneof
      [
        return Ty_Bool;
        (let+ size = small_nat in
         Ty_BitVec (size + 1));
      ]

  let var_from_ty ty =
    let+ num = nat in
    (num, ty)

  let bv_var size = var_from_ty (Ty_BitVec size)

  let bool_var = var_from_ty Ty_Bool

  let bitvec_size : BitVec.t sized =
   fun size ->
    if size <= Sys.int_size then
      let+ a = int in
      BitVec.of_int ~size a
    else if size mod 4 = 0 then
      let+ str = string_size ~gen:BytesSeqT.hex_digit.gen (return (size / 4)) in
      BitVec.of_string ~base:16 ~size str
    else
      let bin_digit_gen = oneofl ['0'; '1'] in
      let+ str = string_size ~gen:bin_digit_gen (return size) in
      BitVec.of_string ~base:16 ~size str

  let bitvec =
    let* size = small_nat in
    bitvec_size size

  type ('v, 'm) gen_param = {
    typ : no ty;  (** Cannot contain an enum *)
    size : int;
    bv_atom_gen : ('v, 'm) exp sized;
    bool_atom_gen : ('v, 'm) exp t;
  }

  (** Generate an atom according to the [params] *)
  let atom ~params =
    match params.typ with
    | Ty_BitVec bvsize ->
        assert (bvsize > 0);
        params.bv_atom_gen bvsize
    | Ty_Bool -> params.bool_atom_gen
    | _ -> assert false

  let unop = function
    | Ty_BitVec bvsize as ty ->
        assert (bvsize > 0);
        let extract =
          let* add_end = small_nat in
          let* add_beg = small_nat in
          return (Extract (add_beg + bvsize - 1, add_beg), Ty_BitVec (add_end + bvsize + add_beg))
        in
        if bvsize >= 2 then
          let extend =
            let* orignal_size = 1 -- (bvsize - 1) in
            let ext_size = bvsize - orignal_size in
            let* ext = oneofl [SignExtend ext_size; ZeroExtend ext_size] in
            return (ext, Ty_BitVec orignal_size)
          in
          oneof [oneofl [(Bvnot, ty); (Bvneg, ty)]; extract; extend]
        else if bvsize = 1 then
          let red =
            let+ bvsize = small_nat and+ red = oneofl [Bvredand; Bvredor] in
            (red, Ty_BitVec (bvsize + 1))
          in
          oneof [oneofl [(Bvnot, ty); (Bvneg, ty)]; extract; red]
        else oneof [oneofl [(Bvnot, ty); (Bvneg, ty)]; extract]
    | Ty_Bool -> return (Not, Ty_Bool)
    | _ -> assert false

  let binop = function
    | Ty_BitVec bvsize as ty ->
        assert (bvsize > 0);
        let+ arith =
          oneofl
            [
              Bvnand;
              Bvnor;
              Bvxnor;
              Bvsub;
              Bvudiv;
              Bvudivi;
              Bvsdiv;
              Bvsdivi;
              Bvurem;
              Bvsrem;
              Bvsmod;
              Bvshl;
              Bvlshr;
              Bvashr;
            ]
        in
        (Bvarith arith, ty, ty)
    | Ty_Bool ->
        let eq =
          let* typ = typ in
          return (Eq, typ, typ)
        in
        let comp =
          let* comp = oneofl [Bvult; Bvslt; Bvule; Bvsle; Bvuge; Bvsge; Bvugt; Bvsgt] in
          let* size = small_nat in
          return (Bvcomp comp, Ty_BitVec (size + 1), Ty_BitVec (size + 1))
        in
        oneof [eq; comp]
    | _ -> assert false

  let manyop = function
    | Ty_BitVec size as ty ->
        assert (size > 0);
        let manyarith =
          let* op = oneofl [Bvand; Bvor; Bvxor; Bvadd; Bvmul] in
          let* num = 2 -- 7 in
          return (Bvmanyarith op, List.repeat num ty)
        in
        if size >= 2 then
          let concat =
            let* num = 2 -- min size 7 in
            (* This will not generate an uniform integer partition but hopefully,
                                   it's not too ugly and fast enough *)
            let gen_partition =
              fix (fun self (num, size) ->
                  assert (size >= num);
                  if num = 1 then return [size]
                  else
                    let* next = 1 -- min (size - num + 1) (size / num * 2) in
                    let* rest = self (num - 1, size - next) in
                    return (next :: rest))
            in
            let* partition = gen_partition (num, size) in
            return
              ( Concat,
                List.map
                  (fun s ->
                    assert (s > 0);
                    Ty_BitVec s)
                  partition )
          in
          oneof [manyarith; concat]
        else manyarith
    | Ty_Bool ->
        let* op = oneofl [And; Or] in
        let* num = 2 -- 7 in
        return (op, List.repeat num Ty_Bool)
    | _ -> assert false

  let exp_from_params params =
    fix
      (fun self params ->
        if params.size = 0 then atom ~params
        else
          let atom = atom ~params in
          let unop =
            let* (op, ntyp) = unop params.typ in
            let* exp = self { params with typ = ntyp; size = params.size - 1 } in
            return @@ Exp.Typed.unop op exp
          in
          let binop =
            let* (op, ntyp, ntyp') = binop params.typ in
            let* exp = self { params with typ = ntyp; size = params.size / 2 } in
            let* exp' = self { params with typ = ntyp'; size = params.size / 2 } in
            return @@ Exp.Typed.binop op exp exp'
          in
          let manyop =
            let* (op, typs) = manyop params.typ in
            let num = List.length typs in
            let* exps =
              flatten_l
                (List.map (fun typ -> self { params with typ; size = params.size / num }) typs)
            in
            return @@ Exp.Typed.manyop op exps
          in
          let ite =
            let* cond = self { params with typ = Ty_Bool; size = params.size / 3 } in
            let* expt = self { params with size = params.size / 3 } in
            let* expf = self { params with size = params.size / 3 } in
            return @@ Exp.Typed.ite ~cond expt expf
          in
          let+ exp = oneof [atom; unop; binop; manyop; ite] in
          assert (Exp.Typed.get_type exp = params.typ);
          exp)
      params

  let bv_consts size =
    assert (size > 0);
    bitvec_size size >|= Exp.Typed.bits

  let bv_atom_from_var (var_gen : 'v sized) size =
    let var = var_gen size >|= Exp.Typed.var ~typ:(Ty_BitVec size) in
    oneof [var; bv_consts size]

  let bv_atom_with_var = bv_atom_from_var bv_var

  let bool_consts = bool >|= Exp.Typed.bool

  let bool_atom_from_var (var_gen : 'v t) =
    let var = var_gen >|= Exp.Typed.var ~typ:Ty_Bool in
    oneof [var; bool_consts]

  let bool_atom_with_var = bool_atom_from_var bool_var
end

(** Propagate an existing shrinker, by try it on all descendants of this. The
The provided shrinker must preserve the type.*)
let rec shrink_propagate (shrinker : ('v, 'm) Exp.Typed.t Q.Shrink.t) exp yield =
  (* Pp.(eprintln @@ dprintf "testing:" ^^ ExpT.pp exp); *)
  let open Ast in
  shrinker exp yield;
  match exp with
  | Unop (op, e, t) -> shrink_propagate shrinker e (fun e -> Unop (op, e, t) |> yield)
  | Binop (op, e, e', t) ->
      shrink_propagate shrinker e (fun e -> Binop (op, e, e', t) |> yield);
      shrink_propagate shrinker e' (fun e' -> Binop (op, e, e', t) |> yield)
  | Manyop (op, el, t) ->
      for i = 0 to List.length el - 1 do
        shrink_propagate shrinker (List.nth el i) (fun e ->
            let nl = List.set_nth el i e in
            Manyop (op, nl, t) |> yield)
      done
  | Ite (c, e, e', t) ->
      shrink_propagate shrinker c (fun c -> Ite (c, e, e', t) |> yield);
      shrink_propagate shrinker e (fun e -> Ite (c, e, e', t) |> yield);
      shrink_propagate shrinker e' (fun e' -> Ite (c, e, e', t) |> yield)
  | _ -> ()

(** Generate an expression arbitrary from a generator (and optionally a [shrink]er) *)
let from_gen ?shrink (gen : ('v, 'm) exp Q.Gen.t) =
  let print e = Pp.tos ExpT.pp e () in
  Q.make ?shrink ~print gen
