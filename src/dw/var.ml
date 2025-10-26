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

(** This module contain all the definition to handle local and global variables
    as defined in the DWARF information of the target file *)

type range = Addr.t * Addr.t option

(** Type of a DWARF variable *)
type t = {
  name : string;
  param : bool;
  ctype : Ctype.t;
  locs : (range * Loc.t) list;
  locs_frame_base : (range * Loc.t) list;
}

(** Type of a DWARF variable in linksem *)
type linksem_t = Dwarf.sdt_variable_or_formal_parameter

(** Merge contiguous location lists *)
let rec loc_merge = function
  | ((a1, b1), d1) :: ((a2, b2), d2) :: l when b1 = Some a2 && Loc.compare d1 d2 = 0 ->
      loc_merge (((a1, b2), d1) :: l)
  | a :: l -> a :: loc_merge l
  | [] -> []

let end_addr_of_sym = function
| Sym_ocaml.Num.Absolute z when Z.compare z (Z.of_int Int.max_int) > 0 -> None
| x -> Some (Addr.of_sym x)

(** Create a DWARF variable from its linksem counterpart *)
let of_linksem (elf : Elf.File.t) (env : Ctype.env) (lvar : linksem_t) : t =
  let name = lvar.svfp_name in
  let param = match lvar.svfp_kind with SVPK_var -> false | SVPK_param -> true in
  let ctype =
    match lvar.svfp_type with
    | Some t -> Ctype.of_linksem ~env t
    | None -> Ctype.of_linksem_none ()
  in
  let locs =
    lvar.svfp_locations |> Option.value ~default:[]
    |> List.map (fun (a, b, l) -> ((Addr.of_sym a, end_addr_of_sym b), Loc.of_linksem elf l))
    |> loc_merge
  in
  let locs_frame_base =
    lvar.svfp_locations_frame_base |> Option.value ~default:[]
    |> List.map (fun (a, b, l) -> ((Addr.of_sym a, end_addr_of_sym b), Loc.of_linksem elf l))
    |> loc_merge
  in
  { name; param; ctype; locs; locs_frame_base }

(** Pretty print a variable *)
let pp_raw v =
  let kind = if v.param then "param" else "var" in
  Pp.(
    record kind
      [
        ("name", string v.name);
        ("ctype", Ctype.pp v.ctype);
        ("locs", list (pair (pair Addr.pp (opt Addr.pp)) Loc.pp) v.locs);
      ])
