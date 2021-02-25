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

(* The documentation is in the mli file *)

open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*        Paths                                                              *)
(*****************************************************************************)

module Path = struct
  type t = string list

  let to_string = String.concat "."

  let of_string = String.split_on_char '.'

  let pp = to_string %> Pp.string
end

(*****************************************************************************)
(*        Registers                                                          *)
(*****************************************************************************)

type t = int

type ty = Ast.no Ast.ty

let pp_ty ty = ty |> Ast.Manip.ty_allow_mem |> Ast.pp_ty

let index : (Path.t, ty) IdMap.t = IdMap.make ()

let pp_index () = IdMap.pp ~keys:Path.pp ~vals:pp_ty index

let mem_path = IdMap.mem index

let mem_string = Path.of_string %> mem_path

let of_path = IdMap.to_ident index

let to_path = IdMap.of_ident index

let of_string = Path.of_string %> of_path

let to_string = to_path %> Path.to_string

let num () = IdMap.length index

let reg_type = IdMap.geti index

let path_type = IdMap.getk index

let add path ty =
  debug "Adding register %s with type %t" (Path.to_string path) (Pp.top pp_ty ty);
  IdMap.add index path ty

let ensure_add path ty =
  match IdMap.to_ident_opt index path with
  | Some reg ->
      let ty' = IdMap.unsafe_geti index reg in
      if ty <> ty' then
        Raise.fail
          "Reg.ensure_adds: Trying to add %s with type %t but it is already in with type %t"
          (Path.to_string path)
          Pp.(tos pp_ty ty)
          Pp.(tos pp_ty ty')
      else reg
  | None -> add path ty

let adds path ty = add path ty |> ignore

let ensure_adds path ty = ensure_add path ty |> ignore

let iter f = IdMap.iter f index

let seq_all () = Seq.iota (num ())

let equal = ( = )

let pp reg = reg |> to_string |> Pp.string

(*****************************************************************************)
(*        Register maps                                                      *)
(*****************************************************************************)

module Map = struct
  type reg = t

  let pp_reg = pp

  include FullVec

  let init = make

  let reinit map gen = set_after map 0 gen

  let map_mut_current f map = map_mut_until ~limit:(num ()) f map

  let iter f map = iter_until ~limit:(num ()) f map

  let iteri f map = iteri_until ~limit:(num ()) f map

  let bindings map = get_vec_until map (num ()) |> Vec.to_listi

  let pp conv map = map |> bindings |> List.map (Pair.map pp_reg conv) |> Pp.mapping "regmap"
end
