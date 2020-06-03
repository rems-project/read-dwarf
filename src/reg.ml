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

  let pp = to_string %> PP.string
end

(*****************************************************************************)
(*        Registers                                                          *)
(*****************************************************************************)

type t = int

type ty = Ast.no Ast.ty

let pp_ty ty = ty |> AstManip.ty_allow_mem |> Ast.pp_ty

let index : (Path.t, ty) IdMap.t = IdMap.make ()

let pp_index () = IdMap.pp ~keys:Path.pp ~vals:pp_ty index

let mem = IdMap.mem_id index

let mem_path = IdMap.mem index

let mem_string = Path.of_string %> mem_path

let of_path = IdMap.to_ident index

let to_path = IdMap.of_ident index

let of_string = Path.of_string %> of_path

let to_string = to_path %> Path.to_string

let num () = IdMap.length index

let reg_type = IdMap.geti index

let path_type = IdMap.getk index

let add = IdMap.add index

let ensure_add path ty =
  match IdMap.to_ident_opt index path with
  | Some reg ->
      let ty' = IdMap.unsafe_geti index reg in
      if ty <> ty' then
        Raise.fail
          "Reg.ensure_adds: Trying to add %s with type %t but it is already in with type %t"
          (Path.to_string path)
          PP.(tos pp_ty ty)
          PP.(tos pp_ty ty')
      else reg
  | None -> add path ty

let adds = IdMap.adds index

let ensure_adds path ty = ensure_add path ty |> ignore

let iter f = IdMap.iter f index

let seq_all () = Seq.iota (num ())

let equal = ( = )

let pp reg = reg |> to_string |> PP.string

(*****************************************************************************)
(*        Register maps                                                      *)
(*****************************************************************************)

module PMap = struct
  type reg = t

  let pp_reg = pp

  include HashVector

  let pp conv pm =
    pm |> HashVector.bindings |> List.map (Pair.map pp_reg conv) |> PP.mapping "regpmap"
end

module Map = struct
  type reg = t

  let pp_reg = pp

  include FullVec

  let dummy () = make (fun _ -> failwith "Reg.Map.dummy used")

  let init = make

  let reinit map gen = set_after map 0 gen

  let map_mut_current f map = map_mut_until ~limit:(num ()) f map

  let iter f map = iter_until ~limit:(num ()) f map

  let iteri f map = iteri_until ~limit:(num ()) f map

  let bindings map = get_vec_until map (num ()) |> Vec.to_listi

  let pp conv map = map |> bindings |> List.map (Pair.map pp_reg conv) |> PP.mapping "regmap"
end
