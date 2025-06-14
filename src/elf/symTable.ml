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

open Logs.Logger (struct
  let str = __MODULE__
end)

open Symbol

type sym = Symbol.t

type linksem_sym = Symbol.linksem_t

type sym_offset = sym * int

module RMap = RngMap.Make (Symbol)
module SMap = Map.Make (String)

module AddrMap = struct
  type t = RMap.t SMap.t

  let add t (addr: Address.t) sym =
    SMap.update addr.section (fun old ->
      let old = match old with
      | None -> RMap.empty
      | Some x -> x
      in
      Some (RMap.add old addr.offset sym)
    ) t
  
  let update f t (addr: Address.t) =
    SMap.update addr.section (Option.map (fun x -> RMap.update f x addr.offset)) t
  
  let empty = SMap.empty

  let at t (addr: Address.t) =
    SMap.find addr.section t |> Fun.flip RMap.at addr.offset

  let at_opt t (addr: Address.t) =
    Option.bind (SMap.find_opt addr.section t) @@ Fun.flip RMap.at_opt addr.offset
  
  let at_off t (addr: Address.t) =
    SMap.find addr.section t |> Fun.flip RMap.at_off addr.offset
  
  let at_off_opt t (addr: Address.t) =
    Option.bind (SMap.find_opt addr.section t) @@ Fun.flip RMap.at_off_opt addr.offset
  
  let bindings t =
    let sections = SMap.bindings t in
    List.bind sections @@ fun (section, rmap) ->
      let inner_bindings = RMap.bindings rmap in
      List.map (fun (offset, sym) -> (Address.{section; offset}, sym)) inner_bindings
  
  
end

type linksem_t = LinksemRelocatable.global_symbol_init_info

type t = { by_name : sym SMap.t; by_addr : AddrMap.t }

let empty = { by_name = SMap.empty; by_addr = AddrMap.empty }

let add t sym =
  let by_name = SMap.add sym.name sym t.by_name in
  try { by_name; by_addr = AddrMap.add t.by_addr sym.addr sym }
  with Invalid_argument _ ->
    let updated = ref false in
    let by_addr =
      AddrMap.update
        (fun usym ->
          if usym.addr = sym.addr && usym.size = sym.size then begin
            updated := true;
            push_name sym.name usym
          end
          else Raise.fail "Failed to merge %s in %s" usym.name sym.name)
        t.by_addr sym.addr
    in
    if !updated then { by_name; by_addr }
    else Raise.fail "Coudln't insert symbol %s, it didn't fit in" sym.name

let of_name t name =
  try SMap.find name t.by_name with Not_found -> fail "not found symbol %s" name

let of_name_opt t name = SMap.find_opt name t.by_name

let of_addr t addr = AddrMap.at t.by_addr addr

let of_addr_opt t addr = AddrMap.at_opt t.by_addr addr

let of_addr_with_offset t addr = AddrMap.at_off t.by_addr addr

let of_addr_with_offset_opt t addr = AddrMap.at_off_opt t.by_addr addr

let to_addr_offset (sym, offset) = Address.(sym.addr + offset)

let string_of_sym_offset ((sym, off) : sym_offset) = sym.name ^ "+" ^ string_of_int off

let sym_offset_of_string t s : sym_offset =
  match String.split_on_char '+' s with
  | [ssym] -> (of_name t (String.trim ssym), 0)
  | [ssym; soff] -> (of_name t (String.trim ssym), soff |> String.trim |> int_of_string)
  | _ -> Raise.fail "sym_offset_to_string: wrong format: %s" s

let of_position_string t s : sym_offset =
  let s = String.trim s in
  if s = "" then raise Not_found;
  if s.[0] = '0' then raise Not_found (* no absolute addresses *)
  else sym_offset_of_string t s

let of_linksem linksem_map =
  let add_linksem_sym_to_map (map : t) (lsym : linksem_sym) =
    if is_interesting_linksem lsym then add map (Symbol.of_linksem lsym) else map
  in
  List.fold_left add_linksem_sym_to_map empty linksem_map

let pp_raw st = AddrMap.bindings st.by_addr |> List.map (Pair.map Address.pp pp_raw) |> Pp.mapping "syms"

let iter t f = SMap.iter (fun _ value -> f value) t.by_name

let fold t e f = SMap.fold (fun _ value -> f value) t.by_name e
