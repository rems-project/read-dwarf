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

(** Finding sdt (DWARF simple-die-tree) subroutines by various predicates *)

open Logs.Logger (struct
  let str = __MODULE__
end)

let rec find_sdt_subroutine_subroutine (recurse : bool) (p : Dwarf.sdt_subroutine -> bool)
    (ss : Dwarf.sdt_subroutine) : Dwarf.sdt_subroutine list =
  if recurse then
    let sss1 =
      List.concat_map (find_sdt_subroutine_subroutine recurse p) ss.ss_subroutines
      @ List.concat_map (find_sdt_subroutine_lexical_block recurse p) ss.ss_lexical_blocks
    in
    if p ss then ss :: sss1 else sss1
  else if p ss then [ss]
  else []

and find_sdt_subroutine_lexical_block (recurse : bool) (p : Dwarf.sdt_subroutine -> bool)
    (lb : Dwarf.sdt_lexical_block) : Dwarf.sdt_subroutine list =
  let sss1 =
    List.concat_map (find_sdt_subroutine_subroutine recurse p) lb.slb_subroutines
    @ List.concat_map (find_sdt_subroutine_lexical_block recurse p) lb.slb_lexical_blocks
  in
  sss1

and find_sdt_subroutine_compilation_unit (recurse : bool) (p : Dwarf.sdt_subroutine -> bool)
    (cu : Dwarf.sdt_compilation_unit) : Dwarf.sdt_subroutine list =
  List.concat_map (find_sdt_subroutine_subroutine recurse p) cu.scu_subroutines

and find_sdt_subroutine_dwarf (recurse : bool) (p : Dwarf.sdt_subroutine -> bool)
    (d : Dwarf.sdt_dwarf) : Dwarf.sdt_subroutine list =
  List.concat_map (find_sdt_subroutine_compilation_unit recurse p) d.sd_compilation_units

let find_sdt_subroutine_by_name (recurse : bool) (sdt_d : Dwarf.sdt_dwarf) (s : string) :
    Dwarf.sdt_subroutine option =
  let p (ss : Dwarf.sdt_subroutine) = match ss.ss_name with None -> false | Some s' -> s' = s in
  match find_sdt_subroutine_dwarf recurse p sdt_d with
  | [] -> None
  | [ss] -> Some ss
  | _ -> fatal "find_sdt_subroutine_by_name for \"%s\" found multiple matching subroutines" s

let find_sdt_subroutine_by_entry_address (sdt_d : Dwarf.sdt_dwarf) addr :
    Dwarf.sdt_subroutine list =
  let p (ss : Dwarf.sdt_subroutine) =
    match ss.ss_entry_address with None -> false | Some addr' -> addr' = addr
  in
  match find_sdt_subroutine_dwarf true p sdt_d with
  | [] -> fatal "find_sdt_subroutine_by_entry_address found no matching subroutines"
  | sss -> sss

let address_of_subroutine_name (sdt_d : Dwarf.sdt_dwarf) (s : string) =
  match find_sdt_subroutine_by_name false sdt_d s with
  | None -> None
  | Some ss -> ss.ss_entry_address
