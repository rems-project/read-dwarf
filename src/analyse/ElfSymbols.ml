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

(*****************************************************************************)
(**       look up address in ELF symbol table                                *)

(*****************************************************************************)
open Logs.Logger (struct
  let str = __MODULE__
end)

open Utils

open ElfTypes
open ControlFlowTypes

(*
let elf_symbols_of_address (test : test) (addr : natural) : string list =
  List.filter_map
    (fun (name, (_typ, _size, address, _mb, _binding)) ->
      if address = addr then Some name else None)
    test.symbol_map

let mk_elf_symbols test instructions : string list array =
  Array.map (function i -> elf_symbols_of_address test i.i_addr) instructions

let address_of_elf_symbol test (s : string) : addr option =
  List.find_map
    (fun (name, (_typ, _size, address, _mb, _binding)) -> if s = name then Some address else None)
    test.symbol_map
 *)



(* this is naively doing address comparison w.r.t. the elf64_st_value field of the symbol table entry, ignoring the " elf64_st_shndx : elf64_half (*Section header index symbol is defined with respect to*)" *)

(* this is naively doing repeated lookups of names - it would be better to precompute them *)
   
let elf_symbols_of_address (test : test) (addr : natural) : string list =
  let (symtab,strtab)=test.symbol_map in 
  List.filter_map
    (fun (entry:Elf_symbol_table.elf64_symbol_table_entry) -> 
      if Ml_bindings.nat_big_num_of_uint64 (*Elf_types_native_uint.natural_of_elf64_addr*) entry.elf64_st_value = addr then
        let name = Uint32_wrapper.to_bigint (*natural_of_elf64_word*) entry.elf64_st_name in
        match String_table.get_string_at name strtab with
        | Success s -> Some s
        | Fail e -> fatal "elf_symbols_of_address strtab lookup %s" e
      else
        None
    )
    symtab

let mk_elf_symbols test instructions : string list array =
  Array.map (function i -> elf_symbols_of_address test i.i_addr) instructions

let address_of_elf_symbol test (s : string) : addr option = 
  let (symtab,strtab)=test.symbol_map in 
  let addrs =
    List.filter_map
      (fun (entry:Elf_symbol_table.elf64_symbol_table_entry) ->
        let name = Uint32_wrapper.to_bigint (*natural_of_elf64_word*) entry.elf64_st_name in
        match String_table.get_string_at name strtab with
        | Success s' ->
           if s'=s then
             let addr = Ml_bindings.nat_big_num_of_uint64 (*Elf_types_native_uint.natural_of_elf64_addr*) entry.elf64_st_value in
             Some addr
           else
             None
        | Fail e -> fatal "elf_symbols_of_address strtab lookup %s" e
      )
      symtab in
  match addrs with
  | [] -> None
  | [addr] -> Some addr
  | _ -> fatal "address_of_elf_symbol found multiple addresses for %s" s

    
