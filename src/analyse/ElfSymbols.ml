(*****************************************************************************)
(**       look up address in ELF symbol table                                *)

(*****************************************************************************)

open Utils
open ElfTypes
open ControlFlowTypes

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
