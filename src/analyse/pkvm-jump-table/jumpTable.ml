(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Linux static keys ("jump labels"): the test sites recorded in an ELF section (normally
    [__jump_table]), as linksem reads and models them ([Pkvm_jump_table]). This module adds what a
    viewer of an object needs: a file's entries indexed by site and by branch target, addresses in
    the form the objdump-derived instructions carry, and the word linksem says a site holds. *)

open Utils
open Pkvm_jump_table

open Logs.Logger (struct
  let str = __MODULE__
end)

module SymMap = Map.Make (struct
  type t = Sym.t

  let compare = Sym.Ordered.compare
end)

type entry = jump_entry

type table = {
  section_name : string;
  entries : entry array;  (** in section order *)
  by_code : entry list SymMap.t;  (** entries indexed by the address of their site *)
  by_target : entry list SymMap.t;  (** entries indexed by the address of their branch target *)
  table_problems : string list;
  elf : Elf_file.elf64_file;
}

let code_addr (e : entry) : addr =
  let (s, o) = e.je_code in
  Sym_ocaml.Num.Offset (s, o)

let target_addr (e : entry) : addr =
  let (s, o) = e.je_target in
  Sym_ocaml.Num.Offset (s, o)

let key_name (e : entry) : string =
  match static_key_name e with Some n -> n | None -> Symbolic_resolution.string_of_sym_expr e.je_key

let flag_name (e : entry) : string = "static_key:" ^ key_name e

(** [table_of_elf f64 section_name] is [None] iff [f64] has no section of that name. If linksem
    cannot read the section, the table has no entries and says why. *)
let table_of_elf (f64 : Elf_file.elf64_file) (section_name : string) : table option =
  let has_section =
    List.exists
      (fun (s : Elf_interpreted_section.elf64_interpreted_section) ->
        s.elf64_section_name_as_string = section_name
      )
      f64.elf64_file_interpreted_sections
  in
  if not has_section then None
  else
    let (entries, table_problems) =
      match read_jump_table_of_section f64 section_name with
      | Error.Success es -> (Array.of_list es, [])
      | Error.Fail m ->
          warn "jump table: linksem could not read %s: %s" section_name m;
          ([||], ["linksem could not read the section: " ^ m])
    in
    let index key =
      Array.fold_left
        (fun m e ->
          let a = key e in
          let old = try SymMap.find a m with Not_found -> [] in
          SymMap.add a (old @ [e]) m
        )
        SymMap.empty entries
    in
    info "jump table: %d entries in %s" (Array.length entries) section_name;
    Some
      { section_name; entries; by_code = index code_addr; by_target = index target_addr; table_problems; elf = f64 }

let at_code (t : table) (a : addr) : entry list = try SymMap.find a t.by_code with Not_found -> []
let at_target (t : table) (a : addr) : entry list = try SymMap.find a t.by_target with Not_found -> []

(** The word linksem's model says the site holds ([b target] iff the key is enabled xor the
    branch bit, else [nop]), over the site's section link-time relocated; or why linksem cannot
    say *)
let word (t : table) (e : entry) : (Symbolic_resolution.sym_expr, string) result =
  match RelocatedSections.relocated_list t.elf [fst e.je_code] with
  | Error m -> Error m
  | Ok secs -> (
      match jump_entry_words t.elf e secs with Error.Success (w, _) -> Ok w | Error.Fail m -> Error m )

(** The distinct keys of the table with the number of sites of each, most sites first *)
let key_counts (entries : entry list) : (string * int) list =
  let counts = Hashtbl.create 8 in
  List.iter (fun e -> let k = key_name e in Hashtbl.replace counts k (1 + try Hashtbl.find counts k with Not_found -> 0)) entries;
  List.sort
    (fun (k1, n1) (k2, n2) -> if n1 <> n2 then compare n2 n1 else compare k1 k2)
    (Hashtbl.fold (fun k n acc -> (k, n) :: acc) counts [])
