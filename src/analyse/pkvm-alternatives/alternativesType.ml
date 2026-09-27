(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Linux arm64 "alternatives": the runtime instruction-patching entries found in an ELF section
    (normally [.altinstructions]), as linksem reads and models them ([Pkvm_alternatives],
    [Generated_arm64_cpucaps]). This module adds only what a viewer of an object needs on top of
    linksem's [alt_entry]: a file's entries indexed by the address of their original code,
    addresses in the form the objdump-derived instructions carry, the two sides of an entry as
    an [action_ref], the kernel's [BUG_ON] sanity checks as per-entry problems, and the words
    linksem's model says an entry writes ([Pkvm_alternatives.alt_entry_words]) over the code
    sections link-time relocated by linksem. *)

open Utils
open Pkvm_alternatives

open Logs.Logger (struct
  let str = __MODULE__
end)

(* Claude: a map keyed by (possibly section-relative) addresses; Sym.compare
   is partial, Sym.Ordered.compare is total *)
module SymMap = Map.Make (struct
  type t = Sym.t

  let compare = Sym.Ordered.compare
end)

type entry = alt_entry

let aarch64_insn_size = 4
let alt_entry_size = 12

type table = {
  section_name : string;
  entries : entry array;  (** in section order: entry [i] is at [section_name + 12 i] *)
  by_orig : entry list SymMap.t;  (** entries indexed by the address of their original code *)
  table_problems : string list;  (** problems with the section as a whole *)
  elf : Elf_file.elf64_file;
  site_sections : string list;
      (** the sections holding original or alternative sites; linksem relocates them
          ([RelocatedSections]) the first time an entry's words are rendered *)
}

(*****************************************************************************)
(*  views of an entry                                                        *)
(*****************************************************************************)

let entry_addr (t : table) (index : int) : addr =
  Sym_ocaml.Num.Offset (t.section_name, Z.of_int (alt_entry_size * index))

(* Claude: the objdump-derived instructions carry section-relative
   addresses, Offset (section, offset), which is also what linksem gives *)
let orig_addr (e : entry) : addr =
  let (s, o) = e.ae_orig in
  Sym_ocaml.Num.Offset (s, o)

let orig_len (e : entry) : int = Z.to_int e.ae_orig_len
let alt_len (e : entry) : int = Z.to_int e.ae_alt_len
let nr_inst (e : entry) : int = orig_len e / aarch64_insn_size
let is_callback (e : entry) : bool = e.ae_callback
let cap_number (e : entry) : int = Z.to_int e.ae_cap

(** The entry's cpucap in linksem's generated table, [None] if its number is beyond it *)
let cap (e : entry) : Generated_arm64_cpucaps.cpucap option =
  Generated_arm64_cpucaps.cpucap_of_number e.ae_cap

(* Claude: what the alternative site denotes: code to copy in, or the
   kernel function that patches the original words *)
type action_ref =
  | Replacement of addr  (** address of [alt_len] bytes of replacement code *)
  | Callback of string * alt_callback  (** the callback's symbol, and linksem's kind for it *)
  | Unresolved of string  (** could not be resolved; explanation *)

let action (e : entry) : action_ref =
  if e.ae_callback then
    match Symbolic_resolution.symbol_of_expr e.ae_alt with
    | Some name -> Callback (name, callback_of_symbol name)
    | None ->
        Unresolved
          ("callback entry whose alternative site is not a symbol: "
          ^ Symbolic_resolution.string_of_sym_expr e.ae_alt)
  else
    match Symbolic_resolution.section_offset_of_expr e.ae_alt with
    | Some (s, o) -> Replacement (Sym_ocaml.Num.Offset (s, o))
    | None ->
        Unresolved
          ("replacement entry whose alternative site is not in a section: "
          ^ Symbolic_resolution.string_of_sym_expr e.ae_alt)

(** The checks corresponding to the [BUG_ON]s in the kernel's [__apply_alternatives()], plus what
    linksem's model does not cover *)
let problems (e : entry) : string list =
  List.filter_map Fun.id
    [
      ( if orig_len e mod aarch64_insn_size <> 0 then
          Some (Printf.sprintf "orig_len %d is not a multiple of 4" (orig_len e))
        else None
      );
      ( if is_callback e && alt_len e <> 0 then
          Some (Printf.sprintf "callback entry with alt_len %d (kernel expects 0)" (alt_len e))
        else None
      );
      ( if (not (is_callback e)) && alt_len e <> orig_len e then
          Some
            (Printf.sprintf "replacement entry with alt_len %d <> orig_len %d" (alt_len e)
               (orig_len e)
            )
        else None
      );
      ( match cap e with
      | None ->
          Some
            (Printf.sprintf "cpucap %d is beyond the %d cpucaps of linksem's generated table"
               (cap_number e)
               (Z.to_int Generated_arm64_cpucaps.arm64_ncaps)
            )
      | Some _ -> None
      );
      ( match action e with
      | Unresolved why -> Some why
      | Callback (name, Cb_other _) -> Some ("callback " ^ name ^ " is not modelled by linksem")
      | Callback _ | Replacement _ -> None
      );
    ]

(*****************************************************************************)
(*  the table of a file                                                      *)
(*****************************************************************************)

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
      match read_alt_entries_of_section f64 section_name with
      | Error.Success es -> (Array.of_list es, [])
      | Error.Fail m ->
          warn "alternatives: linksem could not read %s: %s" section_name m;
          ([||], ["linksem could not read the section: " ^ m])
    in
    let by_orig =
      Array.fold_left
        (fun m e ->
          let a = orig_addr e in
          let old = try SymMap.find a m with Not_found -> [] in
          SymMap.add a (old @ [e]) m
        )
        SymMap.empty entries
    in
    let site_sections =
      List.sort_uniq compare
        (List.concat_map
           (fun e ->
             fst e.ae_orig
             :: (match action e with Replacement (Sym_ocaml.Num.Offset (s, _)) -> [s] | _ -> []))
           (Array.to_list entries)
        )
    in
    info "alternatives: %d entries in %s" (Array.length entries) section_name;
    Some
      {
        section_name;
        entries;
        by_orig;
        table_problems;
        elf = f64;
        site_sections;
      }

(*****************************************************************************)
(*  the words an entry writes                                                *)
(*****************************************************************************)

(** The (offset, word) of each instruction of the entry's footprint as linksem's model says it is
    patched (a replacement's words with [patch_alternative()]'s re-targeting, a callback's
    output over the environment's parameters and flags), and the model's checks; or why linksem
    cannot say. The pass only matters for the boot-pass callback guard, which none of the
    hypervisor's entries hit, so the system pass is asked for. *)
let words (t : table) (e : entry) :
    ((Z.t * Symbolic_resolution.sym_expr) list * Symbolic_resolution.sym_check list, string) result =
  match RelocatedSections.relocated_list t.elf t.site_sections with
  | Error m -> Error m
  | Ok secs -> (
      match alt_entry_words t.elf Pass_system e secs with
      | Error.Success r -> Ok r
      | Error.Fail m -> Error m
    )
