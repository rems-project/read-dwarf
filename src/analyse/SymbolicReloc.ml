(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Resolving linksem's symbolic relocation values.

    For a relocatable file linksem presents each relocation's value as a
    symbolic expression over [Section] and [Const] terms; for the PC-relative
    forms (S + A - P) used by branches and by PREL32 data this is
    [((Section s + Const v) + Const a) - (Section here + Const p)], with the
    undefined-symbol case represented as [Section ("UND." ^ name)].  This
    module recovers the target [S + A] from such an expression, and gives
    per-section access to a relocatable file's relocations. *)

open Utils

open Logs.Logger (struct
  let str = __MODULE__
end)

(* Claude: the resolved value of a relocation of the form S + A - P *)
type pc_relative_target =
  | Target_addr of addr  (** S + A, as section + offset *)
  | Target_undef of string * int  (** undefined symbol name, and the addend *)

exception Unsupported_expression

(* Claude: flatten a sum/difference of Section and Const terms into
   (positive sections, negative sections, constant); anything else is
   unsupported *)
let rec collect sign (e : Elf_symbolic.symbolic_expression) ((pos, neg, c) as acc) =
  match e with
  | Elf_symbolic.Section s -> if sign then (s :: pos, neg, c) else (pos, s :: neg, c)
  | Elf_symbolic.Const x ->
      let x = Z.to_int x in
      (pos, neg, if sign then c + x else c - x)
  | Elf_symbolic.BinOp (a, Elf_symbolic.Add, b) -> collect sign b (collect sign a acc)
  | Elf_symbolic.BinOp (a, Elf_symbolic.Sub, b) -> collect (not sign) b (collect sign a acc)
  | Elf_symbolic.BinOp (_, Elf_symbolic.And, _) | Elf_symbolic.UnOp _ -> raise Unsupported_expression

(* Claude: linksem names an undefined symbol's "section" thus *)
let undef_prefix = "UND."

let is_undef_section s = String.starts_with ~prefix:undef_prefix s

let undef_symbol_name s = String.sub s (String.length undef_prefix) (String.length s - String.length undef_prefix)

(** [resolve_pc_relative ~here ~p e]: the target [S + A] of a relocation with
    value [e] of the form [S + A - P], at offset [p] of section [here].
    Expects exactly one positive section term (the symbol's section, or
    "UND.name" for an undefined symbol) and [here] as the only negative one. *)
let resolve_pc_relative ~here ~p (e : Elf_symbolic.symbolic_expression) : (pc_relative_target, string) result =
  let unsupported () = Error ("unsupported relocation expression " ^ Elf_symbolic.pp_sym_expr e) in
  match collect true e ([], [], 0) with
  | exception Unsupported_expression -> unsupported ()
  | ([s], [h], c) when h = here ->
      let value = c + p in
      if is_undef_section s then Ok (Target_undef (undef_symbol_name s, value))
      else Ok (Target_addr (Sym_ocaml.Num.Offset (s, Z.of_int value)))
  | _ -> unsupported ()

(** The address used for a branch to an undefined symbol: linksem's
    convention, so that it is recognisably not a real address *)
let external_address (name : string) (addend : int) : addr =
  Sym_ocaml.Num.Offset (undef_prefix ^ name, Z.of_int addend)

let external_symbol_of_address (a : addr) : string option =
  match a with
  | Sym_ocaml.Num.Offset (s, _) when is_undef_section s -> Some (undef_symbol_name s)
  | _ -> None

type relocation = Abi_aarch64_symbolic_relocation.aarch64_relocation_target Elf_symbolic.universal_relocation

(** The relocations of a section of a relocatable ELF64 file, by section
    offset; [None] for a linked file.  Cached per section.  Fatal if linksem
    cannot interpret them, as the symbol loading would already have been. *)
let section_relocations_cache : (string, (int, relocation) Hashtbl.t option) Hashtbl.t = Hashtbl.create 8

let section_relocations (f64 : Elf_file.elf64_file) (section_name : string) : (int, relocation) Hashtbl.t option =
  match Hashtbl.find_opt section_relocations_cache section_name with
  | Some r -> r
  | None ->
      let r =
        if not (Elf_header.is_elf64_relocatable_file f64.elf64_file_header) then None
        else
          match
            Elf_symbolic.extract_elf64_relocations_for_section f64
              Abi_aarch64_symbolic_relocation.aarch64_relocation_interpreter section_name
          with
          | Error.Fail s -> fatal "cannot read relocations for section %s: %s" section_name s
          | Error.Success m ->
              let tbl = Hashtbl.create 1024 in
              List.iter (fun (k, rel) -> Hashtbl.replace tbl (Z.to_int k) rel) (Pmap.bindings_list m);
              Some tbl
      in
      Hashtbl.replace section_relocations_cache section_name r;
      r
