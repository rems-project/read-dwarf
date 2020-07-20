(*****************************************************************************)
(** type of collected ELF-file data from linksem                             *)

(*****************************************************************************)

open AnalyseUtils

(* TODO: this should include the simple_die_tree representation, and the variable info should be calculated in terms of that instead of directly *)

(** architectures from linksem elf_header.lem *)
type architecture = AArch64 (* ARM 64-bit architecture (AARCH64), elf_ma_aarch64 = 183*) | X86

(** AMD x86-64 architecture,  elf_ma_x86_64 = 62 *)

type test = {
  elf_file : Elf_file.elf_file;
  arch : architecture;
  symbol_map : Elf_file.global_symbol_init_info;
  segments : Elf_interpreted_segment.elf64_interpreted_segment list;
  e_entry : natural;
  e_machine : natural;
  dwarf_static : Dwarf.dwarf_static;
  dwarf_semi_pp_frame_info :
    (natural (*address*) * string (*cfa*) * (string * string) (*register rules*) list) list;
}
