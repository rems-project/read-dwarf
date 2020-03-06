(** This module adds a command to dump the symbol of an ELF file with their content *)

open Cmdliner
open CommonOpt

let dump_symbols file =
  let elf = ElfFile.of_file file in
  Printf.printf "Elf file with entry 0x%x on architecture %s\n" elf.entry
    (Elf.File.machine_to_string elf.machine);
  PP.println (ElfFile.SymTbl.pp_raw elf.symbols);
  ()

let elf =
  let doc = "ELF file whose symbols are to be dumped" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let info =
  let doc = "Dump the elf symbol with their size and content" in
  Term.(info "dump-sym" ~doc ~exits:default_exits)

let term = Term.(func_option logs_term dump_symbols $ elf)

let command = (term, info)
