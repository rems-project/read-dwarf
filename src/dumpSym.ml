(** This module adds a command to dump the symbol of an ELF file with their content *)

open Cmdliner

let dump_symbols file =
  let elf = ElfFile.of_file file in
  PP.println (ElfFile.SymTbl.pp_raw elf.symbols);
  ()

let elf =
  let doc = "ELF file whose symbols are to be dumped" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let info =
  let doc = "Dump the elf symbol with their size and content" in
  Term.(info "dump-sym" ~doc ~exits:default_exits)

let term = Term.(const dump_symbols $ elf)

let command = (term, info)
