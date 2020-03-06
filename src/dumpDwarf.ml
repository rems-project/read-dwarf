(** This module adds a command to dump the interpreted DWARF information of an ELF file.
    The DWARF information is dumped as interpreted by read-dwarf,

    See {!ReadDwarf} for a different dump/interpretation.
*)

open Cmdliner
open CommonOpt

let dump_dwarf file =
  let dw = Dw.of_file file in
  PP.(println $ Dw.pp_raw dw)

let elf =
  let doc = "ELF file whose dwarf is to be dumped" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let info =
  let doc =
    "Dump the dwarf information as interpreted by read-dwarf, "
    ^ "contrary to the rd subcommand that dump it directly from the output of Linksem"
  in
  Term.(info "dump-dwarf" ~doc ~exits:default_exits)

let term = Term.(func_option logs_term dump_dwarf $ elf)

let command = (term, info)
