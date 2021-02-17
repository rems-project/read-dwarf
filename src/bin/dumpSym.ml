(** This module adds a command to dump the symbol of an ELF file with their content *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Cmdliner
open CmdlinerHelper

let dump_symbols file sym =
  let elf = Elf.File.of_file file in
  base "Elf file with entry 0x%x on architecture %s" elf.entry
    (Elf.File.machine_to_string elf.machine);
  match sym with
  | None -> base "\n%t" (Pp.top Elf.SymTable.pp_raw elf.symbols)
  | Some sympos ->
      let (sym, _) =
        try Elf.SymTable.of_position_string elf.symbols sympos
        with Not_found -> fail "The position %s could not be found in %s" sympos file
      in
      base "\n%t" (Pp.top Elf.Symbol.pp_raw sym)

let elf =
  let doc = "ELF file whose symbols are to be dumped" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let sym =
  let doc = "If specified, the symbols to dump" in
  Arg.(value & opt (some string) None & info ["sym"] ~doc)

let info =
  let doc = "Dump the elf symbol with their size and content" in
  Term.(info "dump-sym" ~doc ~exits:default_exits)

let term = Term.(func_option Logs.term dump_symbols $ elf $ sym)

let command = (term, info)
