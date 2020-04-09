open Cmdliner
open CommonOpt

module Default = struct
  (** Default action to run when no command is set *)
  let action arch = print_endline "Error: read-dwarf, no command specified. Use --help for help"

  (** Global documentation string and name *)
  let info =
    let doc = "Parse dwarf information and use isla to run assembly" in
    Term.(info "read-dwarf" ~doc ~exits)

  (** Default command *)
  let command = (Term.(func_options comopts action $ arch), info)
end

(** List of all non-default commands *)
let pcommands =
  [
    ReadDwarf.command;
    IslaTest.command;
    DumpSym.command;
    IslaServer.Cmd.command;
    RunBB.command;
    DumpDwarf.command;
    Cache.Cmd.command;
    RunFunc.command;
  ]

(** Add the test command if tests are enabled *)
let commands = if Tests.enable_tests then Tests.command :: pcommands else pcommands

let _ = Printexc.record_backtrace Config.enable_backtrace

let _ = assert ("aarch64" = Arch.module_name)

(** main *)

let _ = Term.exit @@ Term.eval_choice Default.command commands
