open Cmdliner
open Config.CommonOpt

module Default = struct
  (** Default action to run when no command is set *)
  let action _arch = print_endline "Error: read-dwarf, no command specified. Use --help for help"

  (** Global documentation string and name *)
  let info =
    let doc = "Parse dwarf information and use isla to run assembly" in
    Term.(info "read-dwarf" ~doc ~exits)

  (** Default command *)
  let command = (Term.(CmdlinerHelper.func_options comopts action $ arch), info)
end

(** List of all non-default commands *)
let commands =
  [
    Run.ReadDwarf.command;
    Isla.Test.command;
    DumpSym.command;
    Isla.Server.Cmd.command;
    Run.BB.command;
    DumpDwarf.command;
    Cache.Cmd.command;
    Run.Func.command;
    Run.Instr.command;
    Run.Block.command;
    Run.FuncRD.command;
    CopySourcesCmd.command;
  ]

let _ = Printexc.record_backtrace Config.enable_backtrace

(* Other architecture are unsupported for now. Remove this only when you are explicitly
   working on make other architecture work with read-dwarf *)
let _ = assert ("aarch64" = Arch.module_name)

(* TODO allow to set the seed in compile time or run time config for debugging *)
let _ = Random.self_init ()

(** Main entry point *)
let _ = Term.exit @@ Term.eval_choice Default.command commands
