open Cmdliner;;


module Default = struct
  (** Default action to run when no command is set *)
  let action () = print_endline "Error: read-dwarf, no command specified. Use --help for help"

  (** Global documentation string and name *)
  let info =
    let doc = "Parse dwarf information and will do other stuff later" in
    Term.(info "read-dwarf" ~doc ~exits:default_exits)

  (** Default command *)
  let command = (Term.(const action $ const ()), info);;
end

(** List of all non-default commands *)
let commands = [ReadDwarf.command]

(** main *)
let _ = Term.exit @@ Term.eval_choice Default.command commands
