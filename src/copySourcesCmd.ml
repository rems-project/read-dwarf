(** This module is the command-line processing for the [copy-sources] subcommand. *)

open Cmdliner
open CommonOpt

let elf =
  let doc = "ELF file whose dwarf is to be examined" in
  setter AnalyseGlobals.elf
    Arg.(value & opt (some non_dir_file) None & info ["elf"] ~docv:"ELF_FILE" ~doc)

let src_target_dir =
  let doc =
    "Path to target directory to copy sources into (e.g. to use later as comp_dir argument)"
  in
  setter AnalyseGlobals.src_target_dir
    Arg.(value & opt (some string) None & info ["src_target_dir"] ~docv:"SRC_TARGET_DIR" ~doc)

let dry_run =
  let doc = "dry run: " in
  setter AnalyseGlobals.copy_sources_dry_run
    Arg.(value & flag & info ["dry_run"] ~docv:"DRY_RUN" ~doc)

let options = [elf; src_target_dir; dry_run; CommonOpt.logs_term]

let full_term = Term.(func_options options CopySources.process_file $ const ())

let info =
  let doc =
    "Copies the source files mentioned in the DWARF information of an ELF file, to the \
     src_target_dir directory.  This can then be used, for example, as the argument to comp_dir, \
     to run read-dwarf without the original build tree."
  in
  Term.(info "copy-sources" ~doc ~exits)

let command = (full_term, info)
