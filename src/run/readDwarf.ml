(** This module implements the [rd] subcommand. This was to original meaning of "read-dwarf",
    and the first command.

    Together with {{!Run.FuncRD}[run-func-rd]}, this is the main user of the {!Analyse} code. *)

open Cmdliner
open CmdlinerHelper
open Config.CommonOpt

let dry_run =
  let doc = "dry run: " in
  setter Analyse.Globals.copy_sources_dry_run
    Arg.(value & flag & info ["dry-run"] ~docv:"DRY_RUN" ~doc)

let skylight =
  let doc = "skylight" in
  setter Analyse.Globals.skylight Arg.(value & flag & info ["skylight"] ~docv:"SKYLIGHT" ~doc)

let comp_dir =
  let doc = "Path to root directory of source files, overriding DWARF comp_dir" in
  setter Analyse.Globals.comp_dir
    Arg.(value & opt (some string) None & info ["comp-dir"] ~docv:"COMP_DIR" ~doc)

let clip_binary =
  let doc = "clip binary to first 1000 instructions" in
  setter Analyse.Globals.clip_binary Arg.(value & flag & info ["clip-binary"] ~doc)

let no_vars =
  let doc = "Do not print variable information at each instruction" in
  setter Analyse.Globals.show_vars Term.(const not $ Arg.(value & flag & info ["no-vars"] ~doc))

let no_cfa =
  let doc = "Do not print CFA information" in
  setter Analyse.Globals.show_cfa Term.(const not $ Arg.(value & flag & info ["no-cfa"] ~doc))

let no_source =
  let doc = "Do not print source lines at each instruction" in
  setter Analyse.Globals.show_source
    Term.(const not $ Arg.(value & flag & info ["no-source"] ~doc))

let objdump_d =
  let doc = "File containing result of objdump -d" in
  setter Analyse.Globals.objdump_d
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables =
  let doc = "File containing branch table base addresses and sizes" in
  setter Analyse.Globals.branch_table_data_file
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf =
  let doc = "ELF file whose dwarf is to be dumped" in
  setter Analyse.Globals.elf
    Arg.(value & opt (some non_dir_file) None & info ["elf"] ~docv:"ELF_FILE" ~doc)

let objdump_d2 =
  let doc = "Second: File containing result of objdump -d" in
  setter Analyse.Globals.objdump_d2
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d2"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables2 =
  let doc = "Second: File containing branch table base addresses and sizes" in
  setter Analyse.Globals.branch_table_data_file2
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables2"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf2 =
  let doc = "Second: ELF file whose dwarf is to be dumped" in
  setter Analyse.Globals.elf2
    Arg.(value & opt (some non_dir_file) None & info ["elf2"] ~docv:"ELF_FILE" ~doc)

let qemu_log =
  let doc = "QEMU log file" in
  setter Analyse.Globals.qemu_log
    Arg.(value & opt (some non_dir_file) None & info ["qemu-log"] ~docv:"QEMU_LOG_FILE" ~doc)

let out_file =
  let doc = "file for single-file output (optional)" in
  setter Analyse.Globals.out_file
    Arg.(value & opt (some string) None & info ["o"; "out"] ~docv:"OUT_FILE" ~doc)

let out_dir =
  let doc = "directory for multiple-file output (optional)" in
  setter Analyse.Globals.out_dir
    Arg.(value & opt (some string) None & info ["out-dir"] ~docv:"OUT_DIR" ~doc)

let cfg_dot_file =
  let doc = "File to output CFG dot to" in
  setter Analyse.Globals.cfg_dot_file
    Arg.(value & opt (some string) None & info ["cfg-dot-file"] ~docv:"CFG_DOT_FILE" ~doc)

let cfg_source_nodes =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Analyse.Globals.cfg_source_nodes
    Arg.(value & opt (some string) None & info ["cfg-source-nodes"] ~docv:"CFG_SOURCE_NODES" ~doc)

let cfg_source_nodes2 =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Analyse.Globals.cfg_source_nodes2
    Arg.(
      value & opt (some string) None & info ["cfg-source-nodes2"] ~docv:"CFG_SOURCE_NODES2" ~doc)

let html =
  let doc = "Enables html output" in
  setter Analyse.Globals.ppmode
    Term.(
      const (fun b -> if b then Analyse.Types.Html else Analyse.Types.Ascii)
      $ Arg.(value & flag & info ["h"; "html"] ~doc))

let options =
  [
    skylight;
    dry_run;
    comp_dir;
    no_vars;
    no_cfa;
    no_source;
    elf;
    objdump_d;
    branch_tables;
    elf2;
    objdump_d2;
    branch_tables2;
    qemu_log;
    clip_binary;
    out_file;
    out_dir;
    cfg_dot_file;
    cfg_source_nodes;
    cfg_source_nodes2;
    html;
    Logs.term;
  ]

let full_term = Term.(func_options options Analyse.process_file $ const ())

let info =
  let doc =
    "Dumps an ELF file in the read-dwarf format. DWARF information is interleaved with the \
     result of the objdump, so one can see how the dwarf information is positioned compared to \
     the assembly. It will also try to read the source file to interleave the original source \
     code."
  in
  Term.(info "rd" ~doc ~exits)

let command = (full_term, info)