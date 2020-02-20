open Cmdliner
open CommonOpt

let comp_dir =
  let doc = "Path to root directory of compilation, to override DWARF comp_dir if need be" in
  setter Globals.comp_dir
    Arg.(value & opt (some string) None & info ["comp_dir"] ~docv:"COMP_DIR" ~doc)

let clip_binary =
  let doc = "clip binary to first 1000 instructions" in
  setter Globals.clip_binary Arg.(value & flag & info ["clip-binary"] ~doc)

let no_vars =
  let doc = "Do not print variable information at each instruction" in
  setter Globals.show_vars Term.(const not $ Arg.(value & flag & info ["no-vars"] ~doc))

let no_cfa =
  let doc = "Do not print CFA information" in
  setter Globals.show_cfa Term.(const not $ Arg.(value & flag & info ["no-cfa"] ~doc))

let no_source =
  let doc = "Do not print source lines at each instruction" in
  setter Globals.show_source Term.(const not $ Arg.(value & flag & info ["no-source"] ~doc))

let objdump_d =
  let doc = "File containing result of objdump -d" in
  setter Globals.objdump_d
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables =
  let doc = "File containing branch table base addresses and sizes" in
  setter Globals.branch_table_data_file
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf =
  let doc = "ELF file whose dwarf is to be dumped" in
  setter Globals.elf
    Arg.(value & opt (some non_dir_file) None & info ["elf"] ~docv:"ELF_FILE" ~doc)

let objdump_d2 =
  let doc = "Second: File containing result of objdump -d" in
  setter Globals.objdump_d2
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d2"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables2 =
  let doc = "Second: File containing branch table base addresses and sizes" in
  setter Globals.branch_table_data_file2
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables2"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf2 =
  let doc = "Second: ELF file whose dwarf is to be dumped" in
  setter Globals.elf2
    Arg.(value & opt (some non_dir_file) None & info ["elf2"] ~docv:"ELF_FILE" ~doc)

(* TODO: cmdliner seems to check that the file exists(?), which we don't want here. Or are we just not opening it properly?*)
let out_file =
  let doc = "file for output (optional)" in
  setter Globals.out_file
    Arg.(value & opt (some non_dir_file) None & info ["out"] ~docv:"OUT_FILE" ~doc)

let info =
  let doc = "Read and dump dwarf information" in
  Term.(info "rd" ~doc ~exits)

let cfg_dot_file =
  let doc = "File to output CFG dot to" in
  setter Globals.cfg_dot_file
    Arg.(value & opt (some string) None & info ["cfg_dot_file"] ~docv:"CFG_DOT_FILE" ~doc)

let cfg_source_nodes =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Globals.cfg_source_nodes
    Arg.(value & opt (some string) None & info ["cfg_source_nodes"] ~docv:"CFG_SOURCE_NODES" ~doc)

let cfg_source_nodes2 =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Globals.cfg_source_nodes2
    Arg.(value & opt (some string) None & info ["cfg_source_nodes2"] ~docv:"CFG_SOURCE_NODES2" ~doc)

  
let options =
  [
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
    clip_binary;
    out_file;
    cfg_dot_file;
    cfg_source_nodes;
    cfg_source_nodes2;
  ]

let full_term = Term.(func_options options Analyse.process_file $ const ())

let command = (full_term, info)
