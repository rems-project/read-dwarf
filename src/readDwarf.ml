open Cmdliner

let setter reference term =
  let set r t = r := t in
  Term.(const (set reference) $ term)

let add_option opt term =
  let g a () = a in
  Term.(const g $ term $ opt)

let add_options olist term = List.fold_left (Fun.flip add_option) term olist

let comp_dir =
  let doc = "Path to root directory of compilation, to override DWARF comp_dir if need be" in
  setter Globals.comp_dir
    Arg.(value & opt (some string) None & info ["comp_dir"] ~docv:"COMP_DIR" ~doc)

let clip_binary =
  let doc = "clip binary to first 1000 instructions" in
  setter Globals.clip_binary Term.(Arg.(value & flag & info ["clip-binary"] ~doc))

let no_vars =
  let doc = "Do not print variable information at each instruction" in
  setter Globals.show_vars Term.(const not $ Arg.(value & flag & info ["no-vars"] ~doc))

let no_cfa =
  let doc = "Do not print CFA information" in
  setter Globals.show_cfa Term.(const not $ Arg.(value & flag & info ["no-cfa"] ~doc))

let no_source =
  let doc = "Do not print source lines at each instruction" in
  setter Globals.show_source Term.(const not $ Arg.(value & flag & info ["no-source"] ~doc))

let objdump =
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

(*
  ("-branch-tables",
     Arg.String (fun s -> Globals.branch_table_data_file := Some s),
     Printf.sprintf "<string> file containing branch table base addresses and sizes");
     *)

let elf =
  let doc = "ELF file whose dwarf is to be dumped" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let info =
  let doc = "Read and dump dwarf information" in
  Term.(info "rd" ~doc ~exits:default_exits)

let dot_file =
  let doc = "File to output dot CFG to" in
  setter Globals.dot_file
    Arg.(value & opt (some string) None & info ["dot_file"] ~docv:"DOT_FILE" ~doc)

  
let options = [comp_dir; no_vars; no_cfa; no_source; objdump; branch_tables; clip_binary; dot_file]

(* let full_term = add_option [comp_dir; show_vars; show_cfa; show_source; objdump] main_term *)
let full_term = Term.(add_options options (const Analyse.process_file) $ elf)

let command = (full_term, info)
