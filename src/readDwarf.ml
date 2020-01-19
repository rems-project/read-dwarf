
open Cmdliner;;

let setter reference term =
  let set r t = print_endline "setting"; r := t in
  Term.(const (set reference) $ term)

let add_option opt term =
  let g a () = print_endline "g"; a in
  Term.(const g $ term $ opt)

let add_options olist term =
  List.fold_left (Fun.flip add_option) term olist

let comp_dir =
  let doc = "Path to root directory of compilation, to override DWARF comp_dir if need be" in
  setter Globals.comp_dir
    Arg.(value & opt (some string) None & info ["comp_dir"] ~docv:"COMP_DIR" ~doc)

let show_vars =
  let doc = "Show variable information at each instruction" in
  setter Globals.show_vars
    Arg.(value & flag & info ["show_vars"] ~doc)

let show_cfa =
  let doc = "Show CFA information at each instruction" in
  setter Globals.show_cfa
    Arg.(value & flag & info ["show_cfa"] ~doc)

let show_source =
  let doc = "Show source lines at each instruction" in
  setter Globals.show_source
    Arg.(value & flag & info ["show_source"] ~doc)

let objdump =
  let doc = "File containing result of objdump -d" in
  setter Globals.objdump_d
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables =
  let doc = "File containing branch table base addresses and sizes" in
  setter Globals.branch_table_data_file
    Arg.(value & opt (some non_dir_file) None & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

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

let options = [comp_dir; show_vars; show_cfa; show_source; objdump]
(* let full_term = add_option [comp_dir; show_vars; show_cfa; show_source; objdump] main_term *)
let full_term = Term.((add_option comp_dir (add_option objdump (add_option branch_tables (const Analyse.process_file)))) $ elf)

let command = (full_term, info)
