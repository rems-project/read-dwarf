



let opts = [
  ("-comp_dir",
    Arg.String (fun s -> Globals.comp_dir := Some s),
    Printf.sprintf "<string> path to root directory of compilation, to override DWARF comp_dir if need be");

  (*
  ("-I",
    Arg.String (fun s -> Globals.dwarf_source_dirs := (!Globals.dwarf_source_dirs) @ [Filename.concat !Globals.dwarf_source_dir_root s] ),
    Printf.sprintf "<string> add include dir, relative to dwarf_source_path");
   *)
  
  ("-objdump-d",
    Arg.String (fun s -> Globals.objdump_d := Some s),
    Printf.sprintf "<string> file containing result of objdump -d, used for disassembly");
  ]

let main = fun () ->
  let usage = "Usage: main [options]* filename (ELF file)\n" ^
              "       main -help   to show options"
  in

  let filenames = ref [] in
  let collect_file s = filenames := s :: !filenames in
  let usage' = usage ^ "\n" (*^ "Options:"*) in
  let help outchan msg =
    Printf.fprintf outchan "%s\n\n" msg;
  in

  begin try Arg.parse_argv Sys.argv (Arg.align opts) collect_file usage' with
  | Arg.Bad msg  -> help stderr msg; exit 1
  | Arg.Help msg -> help stdout msg; exit 0
  end; 

  let filename =
    match !filenames with
    | [filename] -> filename
    | _ -> (help stderr "must specify exactly one elf file"; exit 1)
  in

  let _ = Analyse.process_file filename in
  
  ()



  
let _ = main() 
          
