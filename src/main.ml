



let opts = [
  ("-dwarf_source_path",
    Arg.String (fun s -> Globals.dwarf_source_dir := s),
    Printf.sprintf "<string> path to directory containing source files, for DWARF debug output (%s)" !Globals.dwarf_source_dir);
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
          
