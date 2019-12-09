
module Warn = struct
  let fatal fmt s = Printf.printf fmt s; flush stdout; exit 1
end

module Debug = struct
  let debug = ref true (*false*)
  let print_string s = if !debug then Printf.printf "%s" s; flush stdout
  let print_string2 s = if !debug then Printf.printf "%s" s; flush stdout
end
            
let process_file filename = 
  (* call ELF analyser on file *)
  let info = (Sail_interface.populate_and_obtain_global_symbol_init_info filename) in 
  
  let
    ((elf_file: Elf_file.elf_file),
     (elf_epi: Sail_interface.executable_process_image),
     (symbol_map: Elf_file.global_symbol_init_info))
    =
    begin match info with
    | Error.Fail s -> Warn.fatal "populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
    end
  in
  
  (* Printf.printf "%s\n" (Sail_interface.string_of_executable_process_image elf_epi);*)

  Debug.print_string "elf segments etc\n";
  let (segments, e_entry, e_machine, (dso: Dwarf.dwarf_static option)) =
    begin match elf_epi, elf_file with
    | (Sail_interface.ELF_Class_32 _, _)  -> Warn.fatal "%s" "cannot handle ELF_Class_32"
    | (_, Elf_file.ELF_File_32 _)  -> Warn.fatal "%s" "cannot handle ELF_File_32"
    | (Sail_interface.ELF_Class_64 (segments,e_entry,e_machine), Elf_file.ELF_File_64 f1) ->
       (* remove all the auto generated segments (they contain only 0s) *)
       let segments =
         Lem_list.mapMaybe
           (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
           segments
       in
       let dso =
         match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f1) with
         | None -> Warn.fatal "%s" "extract_dwarf_static failed"
         | Some ds ->
            Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf ds.Dwarf.ds_analysed_location_data);
            Debug.print_string2 (Dwarf.pp_evaluated_frame_info ds.Dwarf.ds_evaluated_frame_info);
            Some ds
       in
       (segments,e_entry,e_machine, dso)
    end
  in
  ()
    
      (*
    Debug.print_string "elf test\n";
    let test =
      { symbol_map  = symbol_map @ (symbols_for_stacks !Globals.elf_threads);
        segments    = segments;
        e_entry     = e_entry;
        e_machine   = e_machine;
        elf_threads = !Globals.elf_threads;
        dwarf_static= dso;
      }
       *)




let opts = []

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

  let _ = process_file filename in
  
  Printf.printf "foo\n"



  
let _ = main() 
          
