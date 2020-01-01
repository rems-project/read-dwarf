
open Printf;;

type natural = Nat_big_num.num

let pp_addr (a:natural) = Ml_bindings.hex_string_of_big_int_pad8 a
                        
let parse_addr (s:string) : natural =
  Scanf.sscanf s "%Lx" (fun i64 ->  Nat_big_num.of_int64 i64)

                        
type test =
  {
   elf_file: Elf_file.elf_file;
   symbol_map: Elf_file.global_symbol_init_info;
   segments: Elf_interpreted_segment.elf64_interpreted_segment list;
   e_entry: natural;
   e_machine: natural;
   dwarf_static: Dwarf.dwarf_static;
   dwarf_semi_pp_frame_info:  (natural (*address*) * string (*cfa*) * (string*string) (*register rules*) list) list;
  }

(** ********************************************************************* *)
(** **   pp symbol map                                                 ** *)
(** ********************************************************************* *)

let pp_symbol_map (symbol_map: Elf_file.global_symbol_init_info) =
  String.concat ""
    (List.map
       (fun (name, (typ, size, address, mb, binding)) ->
         Printf.sprintf "**** name = %s  address = %s  typ = %d\n" name  (pp_addr address) (Nat_big_num.to_int typ))
       symbol_map)

  
(** ********************************************************************* *)
(** **   use linksem to parse ELF file and extract DWARF info          ** *)
(** ********************************************************************* *)
  
let parse_file (filename:string) : test = 
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

  
  let f64 = match elf_file with Elf_file.ELF_File_64 f -> f | _ -> raise (Failure "not Elf64") in

(* linksem main_elf --symbols looks ok for gcc and clang

That uses                 Elf_file.read_elf64_file bs0 >>= fun f1 ->
                return (Harness_interface.harness_string_of_elf64_syms
 *)

(*
  let pp_string_table strtab =  match strtab with String_table.Strings(c,s) -> String.map (function c' -> if c'=c then ' ' else c') s in
  
  (*
  (* check the underlying string table - looks right for clang and gcc*)
  let string_table :String_table.string_table =
    match Elf_file.get_elf64_file_symbol_string_table f64 with
    | Error.Success x -> x
    | Error.Fail s -> raise (Failure ("foo "^s))
  in
  Printf.printf "%s\n" (pp_string_table string_table);
  exit 0;
*)

  (* check the symbol table - plausible looking "Name" offsets for both gcc and clang *)

  (match Elf_file.get_elf64_file_symbol_table f64 with
  | Error.Success (symtab,strtab) -> Printf.printf "%s\n%s" (pp_string_table strtab) (Elf_symbol_table.string_of_elf64_symbol_table symtab)
  | Error.Fail s -> raise (Failure "foo"));




  (* check the symbol_map - right number of entries, and strings for gcc, but no strings for clang... *)
  Printf.printf "symbol_map=\n%s"  (pp_symbol_map symbol_map);
  (* Printf.printf "%s\n" (Sail_interface.string_of_executable_process_image elf_epi);*)
(*  exit 0;*)
 *)
  
  (*  Debug.print_string "elf segments etc\n";*)
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
     let ds =
       match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f1) with
       | None -> Warn.fatal "%s" "extract_dwarf_static failed"
       | Some ds ->
(*          Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf ds.Dwarf.ds_analysed_location_data);
          Debug.print_string2 (Dwarf.pp_evaluated_frame_info ds.Dwarf.ds_evaluated_frame_info);*)
          ds
     in
     let dwarf_semi_pp_frame_info = Dwarf.semi_pp_evaluated_frame_info ds.ds_evaluated_frame_info in 
     let test =
       { elf_file    = elf_file;
         symbol_map  = symbol_map (*@ (symbols_for_stacks !Globals.elf_threads)*);
         segments    = segments;
         e_entry     = e_entry;
         e_machine   = e_machine;
         dwarf_static= ds;
         dwarf_semi_pp_frame_info = dwarf_semi_pp_frame_info;
       } in
     test
  end

(** ********************************************************************* *)
(** **          marshal and unmarshal test                             ** *)
(** ********************************************************************* *)

let marshal_to_file filename test =
  let c = open_out filename in
  Marshal.to_channel c test [];
  close_out c

let marshal_from_file filename : test option =
  try
    let c = open_in filename in
    let test = Marshal.from_channel c in
    close_in c;
    Some test
  with
  | Sys_error s -> None
  | e -> raise e
  
  

(** ********************************************************************* *)
(** **          pretty-print source lines for an address               ** *)
(** ********************************************************************* *)

(* 'safe_open_in filename f' will open filename, pass it to f and cloth
 * the channel at the end or when an exception is raised
 *)
let safe_open_in (filename: string) (f: in_channel -> 'a) : 'a =
    let chan = open_in filename in
    let res = try f chan with e -> close_in chan; raise e in
    close_in chan;
    res

type 'a ok_or_fail = Ok of 'a | MyFail of string    

let read_source_file (name: string) : string array ok_or_fail = 
  let read_lines chan =
    let lines = ref [] in
    let () =
      try
        while true do lines := (input_line chan) :: !lines done
      with
      | End_of_file -> ()
    in
    !lines |> List.rev |> Array.of_list
  in
  match safe_open_in name read_lines with
  | lines -> Ok lines
  | exception Sys_error s ->
     MyFail (Printf.sprintf "read_source_file Sys_error \"%s\"\n" s)

    (*
let rec read_source_file2 (names: string list) (err_acc:string) : string array ok_or_fail =
  match names with
  | [] -> MyFail ("not found:\n"^err_acc)
  | name::names' ->
     match read_source_file name with
     | Ok lines -> Ok lines
     | MyFail err -> read_source_file2 names' (err_acc^err)
     *)
    
let source_file_cache = ref ([] : ((string option * string option *string) * (string array option)) list)

let source_line (comp_dir,dir,file) n1 =
  let pp_string_option s = match s with Some s'->s'|None -> "<none>" in
(*
  Printf.printf "comp_dir=\"%s\"  source_line dir=\"%s\"  file=\"%s\"\n" (pp_string_option comp_dir) (pp_string_option dir) file;
 *)
  let access_lines lines n =
    if n < 0 || n >= Array.length lines then
      Some (sprintf "line out of range: %i vs %i" n (Array.length lines))
    else
      Some lines.(n) in

  let n = n1 -1 in
  match (try Some (List.assoc (comp_dir,dir,file) !source_file_cache) with Not_found -> None) with
  | Some (Some lines) -> access_lines lines n
  | Some (None) -> None
  | None ->
     let filename = match (comp_dir,dir,file) with
       | (Some cd, Some d, f) -> Filename.concat cd (Filename.concat d f)
       | (Some cd, None,   f) -> Filename.concat cd f
       | (None,    Some d, f) -> Filename.concat d  f
       | (None,    None,   f) -> f
     in
     match read_source_file filename with
     | Ok lines ->
        source_file_cache := ((comp_dir,dir,file), Some lines) :: !source_file_cache;
        access_lines lines n
     | MyFail s ->
(*        source_file_cache := (file, None) :: !source_file_cache;
          None *)
         source_file_cache := ((comp_dir,dir,file), None) :: !source_file_cache;
         (Warn.nonfatal "%s" s; None)


     (*
     let filenames = match !Globals.dwarf_source_dirs with
     | [] -> [file]
     | _ -> List.map (fun s -> Filename.concat s file) !Globals.dwarf_source_dirs in
     match read_source_file2 filenames "" with
     | Ok lines ->
        source_file_cache := (file, Some lines) :: !source_file_cache;
        access_lines lines n
     | MyFail s ->
(*        source_file_cache := (file, None) :: !source_file_cache;
          None *)
         source_file_cache := (file, None) :: !source_file_cache;
         (Warn.nonfatal "%s" s; None)
 *)
     
let pp_source_line so =
  match so with
  | Some s -> s (*" (" ^ s ^ ")"*)
  | None -> "file not found"

let pp_dwarf_source_file_lines m ds (pp_actual_line: bool) (a: natural) : string option =
  let sls = Dwarf.source_lines_of_address ds a in
  match sls with
  | [] -> None
  | _ ->
     Some
       (String.concat
          ", "
          (List.map
             (fun (comp_dir,dir,file,n,lnr) ->
               let comp_dir' = match !Globals.comp_dir with None -> comp_dir | Some comp_dir'' -> Some comp_dir'' in 
               file ^ ":" ^ Nat_big_num.to_string n ^ if pp_actual_line then pp_source_line (source_line (comp_dir',dir,file) (Nat_big_num.to_int n))  else ""
             )
             sls
          )
       )


(** ********************************************************************* *)
(** **          look up address in ELF symbol table                    ** *)
(** ********************************************************************* *)

let elf_symbols_of_address (test:test) (addr:natural) : string list =
  List.filter_map
    (fun (name, (typ, size, address, mb, binding)) ->
      if address=addr then Some name else None)
    test.symbol_map


(** ********************************************************************* *)
(** **          look up address in frame info                          ** *)
(** ********************************************************************* *)

let aof ((a:natural),(cfa:string),(regs:(string*string) list)) = a
  
let rec f (aof:'b->natural) (a:natural) (last: 'b option) (bs:'b list) : 'b option =
  match (last,bs) with
  | (None,   [])        -> None
  | (Some b',[])        -> if Nat_big_num.greater_equal a (aof b') then Some b' else None
  | (None,    b''::bs') -> f aof a (Some b'') bs'
  | (Some b', b''::bs') ->
     if Nat_big_num.less a (aof b') then None
     else if Nat_big_num.greater_equal a (aof b') && Nat_big_num.less a (aof b'') then Some b'
     else f aof a (Some b'') bs'

  
let pp_frame_info (test:test) (addr:natural) : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match f aof addr None test.dwarf_semi_pp_frame_info with
  | None -> "<no frame info for this address>\n"
  | Some ((a:natural),(cfa:string),(regs:(string*string) list)) -> 
     pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " " ^ String.concat " " (List.map (function (rname,rinfo)->rname^":"^rinfo) regs) ^"\n"


    
(** ********************************************************************* *)
(** **          pull disassembly out of an objdump -d file             ** *)
(** ********************************************************************* *)

let objdump_lines : (natural * (natural * string)) list option ref = ref None

let init_objdump () =     
  match !Globals.objdump_d with
  | None -> ()
  | Some filename ->
     match read_source_file filename with
     | MyFail s ->
        Warn.fatal2 "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename
     | Ok lines ->
        let parse_line (s:string) : (natural*(natural*string)) option =
(*          if String.length s >=9 && s.[8] = ':' then *)
          match Scanf.sscanf s " %x: %x %n" (fun a -> fun i -> fun n -> (a,i,n)) with
          | (a,i,n) ->
              let s' = String.sub s n (String.length s - n) in
              Some (Nat_big_num.of_int a, (Nat_big_num.of_int i, s'))
          | exception _ -> None
(*          else
            None*)
        in
        objdump_lines := Some (List.filter_map parse_line (Array.to_list lines))

let lookup_objdump_lines (a: natural) : (natural*string) option =
  match !objdump_lines with
  | Some lines -> 
     List.assoc_opt a lines
  | None ->
     None

    
(** ********************************************************************* *)
(** **         hacky parse of control-flow instruction asm             ** *)
(** ********************************************************************* *)

(* this ignores indirect branches (br instructions), so isn't good for much*)
    
type control_flow_insn =
  | C_ret_eret
  | C_branch of string (*mnemonic*) * string (*numeric addr*) * string (*symbolic addr*)
  (*  | C_branch_register of string (*mnemonic*) * string (*argument*)*)
  | C_smc_hvc of string

let pp_target t = match t with
  | C_ret_eret -> "ret/eret"
  | C_branch(is,s1,s2) -> is ^ " "^s1^" "^s2
  | C_smc_hvc s -> "smc/hvc "^s

let pp_target_option t = match t with
  | None -> ""
  | Some t -> pp_target t
               
let parse_target s =
  match Scanf.sscanf s "%s %s" (fun s1 -> fun s2 -> (s1,s2)) with
  | (s1,s2) -> Some (""^s1, ""^s2^"")
  | exception _ -> None

let parse_drop_one s =
  match Scanf.sscanf s "%s %n" (fun s1 -> fun n -> let s' = String.sub s n (String.length s - n) in (s1,s')) with
  | (s1,s') -> Some s'
  | exception _  -> None
             
let parse_control_flow_instruction e s = 
  match Scanf.sscanf s "%s %n" (fun mnemonic -> fun n -> let s' = String.sub s n (String.length s - n) in (mnemonic,s')) with
  | exception _ -> None
  | (mnemonic,s') -> 
     if List.mem mnemonic ["ret"; "eret"] then 
       Some C_ret_eret
     else if List.mem mnemonic ["b."; "b "; "bl"] then 
       match parse_target s' with
       | None -> raise (Failure ("b./b/bl parse error for: \""^s'^"\" in "^e^"\n"))
       | Some (s1,s2) -> Some (C_branch (mnemonic,s1,s2))
     else if List.mem mnemonic ["cbz"; "cbnz"] then 
       match parse_drop_one s' with
       | None -> raise (Failure ("cbz/cbnz 1 parse error for: "^e^"\n"))
       | Some s' ->  
          match parse_target s' with
          | None -> raise (Failure ("cbz/cbnz 2 parse error for: "^s' ^ " in "^e^"\n"))
          | Some (s1,s2) -> Some (C_branch (mnemonic,s1,s2))
     else if List.mem mnemonic ["tbz"; "tbnz"] then 
       match parse_drop_one s' with
       | None -> raise (Failure ("tbz/tbnz 1 parse error for: "^e^"\n"))
       | Some s'' ->  
          match parse_drop_one s'' with
          | None -> raise (Failure ("tbz/tbnz 2 parse error for: "^e^"\n"))
          | Some s''' ->  
             match parse_target s''' with
             | None -> raise (Failure ("tbz/tbnz 3 parse error for: "^e^"\n"))
             | Some (s1,s2) -> Some (C_branch (mnemonic,s1,s2))
     else if List.mem mnemonic ["smc"; "hvc"] then 
       Some (C_smc_hvc s')
     else 
       None
    

type node = natural * string
          

let pp_cfg test = 
  init_objdump ();

  (* pull out instructions from text section, assuming 4-byte insns *)
  let (p,addr,bs) = Dwarf.extract_text test.elf_file in
  let instructions : (natural * natural) list = Dwarf.instructions_of_byte_list addr bs [] in

  let is_control_flow_insn  (addr:natural) : (control_flow_insn option) option =  
    begin match lookup_objdump_lines addr with
    | Some (i,s) ->
       (*       if i=i' then*)
       (match Scanf.sscanf s "%s %s" (fun mnemonic -> fun s' -> (mnemonic,s')) with
        | (mnemonic,s') -> 
           if List.mem mnemonic ["b."; "b "; "bl"; "ret"; "tbz"; "tbnz"; "cbz"; "cbnz"; "eret"; "smc"; "hvc"] then
             Some (parse_control_flow_instruction s s)
                                            (*Printf.printf "%s %s\n" (pp_addr addr) (pp_target_option t);*)
           else
             Some (None)
        | exception _ ->
           Some (None)
(*       else
         Warn.fatal2 "instruction mismatch - linksem: %s vs objdump: %s\n"  (pp_addr i) (pp_addr i')
 *)
       )
    | None ->
       None
    end in

  (* hackery to count reachable instructions *)
  (* assumes addresses small enough to fit in int *)
  let touched : int Array.t = Array.make (List.length instructions) 0 in
  let base_address = match instructions with (addr,s)::_ -> addr in 
  let index_of_address (addr:natural) : int = Nat_big_num.to_int (Nat_big_num.div (Nat_big_num.sub addr base_address) (Nat_big_num.of_int 4)) in
  let address_of_index (i:int) : natural = Nat_big_num.mul (Nat_big_num.of_int 4) (Nat_big_num.add base_address (Nat_big_num.of_int i)) in
  let touch (addr:natural) = let i = index_of_address addr in touched.(i) <- touched.(i) +1 in
  let count_touched () = Array.fold_left (function count -> function c -> if c=0 then count else count+1) 0 touched in    (* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a *)
  
  (* all the branch targets reachable by one successful branch from the given address *)
  let rec branch_successors (addr:natural) : (natural*string) list =
    (*    Printf.printf "%s\n" (pp_addr addr);*)
    touch addr;
    let succ_addr = (Nat_big_num.add addr (Nat_big_num.of_int 4)) in
    match is_control_flow_insn addr with
    | None -> []  (* lookup source line failed *)
    | Some None -> branch_successors succ_addr
    | Some (Some C_ret_eret) -> []
    | Some (Some (C_branch (mnemonic,s1,s2))) ->
       if mnemonic = "b" then [(parse_addr s1,s2)] else (*b./bl (assuming the bl can return) / cbz/cbnz/tbz/tbnz*)
         (parse_addr s1,s2)::branch_successors succ_addr
    | Some (Some (C_smc_hvc s)) -> []
  in
    
  let branch_instructions : (natural * natural * control_flow_insn) list =
    List.filter_map
      (fun ((addr:natural),(i:natural)) -> match is_control_flow_insn addr with None->None | Some None -> None| Some (Some t) -> Some (addr,i,t))
      instructions in

  let branch_targets : (natural * string) list =
    List.filter_map (function (a,i,t) ->
        match t with
        | C_ret_eret -> None
        | C_branch(mnemonic,s1,s2) -> Some (parse_addr s1,s2)
        | C_smc_hvc s -> None
      )
      branch_instructions in

(*
  List.iter (function (s1,s2) -> Printf.printf "%s %s\n" s1 s2) branch_targets;
 *)
  
  let elf_symbols : (natural * string) list = 
    let elf_symbol_addresses = 
      List.sort_uniq compare
        (List.map
           (fun (name, (typ, size, address, mb, binding)) -> address)
           test.symbol_map) in
    List.map
      (fun address ->
        let names =
          (List.filter_map
             (fun (name, (typ, size, address', mb, binding)) -> if address'=address && String.length name >=1 && name.[0]<>'$' then Some name else None) test.symbol_map) in
        (address,"elf:"^String.concat "__" names))
      elf_symbol_addresses
  in

  let fake_symbol = (parse_addr "40009eb4", "api.c:1526") in
  let fake_symbols = [fake_symbol] in
  
  let nodes = List.sort_uniq compare (fake_symbols @ branch_targets @ elf_symbols) in (*TODO: merge same-address pairs*)
  let pp_node (addr,s) = "\"a" ^ pp_addr addr^"_"^s^"\"" in

  (*  let nodes_initial = List.filter (function (addr,s)->s="<sync_lower_exception>") nodes in*)
  let nodes_initial = List.filter (function (addr,s)->s="<api_share_memory>") nodes in
  (*  let nodes_initial = fake_symbols in *)

  (*
  let edges = List.flatten (List.map (function (addr,s) -> (List.map (function (addr',s')-> ((addr,s),(addr',s'))) (List.sort_uniq compare (branch_successors addr)))) nodes) in
   *)

  let edges : (node,node list) Hashtbl.t = Hashtbl.create 1000 in 
  
  let rec stupid_reachability (max_depth:int) (acc_reachable:node list) (todo:node list) : node list =
    if max_depth = 0 then acc_reachable else
    match todo with
    | [] -> acc_reachable
    | ((addr,s) as n)::todo' ->
       if List.mem n acc_reachable then
         stupid_reachability (max_depth-1) acc_reachable todo'
       else
         let new_todo = List.sort_uniq compare (branch_successors addr) in
         Hashtbl.add edges n new_todo;
         stupid_reachability (max_depth-1) (n::acc_reachable) (new_todo @ todo')
  in
  let reachable = stupid_reachability 3 [] nodes_initial in
           
  let pp_edge (n1,n2) = pp_node n1 ^ " -> " ^ pp_node n2 ^ ";\n" in

  Printf.printf "touched = %d of %d instructions\n" (count_touched()) (Array.length touched);
  let c = open_out "foo.dot" in 
  Printf.fprintf c "digraph g {\n";
  Printf.fprintf c "rankdir=\"LR\";\n";
  (List.iter (function n -> Printf.fprintf c "%s [label=\"\"];\n" (pp_node n)) reachable);
  List.iter (function n ->
               let ns' = Hashtbl.find edges n in
               List.iter (function n' -> 
                            Printf.fprintf c "%s" (pp_edge (n,n'))) ns') reachable;
  Printf.fprintf c "}\n";
  let _ = close_out c in
  
  ()

 


(** ********************************************************************* *)
(** **          pretty-print the result                                ** *)
(** ********************************************************************* *)

  
    
let pp_test test = 
  init_objdump ();

  (* pull out instructions from text section, assuming 4-byte insns *)
  let (p,addr,bs) = Dwarf.extract_text test.elf_file in
  let instructions : (natural * natural) list = Dwarf.instructions_of_byte_list addr bs [] in


  
  let last_frame_info = ref "" in
  let last_var_info = ref "" in
  let last_source_info = ref "" in
  
  let pp_instruction ((addr:natural),(i:natural)) =

    (* the elf symbols at this address, if any *)
    String.concat ""
      (List.map
         (fun (s:string) ->
           pp_addr addr
           ^ " <" ^ s ^">:\n")
         (elf_symbols_of_address test addr))
    (* the source file lines (if any) associated to this address *)
    ^ (if !Globals.show_source then (let source_info = begin match pp_dwarf_source_file_lines () test.dwarf_static true addr with Some s -> s^"\n" | None -> "" end in if source_info = !last_source_info then ""(*"unchanged\n"*) else (last_source_info:=source_info;source_info)) else "")
    
    (* the address and (hex) instruction *)
    ^ pp_addr addr ^ ":  " ^ pp_addr i 
    (* the dissassembly from objdump, if it exists *)
    ^ "  " ^ begin match lookup_objdump_lines addr with
      | Some (i',s) ->
         if i=i' then 
           s
         else
           Warn.fatal2 "instruction mismatch - linksem: %s vs objdump: %s\n"  (pp_addr i) (pp_addr i')
      | None ->
         ""
      end
    ^ "\n"
    (* the frame info for this address *)
    ^ (if !Globals.show_cfa then (let frame_info = pp_frame_info test addr in if frame_info = !last_frame_info then ""(*"CFA: unchanged\n"*) else (last_frame_info:=frame_info;frame_info)) else "")
    (* the variables whose location ranges include this address *)
    ^ (if !Globals.show_vars then
         let var_info = 
           let als (*fald*) = Dwarf.filtered_analysed_location_data test.dwarf_static addr in
           Dwarf.pp_analysed_location_data test.dwarf_static.ds_dwarf als in
         if var_info = !last_var_info then ""(*"vars: unchanged\n"*) else (last_var_info:=var_info;var_info)
       else
         "")
        (*    ^ "\n"*)
  in

  String.concat "" (List.map pp_instruction instructions)




                      
  



(** ********************************************************************* *)
(** **          top-level                                              ** *)
(** ********************************************************************* *)
    

let process_file (filename:string) : unit =
  (* try caching linksem output - though linksem only takes 5s, so scarcely worth the possible confusion. It's recomputing the variable info that takes the time *)
  (*
  let filename_marshalled = filename ^ ".linksem-marshalled" in
  let test =
    match marshal_from_file filename_marshalled with
    | None ->
       let test = parse_file filename in
       marshal_to_file filename_marshalled test;
       test
    | Some test ->
       test
  in              
   *)
  let test = parse_file filename in
  printf "%s" (pp_test test)
  
  (*  pp_cfg test*)
  (*printf "%s" (pp_test test)*)
    
    
