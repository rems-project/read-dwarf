open Printf

type natural = Nat_big_num.num

let pp_addr (a : natural) = Ml_bindings.hex_string_of_big_int_pad8 a

type test = {
  elf_file : Elf_file.elf_file;
  symbol_map : Elf_file.global_symbol_init_info;
  segments : Elf_interpreted_segment.elf64_interpreted_segment list;
  e_entry : natural;
  e_machine : natural;
  dwarf_static : Dwarf.dwarf_static;
  dwarf_semi_pp_frame_info :
    (natural (*address*) * string (*cfa*) * (string * string) (*register rules*) list) list;
}

(*****************************************************************************)
(*        pp symbol map                                                      *)
(*****************************************************************************)

let pp_symbol_map (symbol_map : Elf_file.global_symbol_init_info) =
  String.concat ""
    (List.map
       (fun (name, (typ, size, address, mb, binding)) ->
         Printf.sprintf "**** name = %s  address = %s  typ = %d\n" name (pp_addr address)
           (Nat_big_num.to_int typ))
       symbol_map)

(*****************************************************************************)
(*        use linksem to parse ELF file and extract DWARF info               *)
(*****************************************************************************)

let parse_file (filename : string) : test =
  (* call ELF analyser on file *)
  let info = Sail_interface.populate_and_obtain_global_symbol_init_info filename in

  let ( (elf_file : Elf_file.elf_file),
        (elf_epi : Sail_interface.executable_process_image),
        (symbol_map : Elf_file.global_symbol_init_info) ) =
    match info with
    | Error.Fail s -> Warn.fatal "populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
  in

  let f64 =
    match elf_file with Elf_file.ELF_File_64 f -> f | _ -> raise (Failure "not Elf64")
  in

  (* linksem main_elf --symbols looks ok for gcc and clang

     That uses                 Elf_file.read_elf64_file bs0 >>= fun f1 ->
                return (Harness_interface.harness_string_of_elf64_syms
  *)

  (*
  let pp_string_table strtab =
    match strtab with String_table.Strings(c,s) ->
      String.map (function c' -> if c'=c then ' ' else c') s
  in

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
  | Error.Success (symtab,strtab) ->
       Printf.printf "%s\n%s" (pp_string_table strtab)
         (Elf_symbol_table.string_of_elf64_symbol_table symtab)
  | Error.Fail s -> raise (Failure "foo"));




  (* check the symbol_map - right number of entries, and strings for gcc,
     but no strings for clang... *)
  Printf.printf "symbol_map=\n%s"  (pp_symbol_map symbol_map);
  (* Printf.printf "%s\n" (Sail_interface.string_of_executable_process_image elf_epi);*)
(*  exit 0;*)
 *)

  (*  Debug.print_string "elf segments etc\n";*)
  match (elf_epi, elf_file) with
  | Sail_interface.ELF_Class_32 _, _ -> Warn.fatal "%s" "cannot handle ELF_Class_32"
  | _, Elf_file.ELF_File_32 _ -> Warn.fatal "%s" "cannot handle ELF_File_32"
  | Sail_interface.ELF_Class_64 (segments, e_entry, e_machine), Elf_file.ELF_File_64 f1 ->
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
            (* Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf
                                    ds.Dwarf.ds_analysed_location_data);
             Debug.print_string2 (Dwarf.pp_evaluated_frame_info
                                    ds.Dwarf.ds_evaluated_frame_info);*)
            ds
      in
      let dwarf_semi_pp_frame_info =
        Dwarf.semi_pp_evaluated_frame_info ds.ds_evaluated_frame_info
      in
      let test =
        {
          elf_file;
          symbol_map (*@ (symbols_for_stacks !Globals.elf_threads)*);
          segments;
          e_entry;
          e_machine;
          dwarf_static = ds;
          dwarf_semi_pp_frame_info;
        }
      in
      test

(*****************************************************************************)
(*        marshal and unmarshal test                                         *)
(*****************************************************************************)

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

(*****************************************************************************)
(*        pretty-print source lines for an address                           *)
(*****************************************************************************)

(** 'safe_open_in filename f' will open filename, pass it to f and cloth
 * the channel at the end or when an exception is raised
 * TODO use Protect.protect
 *)
let safe_open_in (filename : string) (f : in_channel -> 'a) : 'a =
  let chan = open_in filename in
  let res =
    try f chan
    with e ->
      close_in chan;
      raise e
  in
  close_in chan;
  res

type 'a ok_or_fail = Ok of 'a | MyFail of string

let read_source_file (name : string) : string array ok_or_fail =
  let read_lines chan =
    let lines = ref [] in
    let () =
      try
        while true do
          lines := input_line chan :: !lines
        done
      with End_of_file -> ()
    in
    !lines |> List.rev |> Array.of_list
  in
  match safe_open_in name read_lines with
  | lines -> Ok lines
  | exception Sys_error s -> MyFail (Printf.sprintf "read_source_file Sys_error \"%s\"\n" s)

(*
let rec read_source_file2 (names: string list) (err_acc:string) : string array ok_or_fail =
  match names with
  | [] -> MyFail ("not found:\n"^err_acc)
  | name::names' ->
     match read_source_file name with
     | Ok lines -> Ok lines
     | MyFail err -> read_source_file2 names' (err_acc^err)
     *)

let source_file_cache =
  ref ([] : ((string option * string option * string) * string array option) list)

let source_line (comp_dir, dir, file) n1 =
  let pp_string_option s = match s with Some s' -> s' | None -> "<none>" in
  (* Printf.printf "comp_dir=\"%s\"  source_line dir=\"%s\"  file=\"%s\"\n"
       (pp_string_option comp_dir) (pp_string_option dir) file; *)
  let access_lines lines n =
    if n < 0 || n >= Array.length lines then
      Some (sprintf "line out of range: %i vs %i" n (Array.length lines))
    else Some lines.(n)
  in

  let n = n1 - 1 in
  match
    try Some (List.assoc (comp_dir, dir, file) !source_file_cache) with Not_found -> None
  with
  | Some (Some lines) -> access_lines lines n
  | Some None -> None
  | None -> (
      let filename =
        match (comp_dir, dir, file) with
        | Some cd, Some d, f -> Filename.concat cd (Filename.concat d f)
        | Some cd, None, f -> Filename.concat cd f
        | None, Some d, f -> Filename.concat d f
        | None, None, f -> f
      in
      match read_source_file filename with
      | Ok lines ->
          source_file_cache := ((comp_dir, dir, file), Some lines) :: !source_file_cache;
          access_lines lines n
      | MyFail s ->
          (*        source_file_cache := (file, None) :: !source_file_cache;
                  None *)
          source_file_cache := ((comp_dir, dir, file), None) :: !source_file_cache;
          Warn.nonfatal2 "filename %s %s" filename s;
          None
    )

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

let pp_source_line so = match so with Some s -> s (*" (" ^ s ^ ")"*) | None -> "file not found"

let pp_dwarf_source_file_lines m ds (pp_actual_line : bool) (a : natural) : string option =
  let sls = Dwarf.source_lines_of_address ds a in
  match sls with
  | [] -> None
  | _ ->
      Some
        (String.concat ", "
           (List.map
              (fun ((comp_dir, dir, file), n, lnr) ->
                let comp_dir' =
                  match !Globals.comp_dir with
                  | None -> comp_dir
                  | Some comp_dir'' -> (
                      match comp_dir with
                      | None -> Some comp_dir''
                      | Some s -> Some (Filename.concat comp_dir'' s)
                    )
                in
                file ^ ":" ^ Nat_big_num.to_string n ^ " "
                ^
                if pp_actual_line then
                  pp_source_line (source_line (comp_dir', dir, file) (Nat_big_num.to_int n))
                else "")
              sls))

(*****************************************************************************)
(*        look up address in ELF symbol table                                *)
(*****************************************************************************)

let elf_symbols_of_address (test : test) (addr : natural) : string list =
  List.filter_map
    (fun (name, (typ, size, address, mb, binding)) -> if address = addr then Some name else None)
    test.symbol_map

(*****************************************************************************)
(*        look up address in frame info                                      *)
(*****************************************************************************)

let aof ((a : natural), (cfa : string), (regs : (string * string) list)) = a

let rec f (aof : 'b -> natural) (a : natural) (last : 'b option) (bs : 'b list) : 'b option =
  match (last, bs) with
  | None, [] -> None
  | Some b', [] -> if Nat_big_num.greater_equal a (aof b') then Some b' else None
  | None, b'' :: bs' -> f aof a (Some b'') bs'
  | Some b', b'' :: bs' ->
      if Nat_big_num.less a (aof b') then None
      else if Nat_big_num.greater_equal a (aof b') && Nat_big_num.less a (aof b'') then Some b'
      else f aof a (Some b'') bs'

let pp_frame_info (test : test) (addr : natural) : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match f aof addr None test.dwarf_semi_pp_frame_info with
  | None -> "<no frame info for this address>\n"
  | Some ((a : natural), (cfa : string), (regs : (string * string) list)) ->
      pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " "
      ^ String.concat " " (List.map (function rname, rinfo -> rname ^ ":" ^ rinfo) regs)
      ^ "\n"

(*****************************************************************************)
(*        pull disassembly out of an objdump -d file                         *)
(*****************************************************************************)

let objdump_lines : (natural * (natural * string)) list option ref = ref None

let init_objdump () =
  if !objdump_lines <> None then ()
  else
    match !Globals.objdump_d with
    | None -> ()
    | Some filename -> (
        match read_source_file filename with
        | MyFail s -> Warn.fatal2 "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename
        | Ok lines ->
            let parse_line (s : string) : (natural * (natural * string)) option =
              (*          if String.length s >=9 && s.[8] = ':' then *)
              match Scanf.sscanf s " %x: %x %n" (fun a i n -> (a, i, n)) with
              | a, i, n ->
                  let s' = String.sub s n (String.length s - n) in
                  Some (Nat_big_num.of_int a, (Nat_big_num.of_int i, s'))
              | exception _ -> None
              (*          else
                        None*)
            in
            objdump_lines := Some (List.filter_map parse_line (Array.to_list lines))
      )

let lookup_objdump_lines (a : natural) : (natural * string) option =
  match !objdump_lines with Some lines -> List.assoc_opt a lines | None -> None

(*****************************************************************************)
(*        parse of control-flow instruction asm                              *)
(*****************************************************************************)

type addr = natural

type node = addr * int * (string list)
          
type control_flow_insn =
  | C_ret
  | C_eret
  | C_branch of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_and_link of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_cond of string (*mnemonic*) * addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_register of string (*argument*)
  | C_smc_hvc of string

let pp_control_flow_instruction c =
  match c with
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch (a, s) -> "b" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_and_link (a, s) -> "bl" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_cond (is, a, s) -> is ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_register r -> "br"
  | C_smc_hvc s -> "smc/hvc " ^ s

let pp_control_flow_instruction_short c =
  match c with
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch (a, s) -> "b"
  | C_branch_and_link (a, s) -> "bl"
  | C_branch_cond (is, a, s) -> is
  | C_branch_register r -> "br"
  | C_smc_hvc s -> "smc/hvc"

let highlight c =
  match c with
  | C_ret | C_eret | C_branch_and_link (_, _) | C_smc_hvc _ -> false
  | C_branch (_, _) | C_branch_cond (_, _, _) | C_branch_register _ -> true

(* highlight branch targets to earlier addresses*)
let pp_target_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Nat_big_num.less a addr then "^" else "") ^ pp_addr a

(* highlight branch come-froms from later addresses*)
let pp_come_from_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Nat_big_num.greater a addr then "v" else "") ^ pp_addr a

(* hacky parsing of assembly from objdump -d *)

let parse_addr (s : string) : natural = Scanf.sscanf s "%Lx" (fun i64 -> Nat_big_num.of_int64 i64)

let parse_target s =
  match Scanf.sscanf s "%s %s" (fun s1 s2 -> (s1, s2)) with
  | s1, s2 -> Some (parse_addr s1, s2)
  | exception _ -> None

let parse_drop_one s =
  match
    Scanf.sscanf s "%s %n" (fun s1 n ->
        let s' = String.sub s n (String.length s - n) in
        (s1, s'))
  with
  | s1, s' -> Some s'
  | exception _ -> None

let parse_control_flow_instruction s mnemonic s' =
  (*  match Scanf.sscanf s "%s %n" (fun mnemonic -> fun n -> let s' = String.sub s n (String.length s - n) in (mnemonic,s')) with
        | exception _ -> None
        | (mnemonic,s') ->
         (
    *)
  (*  Printf.printf "s=\"%s\" mnemonic=\"%s\" s'=\"%s\"\n"s mnemonic s';*)
  if List.mem mnemonic ["ret"] then Some C_ret
  else if List.mem mnemonic ["eret"] then Some C_eret
  else if List.mem mnemonic ["br"] then Some (C_branch_register mnemonic)
  else if
    (String.length mnemonic >= 2 && String.sub s 0 2 = "b.") || List.mem mnemonic ["b"; "bl"]
  then
    match parse_target s' with
    | None -> raise (Failure ("b./b/bl parse error for: \"" ^ s ^ "\"\n"))
    | Some (a, s) ->
        if mnemonic = "b" then Some (C_branch (a, s))
        else if mnemonic = "bl" then Some (C_branch_and_link (a, s))
        else Some (C_branch_cond (mnemonic, a, s))
  else if List.mem mnemonic ["cbz"; "cbnz"] then
    match parse_drop_one s' with
    | None -> raise (Failure ("cbz/cbnz 1 parse error for: " ^ s ^ "\n"))
    | Some s' -> (
        match parse_target s' with
        | None -> raise (Failure ("cbz/cbnz 2 parse error for: " ^ s ^ "\n"))
        | Some (a, s) -> Some (C_branch_cond (mnemonic, a, s))
      )
  else if List.mem mnemonic ["tbz"; "tbnz"] then
    match parse_drop_one s' with
    | None -> raise (Failure ("tbz/tbnz 1 parse error for: " ^ s ^ "\n"))
    | Some s'' -> (
        match parse_drop_one s'' with
        | None -> raise (Failure ("tbz/tbnz 2 parse error for: " ^ s ^ "\n"))
        | Some s''' -> (
            match parse_target s''' with
            | None -> raise (Failure ("tbz/tbnz 3 parse error for: " ^ s ^ "\n"))
            | Some (a, s'''') ->
                (*                Printf.printf "s=%s mnemonic=%s s'=%s s''=%s s'''=%s s''''=%s\n"s mnemonic s' s'' s''' s'''';*)
                Some (C_branch_cond (mnemonic, a, s''''))
          )
      )
  else if List.mem mnemonic ["smc"; "hvc"] then Some (C_smc_hvc s')
  else None

(*
    )
   *)

let branch_targets test =
  init_objdump ();

  (* **         read in branch-table description file                   ** *)
  let branch_data : (natural (*a_br*) * (natural (*a_table*) * natural)) (*size*) list =
    match !Globals.branch_table_data_file with
    | None -> [] (*None*)
    (*Warn.fatal "no branch table data file\n"*)
    | Some filename -> (
        match read_source_file filename with
        | MyFail s -> Warn.fatal2 "%s\ncouldn't read branch table data file: \"%s\"\n" s filename
        | Ok lines ->
            let parse_line (s : string) : (natural * (natural * natural)) option =
              match Scanf.sscanf s " %x: %x %x " (fun a_br a_table n -> (a_br, a_table, n)) with
              | a_br, a_table, n ->
                  Some
                    (Nat_big_num.of_int a_br, (Nat_big_num.of_int a_table, Nat_big_num.of_int n))
              | exception _ -> Warn.fatal "couldn't parse branch table data file line: \"%s\"\n" s
            in
            List.filter_map parse_line (List.tl (Array.to_list lines))
      )
  in

  let ((c, addr, bs) as rodata : Dwarf.p_context * Nat_big_num.num * char list) =
    Dwarf.extract_section_body test.elf_file ".rodata" false
  in
  (* chop into 4-byte words - as needed for branch offset tables,
     though not for all other things in .rodata *)
  let rodata_words : (natural * natural) list = Dwarf.words_of_byte_list addr bs [] in

  (*
  let objdump_rodata : (natural(*addr*) * natural(*word*)) list =
    match !Globals.objdump_rodata with
    | None -> Warn.fatal0 "no objdump_rodata file\n" ""
    | Some filename ->
       match read_source_file filename with
       | MyFail s ->
          Warn.fatal2 "%s\ncouldn't read objdump_rodata file: \"%s\"\n" s filename
       | Ok lines ->
          let parse_line (s:string) : (natural*natural) option =
            match Scanf.sscanf s " %x: %x .word %x" (fun a -> fun w -> fun _ -> (a,w)) with
            | (a,w) ->
               Some (Nat_big_num.of_int a, Nat_big_num.of_int w)
            | exception _ -> None
          in
          (List.filter_map parse_line (Array.to_list lines)) in
 *)
  let rec natural_assoc_opt n nys =
    match nys with
    | [] -> None
    | (n', y) :: nys' -> if Nat_big_num.equal n n' then Some y else natural_assoc_opt n nys'
  in

  let succ_addr addr = Nat_big_num.add addr (Nat_big_num.of_int 4) in

  let targets_of_control_flow_insn (addr : natural) (c : control_flow_insn) : (addr * string) list
      =
    match c with
    | C_ret -> []
    | C_eret -> []
    | C_branch (a, s) -> [(a, s)]
    | C_branch_and_link (a, s) ->
       if s = "<abort>" then (* special case because abort doesn't return *)
         [(a, s)] 
       else
         [(a, s); (succ_addr addr, "<return>")] (* we rely later on the ordering of these *)
    | C_branch_cond (is, a, s) ->
        let succ_addr = Nat_big_num.add addr (Nat_big_num.of_int 4) in
        [(a, s); (succ_addr, "<fallthrough>")]
    | C_branch_register r1 -> (
        match natural_assoc_opt addr branch_data with
        | None -> Warn.fatal "no branch table for address%s\n" (pp_addr addr)
        | Some (a_table, n) ->
            let rec f i =
              if i > Nat_big_num.to_int n then []
              else
                let table_entry_addr = Nat_big_num.add a_table (Nat_big_num.of_int (4 * i)) in
                match natural_assoc_opt table_entry_addr rodata_words with
                | None ->
                    Warn.fatal2 "no branch table entry for address %s, for code address %s\n"
                      (pp_addr table_entry_addr) (pp_addr addr)
                | Some table_entry ->
                    let a_target =
                      Nat_big_num.modulus
                        (Nat_big_num.add a_table table_entry)
                        (Nat_big_num.pow_int_positive 2 32)
                    in
                    (* that 32 is good for the sign-extended negative 32-bit offsets we see
                       in the Hf branch tables *)
                    (a_target, "<indirect" ^ string_of_int i ^ ">") :: f (i + 1)
            in
            f 0
      )
    | C_smc_hvc s -> []
  in

  (* pull out instructions from text section, assuming 4-byte insns *)
  let p, text_addr, bs = Dwarf.extract_text test.elf_file in
  let instructions : (natural * natural) list = Dwarf.words_of_byte_list text_addr bs [] in

  let control_flow_insn ((addr : natural), (i : natural)) : (addr * control_flow_insn) option =
    match lookup_objdump_lines addr with
    | Some (i, s) -> (
        match
          Scanf.sscanf s "%s %n" (fun mnemonic n ->
              let s' = String.sub s n (String.length s - n) in
              (mnemonic, s'))
        with
        | mnemonic, s' -> (
            (*           if (String.length mnemonic >=2 && String.sub s 0 2 ="b.") || List.mem mnemonic ["b"; "bl"; "br"; "ret"; "tbz"; "tbnz"; "cbz"; "cbnz"; "eret"; "smc"; "hvc"] then*)
            match parse_control_flow_instruction s mnemonic s' with
            | None -> None (*Warn.fatal "parse error %s\n" s*)
            | Some c -> Some (addr, c)
            (*           else
                        None*)
            | exception _ -> None
          )
      )
    | None -> None
  in

  let control_flow_insns : (addr * control_flow_insn) list =
    List.filter_map control_flow_insn instructions
  in

  let control_flow_insns_with_targets : (addr * control_flow_insn * (addr * string) list) list =
    List.map (function a, c -> (a, c, targets_of_control_flow_insn a c)) control_flow_insns
  in

  let index_of_address (addr : natural) : int =
    Nat_big_num.to_int (Nat_big_num.sub addr text_addr) / 4
  in
  let address_of_index (i : int) : natural =
    Nat_big_num.add text_addr (Nat_big_num.of_int (i * 4))
  in

  let size = List.length instructions in

  let control_flow_insns_with_targets_array :
      (addr * natural (*isns*) * control_flow_insn option * ((addr * int * string) list)) array =
    Array.init size (function k ->
        (let a, i = List.nth instructions k in
         let co = control_flow_insn (a, i) in
         match co with
         | None -> (a, i, None, [(succ_addr a, k + 1, "succ")])
         | Some (a', c) ->
             let targets = targets_of_control_flow_insn a c in
             let targets' =
               List.map (function a'', s'' -> (a'', index_of_address a'', s'')) targets
             in
             (a, i, Some c, targets')))
  in

  let indirect_branches =
    List.filter
      (function
        | a, c, ts -> (
            match c with C_branch_register _ -> true | _ -> false
          ))
      control_flow_insns_with_targets
  in

  ( control_flow_insns_with_targets,
    control_flow_insns_with_targets_array,
    index_of_address,
    address_of_index,
    indirect_branches )

let pp_branch_targets (xs : (addr * control_flow_insn * (addr * string) list) list) =
  String.concat ""
    (List.map
       (function
         | a, c, ts ->
             pp_addr a ^ ":  " ^ pp_control_flow_instruction c ^ " -> "
             ^ String.concat "," (List.map (function a', s -> pp_addr a' ^ "" ^ s ^ "") ts)
             ^ "\n")
       xs)

let come_from_table (xs : (addr * control_flow_insn * (addr * string) list) list) :
    (int, addr * control_flow_insn * string) Hashtbl.t =
  let t = Hashtbl.create 1000 in
  List.iter
    (function
      | a, c, ts ->
          List.iter
            (function
              | a', s ->
                  let aint' = Nat_big_num.to_int a' in
                  (* let prev = Hashtbl.find t aint' in *)
                  let come_from = (a, c, s) in
                  Hashtbl.add t aint' come_from)
            ts)
    xs;
  t

let come_froms t addr : (addr * control_flow_insn * string) list =
  List.rev (Hashtbl.find_all t (Nat_big_num.to_int addr))

let pp_come_froms (addr : addr) (cfs : (addr * control_flow_insn * string) list) : string =
  match cfs with
  | [] -> ""
  | _ ->
      " <- "
      ^ String.concat ","
          (List.map
             (function
               | a, c, s ->
                   pp_come_from_addr_wrt addr c a ^ "("
                   ^ pp_control_flow_instruction_short c
                   ^ ")" ^ s)
             cfs)

(*****************************************************************************)
(*        call-graph                                                         *)
(*****************************************************************************)
(*module P = Pack*)
(*
        Warning 49: no cmi file was found in path for module Pack 
 *)
(*
module D = Pack.Digraph
        Error: Unbound module Pack
*)

    
let pp_call_graph test
    ( control_flow_insns_with_targets,
      control_flow_insns_with_targets_array,
      index_of_address,
      address_of_index,
      indirect_branches ) =
  (* take the nodes to be all the elf symbol addresses of stt_func
     symbol type (each with their list of elf symbol names) together
     with all the other-address bl-targets (of which in Hf there are just
     three, the same in O0 and O2, presumably explicit in assembly) *)
  let elf_symbols : (natural * string list) list =
    let elf_symbol_addresses =
      List.sort_uniq compare
        (List.filter_map
           (fun (name, (typ, size, address, mb, binding)) ->
             if typ = Elf_symbol_table.stt_func then Some address else None)
           test.symbol_map)
    in
    List.map
      (fun address ->
        let names =
          List.sort_uniq compare
            (List.filter_map
               (fun (name, (typ, size, address', mb, binding)) ->
                 if address' = address && String.length name >= 1 && name.[0] <> '$' then
                   Some name
                 else None)
               test.symbol_map)
        in
        (address, names))
      elf_symbol_addresses
  in

  let extra_bl_targets' =
    List.filter_map
      (function
        | a, c, ts -> (
            match c with
            | C_branch_and_link (a', s') ->
                if
                  not (List.exists (function a'', ss'' -> Nat_big_num.equal a' a'') elf_symbols)
                then Some (a', ["FROM BL:" ^ s'])
                else None
            | _ -> None
          ))
      control_flow_insns_with_targets
  in

  let rec dedup axs acc =
    match axs with
    | [] -> acc
    | (a, x) :: axs' ->
        if not (List.exists (function a', x' -> Nat_big_num.equal a a') acc) then
          dedup axs' ((a, x) :: acc)
        else dedup axs' acc
  in

  let extra_bl_targets = dedup extra_bl_targets' [] in

  let nodes0 =
    List.sort
      (function
        | a, ss -> (
            function a', ss' -> Nat_big_num.compare a a'
          ))
      (elf_symbols @ extra_bl_targets)
  in

  let nodes : node list = List.map (function a, ss -> (a, index_of_address a, ss)) nodes0 in

  let pp_node ((a, k, ss) as node) =
    pp_addr a (*" " ^ string_of_int k ^*) ^ " <" ^ String.concat ", " ss ^ ">"
  in

  let node_of_index k =
    match List.find_opt (function a, k', ss -> k' = k) nodes with
    | Some n -> n
    | None ->
        Warn.nonfatal "node_of_index %d\n" k;
        List.hd nodes
  in

  let rec stupid_reachability (acc_reachable : int list) (acc_bl_targets : int list)
      (todo : int list) : int list * int list =
    match todo with
    | [] -> (acc_reachable, acc_bl_targets)
    | k :: todo' ->
        if List.mem k acc_reachable then stupid_reachability acc_reachable acc_bl_targets todo'
        else
          let a, i, co, targets = control_flow_insns_with_targets_array.(k) in
          let target_indices = List.map (function a'', k'', s'' -> k'') targets in
          let non_bl_targets, bl_targets =
            match co with
            | Some (C_branch_and_link (_, _)) ->
               (match target_indices with
               | [t1; t2] -> 
                  ([t2], [t1])
               | [t1] ->
                  ([], [t1])
               )
            | _ -> (target_indices, [])
          in
          stupid_reachability (k :: acc_reachable)
            (List.sort_uniq compare (bl_targets @ acc_bl_targets))
            (non_bl_targets @ todo')
  in

  let bl_targets k =
    let reachable, bl_targets = stupid_reachability [] [] [k] in
    bl_targets
  in

  let call_graph =
    List.map
      (function (a, k, ss) as node -> (node, List.map node_of_index (bl_targets k)))
      nodes
  in

  let pp_call_graph_entry (n,ns) = 
    pp_node n ^ ":\n"
    ^ String.concat "" (List.map (function n' -> "  " ^ pp_node n' ^ "\n") ns) in
  
  let pp_call_graph call_graph =
    String.concat ""
      (List.map pp_call_graph_entry call_graph)
  in
  
  let rec stupid_reachability' (acc_reachable : node list) (todo : node list) : node list =
    match todo with
    | [] -> acc_reachable
    | ((a,k,ss) as n) :: todo' ->
       if List.exists (function (a',k',ss')->k'=k) acc_reachable then
         stupid_reachability' acc_reachable todo'
        else
          let (_,targets) = List.find (function ((a',k',ss'),_) -> k'=k) call_graph in
          stupid_reachability' (n :: acc_reachable) (targets @ todo')
  in

  let transitive_call_graph = 
    List.map
      (function ((a,k,ss) as n) ->
         let (_,targets) = List.find (function ((a',k',ss'),_) -> k'=k) call_graph in
         (n,(stupid_reachability' [] targets))
      ) nodes in

  let pp_transitive_call_graph transitive_call_graph =
    String.concat ""
      (List.map 
         (function (((a,k,ss) as n),ns) ->
            (if List.exists (function (a',k',ss')->k'=k) ns then "RECURSIVE " else "") ^ "\n"
            ^ pp_call_graph_entry (n,ns))
         transitive_call_graph)
       in
  
  pp_call_graph call_graph
  ^ "*************** transitive call graph **************\n"
  ^ pp_transitive_call_graph transitive_call_graph

(*
type node = natural * string

  (* hackery to count reachable instructions *)
  (* assumes addresses small enough to fit in int *)
  let touched : int Array.t = Array.make (List.length instructions) 0 in
  let base_address = match instructions with (addr,s)::_ -> addr in
  let index_of_address (addr:natural) : int =
    Nat_big_num.to_int (Nat_big_num.div (Nat_big_num.sub addr base_address)
                       (Nat_big_num.of_int 4))
  in
  let address_of_index (i:int) : natural =
    Nat_big_num.mul (Nat_big_num.of_int 4)
                    (Nat_big_num.add base_address (Nat_big_num.of_int i))
  in
  let touch (addr:natural) = let i = index_of_address addr in touched.(i) <- touched.(i) +1 in
  let count_touched () =
    Array.fold_left (
      function count -> function c ->
        if c=0 then count else count+1) 0 touched
  in
  (* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a *)

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
       if mnemonic = "b" then [(parse_addr s1,s2)] else
       (*b./bl (assuming the bl can return) / cbz/cbnz/tbz/tbnz*)
         (parse_addr s1,s2)::branch_successors succ_addr
    | Some (Some (C_smc_hvc s)) -> []
  in

  let branch_instructions : (natural * natural * control_flow_insn) list =
    List.filter_map
      (fun ((addr:natural),(i:natural)) -> match is_control_flow_insn addr with
         None->None | Some None -> None| Some (Some t) -> Some (addr,i,t))
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
             (fun (name, (typ, size, address', mb, binding)) ->
                if address'=address && String.length name >=1 && name.[0]<>'$'
                then Some name else None) test.symbol_map)
        in
        (address,"elf:"^String.concat "__" names))
      elf_symbol_addresses
  in

  let fake_symbol = (parse_addr "40009eb4", "api.c:1526") in
  let fake_symbols = [fake_symbol] in

  let nodes = List.sort_uniq compare (fake_symbols @ branch_targets @ elf_symbols) in
    (*TODO: merge same-address pairs*)
  let pp_node (addr,s) = "\"a" ^ pp_addr addr^"_"^s^"\"" in

  (*  let nodes_initial = List.filter (function (addr,s)->s="<sync_lower_exception>") nodes in*)
  let nodes_initial = List.filter (function (addr,s)->s="<api_share_memory>") nodes in
  (*  let nodes_initial = fake_symbols in *)

  (*
  let edges = List.flatten (List.map (function (addr,s) ->
                (List.map (function (addr',s')-> ((addr,s),(addr',s')))
                (List.sort_uniq compare (branch_successors addr)))) nodes) in
   *)

  let edges : (node,node list) Hashtbl.t = Hashtbl.create 1000 in

  let rec stupid_reachability (max_depth:int) (acc_reachable:node list)
      (todo:node list) : node list =
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

 *)

(*****************************************************************************)
(*        pretty-print the result                                            *)
(*****************************************************************************)

let pp_test test =
  init_objdump ();

  (* pull out instructions from text section, assuming 4-byte insns *)
  let p, addr, bs = Dwarf.extract_text test.elf_file in
  let instructions : (natural * natural) list = Dwarf.words_of_byte_list addr bs [] in

  (* hack to cut down problem size for runtime experimentation *)
  (* 14.5s with show_vars; 3.2s without.   13.3s with myconcat stubbed out, 15.2s with String.concat *)
  let rec first n xs = if n = 0 then [] else match xs with x :: xs' -> x :: first (n - 1) xs' in
  let instructions = if !Globals.clip_binary then first 1000 instructions else instructions in

  (* compute the come-from data *)
  let ( control_flow_insns_with_targets,
        control_flow_insns_with_targets_array,
        index_of_address,
        address_of_index,
        indirect_branches ) =
    branch_targets test
  in
  let t = come_from_table control_flow_insns_with_targets in

  (* compute the inlining data *)
  let iss = Dwarf.analyse_inlined_subroutines test.dwarf_static.ds_dwarf in
  let issr = Dwarf.analyse_inlined_subroutines_by_range iss in

  let last_frame_info = ref "" in
  let last_var_info = ref [] in
  let last_source_info = ref "" in

  let pp_instruction ((addr : natural), (i : natural)) =
    (* the come_froms for this address, calculated first to determine whether this is the start of a basic block *)
    let come_froms' = come_froms t addr in

    (* the elf symbols at this address, if any (and reset the last_var_info if any) *)
    let elf_symbols = elf_symbols_of_address test addr in
    (match elf_symbols with [] -> () | _ -> last_var_info := []);

    (if come_froms' <> [] || elf_symbols <> [] then "\n" else "")
    ^ String.concat ""
        (List.map (fun (s : string) -> pp_addr addr ^ " <" ^ s ^ ">:\n") elf_symbols)
    (* the source file lines (if any) associated to this address *)
    ^ begin
        if !Globals.show_source then
          let source_info =
            match pp_dwarf_source_file_lines () test.dwarf_static true addr with
            | Some s -> s ^ "\n"
            | None -> ""
          in
          if source_info = !last_source_info then "" (*"unchanged\n"*)
          else (
            last_source_info := source_info;
            source_info
          )
        else ""
      end
    (* the inlining info for this address *)
    ^
    let issr_here =
      List.filter (function (n1, n2), (m, n), is -> Nat_big_num.equal n1 addr) issr
    in
    Dwarf.pp_inlined_subroutines_by_range test.dwarf_static.ds_dwarf issr_here
    (* the frame info for this address *)
    ^ begin
        if !Globals.show_cfa then
          let frame_info = pp_frame_info test addr in
          if frame_info = !last_frame_info then "" (*"CFA: unchanged\n"*)
          else (
            last_frame_info := frame_info;
            frame_info
          )
        else ""
      end
    (* the variables whose location ranges include this address *)
    ^ begin
        if (*true*) !Globals.show_vars then (
          let als_old = !last_var_info in
          let als_new (*fald*) = Dwarf.filtered_analysed_location_data test.dwarf_static addr in
          last_var_info := als_new;
          Dwarf.pp_analysed_location_data_diff test.dwarf_static.ds_dwarf als_old als_new
        )
        else ""
      end
    (* the address and (hex) instruction *)
    ^ pp_addr addr
    ^ ":  " ^ pp_addr i
    (* the dissassembly from objdump, if it exists *)
    ^ "  "
    ^ begin
        match lookup_objdump_lines addr with
        | Some (i', s) ->
            if i = i' then s
            else
              Warn.fatal2 "instruction mismatch - linksem: %s vs objdump: %s\n" (pp_addr i)
                (pp_addr i')
        | None -> ""
      end
    ^ begin
        match
          List.find_opt (function a, c, ts -> Nat_big_num.equal a addr) indirect_branches
        with
        | Some (a, c, ts) ->
            " -> "
            ^ String.concat ","
                (List.map (function a', s -> pp_target_addr_wrt addr c a' ^ "" ^ s ^ "") ts)
            ^ " "
        | None -> ""
      end
    ^ pp_come_froms addr come_froms' ^ "\n"
    (*    ^ "\n"*)
  in

  "************** aggregate type definitions *****************\n"
  ^ (let d = test.dwarf_static.ds_dwarf in
     let c = Dwarf.p_context_of_d d in
     Dwarf.pp_all_aggregate_types c d)
  ^ "\n************** instructions *****************\n"
  ^ String.concat "" (List.map pp_instruction instructions)
  ^ "\n************** branch targets *****************\n"
  ^ pp_branch_targets control_flow_insns_with_targets
  ^ "\n************** call graph *****************\n"
  ^ pp_call_graph test
      ( control_flow_insns_with_targets,
        control_flow_insns_with_targets_array,
        index_of_address,
        address_of_index,
        indirect_branches )

(*****************************************************************************)
(*        top-level                                                          *)
(*****************************************************************************)

let process_file (filename : string) : unit =
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
