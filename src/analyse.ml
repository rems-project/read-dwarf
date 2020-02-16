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

type addr = natural

type instruction = natural

type index = int (* index into instruction-indexed arrays *)

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

let parse_elf_file (filename : string) : test =
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
  | (Sail_interface.ELF_Class_32 _, _) -> Warn.fatal "%s" "cannot handle ELF_Class_32"
  | (_, Elf_file.ELF_File_32 _) -> Warn.fatal "%s" "cannot handle ELF_File_32"
  | (Sail_interface.ELF_Class_64 (segments, e_entry, e_machine), Elf_file.ELF_File_64 f1) ->
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
(*        read file of text lines                                            *)
(*****************************************************************************)

(** 'safe_open_in filename f' will open filename, pass it to f and cloth
    the channel at the end or when an exception is raised
    TODO use Protect.protect *)
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

let read_file_lines (name : string) : string array ok_or_fail =
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
  | exception Sys_error s -> MyFail (Printf.sprintf "read_file_lines Sys_error \"%s\"\n" s)

(*****************************************************************************)
(*        find and pretty-print source lines for addresses                   *)
(*****************************************************************************)

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
        | (Some cd, Some d, f) -> Filename.concat cd (Filename.concat d f)
        | (Some cd, None, f) -> Filename.concat cd f
        | (None, Some d, f) -> Filename.concat d f
        | (None, None, f) -> f
      in
      match read_file_lines filename with
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

let pp_source_line so = match so with Some s -> s (*" (" ^ s ^ ")"*) | None -> "file not found"

let pp_dwarf_source_file_lines m ds (pp_actual_line : bool) (a : natural) : string option =
  let sls = Dwarf.source_lines_of_address ds a in
  match sls with
  | [] -> None
  | _ ->
      Some
        (String.concat ", "
           (List.map
              (fun ((comp_dir, dir, file), n, lnr, subprogram_name) ->
                let comp_dir' =
                  match !Globals.comp_dir with
                  | None -> comp_dir
                  | Some comp_dir'' -> (
                      match comp_dir with
                      | None -> Some comp_dir''
                      | Some s -> Some (Filename.concat comp_dir'' s)
                    )
                in
                file ^ ":" ^ Nat_big_num.to_string n ^ " (" ^ subprogram_name ^ ")"
                ^
                if pp_actual_line then
                  pp_source_line (source_line (comp_dir', dir, file) (Nat_big_num.to_int n))
                else "")
              sls))

(* source line info for matching instructions between binaries - ignoring inlining for now *)
let dwarf_source_file_line_numbers test (a : natural) :
    (string (*subprogram name*) * int) (*line number*) list =
  let sls = Dwarf.source_lines_of_address test.dwarf_static a in
  List.map
    (fun ((comp_dir, dir, file), n, lnr, subprogram_name) ->
      (subprogram_name, Nat_big_num.to_int n))
    sls

(*****************************************************************************)
(*        look up address in ELF symbol table                                *)
(*****************************************************************************)

let elf_symbols_of_address (test : test) (addr : natural) : string list =
  List.filter_map
    (fun (name, (typ, size, address, mb, binding)) -> if address = addr then Some name else None)
    test.symbol_map

let mk_elf_symbols_array test instructions : string list array =
  Array.of_list (List.map (function (addr, i) -> elf_symbols_of_address test addr) instructions)

(*****************************************************************************)
(*        look up address in frame info                                      *)
(*****************************************************************************)

let aof ((a : natural), (cfa : string), (regs : (string * string) list)) = a

let rec f (aof : 'b -> natural) (a : natural) (last : 'b option) (bs : 'b list) : 'b option =
  match (last, bs) with
  | (None, []) -> None
  | (Some b', []) -> if Nat_big_num.greater_equal a (aof b') then Some b' else None
  | (None, b'' :: bs') -> f aof a (Some b'') bs'
  | (Some b', b'' :: bs') ->
      if Nat_big_num.less a (aof b') then None
      else if Nat_big_num.greater_equal a (aof b') && Nat_big_num.less a (aof b'') then Some b'
      else f aof a (Some b'') bs'

let mk_frame_info_array test instructions :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array =
  Array.of_list
    (List.map
       (function (addr, i) -> f aof addr None test.dwarf_semi_pp_frame_info)
       instructions)

let pp_frame_info frame_info_array k : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match frame_info_array.(k) with
  | None -> "<no frame info for this address>\n"
  | Some ((a : natural), (cfa : string), (regs : (string * string) list)) ->
      pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " "
      ^ String.concat " " (List.map (function (rname, rinfo) -> rname ^ ":" ^ rinfo) regs)
      ^ "\n"

(*****************************************************************************)
(*        pull disassembly out of an objdump -d file                         *)
(*****************************************************************************)

let parse_objdump_lines lines =
  let parse_line (s : string) : (natural * natural * string) option =
    match Scanf.sscanf s " %x: %x %n" (fun a i n -> (a, i, n)) with
    | (a, i, n) ->
        let s' = String.sub s n (String.length s - n) in
        Some (Nat_big_num.of_int a, Nat_big_num.of_int i, s')
    | exception _ -> None
  in
  List.filter_map parse_line (Array.to_list lines)

let parse_objdump_file filename_objdump_d =
  match read_file_lines filename_objdump_d with
  | MyFail s -> Warn.fatal2 "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename_objdump_d
  | Ok lines -> parse_objdump_lines lines

let mk_objdump_lines_array parsed_objdump_lines instructions :
    (addr * natural (*insn/data*) * string) option array =
  Array.of_list
    (List.map
       (function
         | (addr, i) -> (
             match List.filter (function (a, i, s) -> a = addr) parsed_objdump_lines with
             | [l] -> Some l
             | [] -> None
             | l :: ls ->
                 Warn.nonfatal "Warning: multiple objdump lines for the same address:%s\n"
                   (pp_addr addr);
                 Some l
           ))
       instructions)

(*****************************************************************************)
(*   parse control-flow instruction asm from objdump and branch table data   *)
(*****************************************************************************)

type control_flow_insn =
  | C_no_instruction
  | C_plain
  | C_ret
  | C_eret
  | C_branch of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_and_link of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_cond of string (*mnemonic*) * addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_register of string (*argument*)
  | C_smc_hvc of string

(*mnemonic*)

type target_kind =
  | T_plain_successor
  | T_branch
  | T_branch_and_link_call
  | T_branch_and_link_call_noreturn
  | T_branch_and_link_successor
  | T_branch_cond_branch
  | T_branch_cond_successor
  | T_branch_register
  | T_smc_hvc_successor

let pp_control_flow_instruction c =
  match c with
  | C_no_instruction -> "no instruction"
  | C_plain -> "plain"
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch (a, s) -> "b" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_and_link (a, s) -> "bl" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_cond (is, a, s) -> is ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_register r -> "br"
  | C_smc_hvc s -> "smc/hvc " ^ s

let pp_control_flow_instruction_short c =
  match c with
  | C_no_instruction -> "no instruction"
  | C_plain -> "plain"
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch (a, s) -> "b"
  | C_branch_and_link (a, s) -> "bl"
  | C_branch_cond (is, a, s) -> is
  | C_branch_register r -> "br"
  | C_smc_hvc s -> "smc/hvc"

let pp_target_kind_short = function
  | T_plain_successor -> "succ"
  | T_branch -> "b"
  | T_branch_and_link_call -> "bl"
  | T_branch_and_link_call_noreturn -> "bl-noreturn"
  | T_branch_and_link_successor -> "bl-succ"
  | T_branch_cond_branch -> "b.cc"
  | T_branch_cond_successor -> "b.cc-succ"
  | T_branch_register -> "br"
  | T_smc_hvc_successor -> "smc-hvc-succ"

(* hacky parsing of assembly from objdump -d to identify control-flow instructions and their arguments *)

let parse_addr (s : string) : natural = Scanf.sscanf s "%Lx" (fun i64 -> Nat_big_num.of_int64 i64)

let parse_target s =
  match Scanf.sscanf s "%s %s" (fun s1 s2 -> (s1, s2)) with
  | (s1, s2) -> Some (parse_addr s1, s2)
  | exception _ -> None

let parse_drop_one s =
  match
    Scanf.sscanf s "%s %n" (fun s1 n ->
        let s' = String.sub s n (String.length s - n) in
        (s1, s'))
  with
  | (s1, s') -> Some s'
  | exception _ -> None

let parse_control_flow_instruction s mnemonic s' : control_flow_insn =
  (*  Printf.printf "s=\"%s\" mnemonic=\"%s\" s'=\"%s\"\n"s mnemonic s';*)
  if List.mem mnemonic ["ret"] then C_ret
  else if List.mem mnemonic ["eret"] then C_eret
  else if List.mem mnemonic ["br"] then C_branch_register mnemonic
  else if
    (String.length mnemonic >= 2 && String.sub s 0 2 = "b.") || List.mem mnemonic ["b"; "bl"]
  then
    match parse_target s' with
    | None -> raise (Failure ("b./b/bl parse error for: \"" ^ s ^ "\"\n"))
    | Some (a, s) ->
        if mnemonic = "b" then C_branch (a, s)
        else if mnemonic = "bl" then C_branch_and_link (a, s)
        else C_branch_cond (mnemonic, a, s)
  else if List.mem mnemonic ["cbz"; "cbnz"] then
    match parse_drop_one s' with
    | None -> raise (Failure ("cbz/cbnz 1 parse error for: " ^ s ^ "\n"))
    | Some s' -> (
        match parse_target s' with
        | None -> raise (Failure ("cbz/cbnz 2 parse error for: " ^ s ^ "\n"))
        | Some (a, s) -> C_branch_cond (mnemonic, a, s)
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
                C_branch_cond (mnemonic, a, s'''')
          )
      )
  else if List.mem mnemonic ["smc"; "hvc"] then C_smc_hvc s'
  else C_plain

let mk_control_flow_insns_with_targets_array test instructions objdump_lines_array
    index_of_address address_of_index filename_branch_table size :
    (addr * instruction * control_flow_insn * (target_kind * addr * index * string) list) array =
  (* read in and parse branch-table description file *)
  let branch_data : (natural (*a_br*) * (natural (*a_table*) * natural)) (*size*) list =
    match read_file_lines filename_branch_table with
    | MyFail s ->
        Warn.fatal2 "%s\ncouldn't read branch table data file: \"%s\"\n" s filename_branch_table
    | Ok lines ->
        let parse_line (s : string) : (natural * (natural * natural)) option =
          match Scanf.sscanf s " %x: %x %x " (fun a_br a_table n -> (a_br, a_table, n)) with
          | (a_br, a_table, n) ->
              Some (Nat_big_num.of_int a_br, (Nat_big_num.of_int a_table, Nat_big_num.of_int n))
          | exception _ -> Warn.fatal "couldn't parse branch table data file line: \"%s\"\n" s
        in
        List.filter_map parse_line (List.tl (Array.to_list lines))
  in

  (* pull out .rodata section from ELF *)
  let ((c, addr, bs) as rodata : Dwarf.p_context * Nat_big_num.num * char list) =
    Dwarf.extract_section_body test.elf_file ".rodata" false
  in
  (* chop into 4-byte words - as needed for branch offset tables,
     though not for all other things in .rodata *)
  let rodata_words : (natural * natural) list = Dwarf.words_of_byte_list addr bs [] in

  (* compute targets of each instruction *)
  let rec natural_assoc_opt n nys =
    match nys with
    | [] -> None
    | (n', y) :: nys' -> if Nat_big_num.equal n n' then Some y else natural_assoc_opt n nys'
  in

  let succ_addr addr = Nat_big_num.add addr (Nat_big_num.of_int 4) in

  let targets_of_control_flow_insn_without_index (addr : natural) (c : control_flow_insn) :
      (target_kind * addr * string) list =
    match c with
    | C_no_instruction -> []
    | C_plain -> [(T_plain_successor, succ_addr addr, "")]
    | C_ret -> []
    | C_eret -> []
    | C_branch (a, s) -> [(T_branch, a, s)]
    | C_branch_and_link (a, s) ->
        (* special-case non-return functions to have no successor target of calls *)
        if List.mem s ["<abort>"; "<panic>"; "<__stack_chk_fail>"] then
          [(T_branch_and_link_call_noreturn, a, s)]
        else
          [
            (T_branch_and_link_call, a, s);
            (T_branch_and_link_successor, succ_addr addr, "<return>");
          ]
    | C_branch_cond (is, a, s) ->
        [(T_branch_cond_branch, a, s); (T_branch_cond_successor, succ_addr addr, "<fallthrough>")]
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
                    (T_branch_register, a_target, "<indirect" ^ string_of_int i ^ ">") :: f (i + 1)
            in
            f 0
      )
    | C_smc_hvc s -> [(T_smc_hvc_successor, succ_addr addr, "<C_smc_hvc successor>")]
  in

  let targets_of_control_flow_insn (addr : natural) (c : control_flow_insn) :
      (target_kind * addr * index * string) list =
    List.map
      (function (tk, a'', s'') -> (tk, a'', index_of_address a'', s''))
      (targets_of_control_flow_insn_without_index addr c)
  in

  let control_flow_insn (k : index) ((addr : natural), (i : natural)) : control_flow_insn =
    match objdump_lines_array.(k) with
    | Some (a, i, s) -> (
        match
          Scanf.sscanf s "%s %n" (fun mnemonic n ->
              let s' = String.sub s n (String.length s - n) in
              (mnemonic, s'))
        with
        | (mnemonic, s') -> parse_control_flow_instruction s mnemonic s'
      )
    | None -> C_no_instruction
    (*Warn.fatal "control_flow_insn: no objdump for %s" (pp_addr addr)*)
  in

  let control_flow_insns_with_targets_array :
      (addr * natural (*insn*) * control_flow_insn * (target_kind * addr * index * string) list)
      array =
    Array.init size (function k ->
        let (a, i) = List.nth instructions k in
        let c = control_flow_insn k (a, i) in
        let targets = targets_of_control_flow_insn a c in
        (a, i, c, targets))
  in

  control_flow_insns_with_targets_array

(* pull out indirect branches *)
let mk_indirect_branches control_flow_insns_with_targets_array =
  List.filter
    (function
      | (a, i, c, ts) -> (
          match c with C_branch_register _ -> true | _ -> false
        ))
    (Array.to_list control_flow_insns_with_targets_array)

let highlight c =
  match c with
  | C_no_instruction -> false
  | C_plain | C_ret | C_eret | C_branch_and_link (_, _) | C_smc_hvc _ -> false
  | C_branch (_, _) | C_branch_cond (_, _, _) | C_branch_register _ -> true

(* highlight branch targets to earlier addresses*)
let pp_target_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Nat_big_num.less a addr then "^" else "") ^ pp_addr a

(* highlight branch come-froms from later addresses*)
let pp_come_from_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Nat_big_num.greater a addr then "v" else "") ^ pp_addr a

(*  
let pp_branch_targets (xs : (addr * control_flow_insn * (target_kind * addr * int * string) list) list)
    =
  String.concat ""
    (List.map
       (function
         | (a, c, ts) ->
             pp_addr a ^ ":  " ^ pp_control_flow_instruction c ^ " -> "
             ^ String.concat ","
                 (List.map (function (tk, a', k', s) -> pp_addr a' ^ "" ^ s ^ "") ts)
             ^ "\n")
       xs)
 *)

(*****************************************************************************)
(*   invert control-flow data to get come-from data                          *)
(*****************************************************************************)

let mk_come_from_array control_flow_insns_with_targets_array :
    (target_kind * addr * index * control_flow_insn * string) list array =
  let size = Array.length control_flow_insns_with_targets_array in
  let come_from_array = Array.make size [] in
  Array.iteri
    (function
      | k -> (
          function
          | (a, i, c, ts) ->
              List.iter
                (function
                  | (tk, a', k', s) ->
                      let come_from = (tk, a, k, c, s) in
                      if k' < size then come_from_array.(k') <- come_from :: come_from_array.(k')
                      else ())
                ts
        ))
    control_flow_insns_with_targets_array;
  Array.iteri
    (function
      | k -> (
          function cfs -> come_from_array.(k) <- List.rev cfs
        ))
    come_from_array;
  come_from_array

let pp_come_froms (addr : addr)
    (cfs : (target_kind * addr * index * control_flow_insn * string) list) : string =
  match cfs with
  | [] -> ""
  | _ ->
      " <- "
      ^ String.concat ","
          (List.map
             (function
               | (tk, a, k, c, s) ->
                   pp_come_from_addr_wrt addr c a ^ "(" ^ pp_target_kind_short tk
                   (*^ pp_control_flow_instruction_short c*)
                   ^ ")"
                   ^ s)
             cfs)

(*****************************************************************************)
(*        pp control-flow graph                                              *)
(*****************************************************************************)

(* pp to dot a CFG.  Make a node for each non-{C_plain, C_branch}
   instruction, and an extra node for each ELF sumbol or other bl
   target.  *)

type node_kind_cfg =
  | CFG_node_source (* elf symbol or other bl target *)
  | CFG_node_branch_cond
  | CFG_node_branch_register
  | CFG_node_branch_and_link
  (*  | CFG_node_bl_noreturn*)
  | CFG_node_smc_hvc
  | CFG_node_ret
  | CFG_node_eret

type edge_kind_cfg = CFG_edge_flow | CFG_edge_correlate

type node_name = string (*graphviz node name*)

type node_cfg = node_name * node_kind_cfg * string (*label*) * addr * index

type edge_cfg = node_name * node_name * edge_kind_cfg

type graph_cfg =
  node_cfg list
  (* "source" nodes - elf symbols or other bl targets*)
  * node_cfg list
  (* interior nodes*)
  * edge_cfg list

(*edges*)

let mk_cfg node_name_prefix elf_symbols_array control_flow_insns_with_targets_array
    come_froms_array index_of_address : graph_cfg =
  (* the graphette source nodes are the addresses which are either
       - elf symbols
       - the branch target (but not the successor) of a C_branch_and_link
       - the targets (branch and fall-through) of a C_branch_cond, and/or
       - the targets of a C_branch_register *)
  let is_graphette_source_target (tk, addr, k, c, s) =
    match tk with
    | T_plain_successor -> false
    | T_branch -> false
    | T_branch_and_link_call -> true
    | T_branch_and_link_call_noreturn -> true
    | T_branch_and_link_successor -> false
    | T_branch_cond_branch -> false
    | T_branch_cond_successor -> false
    | T_branch_register -> false
    | T_smc_hvc_successor -> false
  in

  let is_graphette_source k =
    elf_symbols_array.(k) <> [] || List.exists is_graphette_source_target come_froms_array.(k)
  in

  let is_graph_non_source_node k =
    let (addr, i, c, targets) = control_flow_insns_with_targets_array.(k) in
    match c with
    | C_no_instruction -> false
    | C_plain -> false
    | C_ret -> true
    | C_eret -> true
    | C_branch (a, s) -> false
    | C_branch_and_link (a, s) -> true
    | C_smc_hvc s -> true
    | C_branch_cond _ -> true
    | C_branch_register _ -> true
  in

  (* we make up an additional node for all ELF symbols and bl targets; all others are just the address *)
  let node_name_source addr = node_name_prefix ^ "source_" ^ pp_addr addr in
  let node_name addr = node_name_prefix ^ pp_addr addr in

  let rec next_non_source_node_name visited k =
    let (addr, i, c, targets) = control_flow_insns_with_targets_array.(k) in
    match c with
    | C_plain -> (
        match targets with
        | [(tk, addr', k', s)] -> next_non_source_node_name visited k'
        | _ -> Warn.fatal "non-unique plain targets at %s" (pp_addr addr)
      )
    | C_branch (a, s) -> (
        match targets with
        | [(tk, addr', k', s)] ->
            if List.mem k' visited then node_name addr' (* TODO: something more useful *)
            else next_non_source_node_name (k' :: visited) k'
        | _ -> Warn.fatal "non-unique branch targets at %s" (pp_addr addr)
      )
    | _ -> node_name addr
  in

  let k_max = Array.length elf_symbols_array in

  (* need to track branch-visited edges because Hf loops back to a wfi (wait for interrupt)*)
  let graphette_source k : node_cfg list * edge_cfg list =
    let (addr, i, co, targets) = control_flow_insns_with_targets_array.(k) in
    let ss = elf_symbols_array.(k) in
    let (s, nn) =
      match ss with
      | [] -> (pp_addr addr, node_name addr)
      | _ -> (List.hd (List.rev ss), node_name_source addr)
    in
    let label = s in
    let node = (nn, CFG_node_source, label, addr, k) in
    let nn' = next_non_source_node_name [] k in
    let edge = (nn, nn', CFG_edge_flow) in
    ([node], [edge])
  in

  let sink_node addr s ckn k =
    let nn = node_name addr in
    let label = s in
    let node = (nn, CFG_node_ret, label, addr, k) in
    (*    let nn' = next_non_source_node_name [] k in
    let edge = (nn,nn') in*)
    ([node], [])
  in

  let simple_edge addr s ckn k =
    let nn = node_name addr in
    let label = s in
    let node = (nn, CFG_node_ret, label, addr, k) in
    let nn' = next_non_source_node_name [] k in
    let edge = (nn, nn', CFG_edge_flow) in
    ([node], [edge])
  in

  let graphette_normal k : node_cfg list * edge_cfg list =
    (*Printf.printf "gb k=%d\n a=%s" k (pp_addr (address_of_index k));flush stdout;*)
    let (addr, i, c, targets) = control_flow_insns_with_targets_array.(k) in
    match c with
    | C_no_instruction ->
        Warn.fatal0 "graphette_normal on C_no_instruction"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_plain ->
        Warn.fatal0 "graphette_normal on C_plain"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_branch _ ->
        Warn.fatal0 "graphette_normal on C_branch"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_ret -> sink_node addr "ret" CFG_node_ret k
    | C_eret -> sink_node addr "eret" CFG_node_eret k
    | C_branch_and_link (a, s) ->
        let nn = node_name addr in
        let label = s in
        let node = (nn, CFG_node_branch_and_link, label, addr, k) in
        let edges =
          List.filter_map
            (function
              | (T_branch_and_link_successor, a', k', s') ->
                  let nn' = next_non_source_node_name [] k' in
                  Some (nn, nn', CFG_edge_flow)
              | _ -> None)
            targets
        in
        ([node], edges)
    | C_smc_hvc s ->
        let nn = node_name addr in
        let label = "smc/hvc " ^ s in
        let node = (nn, CFG_node_smc_hvc, label, addr, k) in
        let edges =
          List.filter_map
            (function
              | (T_smc_hvc_successor, a', k', s') ->
                  let nn' = next_non_source_node_name [] k' in
                  Some (nn, nn', CFG_edge_flow)
              | _ -> None)
            targets
        in
        ([node], edges)
    | C_branch_cond (mnemonic, a, s) ->
        let nn = node_name addr in
        let label = pp_addr addr in
        let node = (nn, CFG_node_branch_cond, label, addr, k) in
        let edges =
          List.map
            (function
              | (tk, addr', k', s') ->
                  let nn' = next_non_source_node_name [] k' in
                  (nn, nn', CFG_edge_flow))
            targets
        in
        ([node], edges)
    | C_branch_register _ ->
        let nn = node_name addr in
        let label = pp_addr addr in
        let node = (nn, CFG_node_branch_register, label, addr, k) in
        let edges =
          List.sort_uniq compare
            (List.map
               (function
                 | (tk, addr', k', s') ->
                     let nn' = next_non_source_node_name [] k' in
                     (nn, nn', CFG_edge_flow))
               targets)
        in
        ([node], edges)
  in

  let rec mk_graph acc_nodes_source acc_nodes acc_edges n k =
    if k >= n then (acc_nodes_source, acc_nodes, acc_edges)
    else
      let (acc_nodes_source', acc_nodes', acc_edges') =
        if is_graphette_source k then
          let (nodes_source, edges) = graphette_source k in
          (nodes_source @ acc_nodes_source, acc_nodes, edges @ acc_edges)
        else (acc_nodes_source, acc_nodes, acc_edges)
      in
      let (acc_nodes_source'', acc_nodes'', acc_edges'') =
        if is_graph_non_source_node k then
          let (nodes, edges) = graphette_normal k in
          (acc_nodes_source', nodes @ acc_nodes', edges @ acc_edges')
        else (acc_nodes_source', acc_nodes', acc_edges')
      in
      mk_graph acc_nodes_source'' acc_nodes'' acc_edges'' n (k + 1)
  in
  let ((nodes_source, nodes, edges) as graph : graph_cfg) = mk_graph [] [] [] k_max 0 in

  graph

let pp_cfg ((nodes_source, nodes, edges) : graph_cfg) dot_file : unit =
  (*    let margin = "[margin=\"0.11,0.055\"]" in  (*graphviz default*) *)
  let margin = "[margin=\"0.03,0.02\"]" in
  (* let nodesep = "[nodesep=\"0.25\"]" in (*graphviz default *) *)
  let nodesep = "[nodesep=\"0.1\"]" in

  let pp_node_name nn = "\"" ^ nn ^ "\"" in
  let pp_edge (nn, nn', cek) =
    match cek with
    | CFG_edge_flow -> pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep ^ ";\n"
    | CFG_edge_correlate ->
        pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep
        ^ "[constraint=\"false\";style=\"dashed\"];\n"
  in

  let pp_node (nn, cnk, label, addr, k) =
    let shape =
      match cnk with CFG_node_branch_and_link | CFG_node_smc_hvc -> "[shape=\"box\"]" | _ -> ""
    in
    Printf.sprintf "%s [label=\"%s\"][tooltip=\"%s\"]%s%s;\n" (pp_node_name nn) label label margin
      shape
  in

  let c = open_out dot_file in
  Printf.fprintf c "digraph g {\n";
  Printf.fprintf c "rankdir=\"LR\";\n";
  List.iter (function node -> Printf.fprintf c "%s\n" (pp_node node)) nodes_source;
  Printf.fprintf c "{ rank=min; %s }\n"
    (String.concat ""
       (List.map (function (nn, _, _, _, _) -> pp_node_name nn ^ ";") nodes_source));
  List.iter (function node -> Printf.fprintf c "%s\n" (pp_node node)) nodes;
  List.iter (function e -> Printf.fprintf c "%s\n" (pp_edge e)) edges;
  Printf.fprintf c "}\n";
  let _ = close_out c in
  ()

let reachable_subgraph ((nodes_source, nodes, edges) : graph_cfg) (labels_start : string list) :
    graph_cfg =
  let nodes_all : node_cfg list = nodes_source @ nodes in
  let edges_all : (node_name * node_name list) list =
    List.map
      (function
        | (nn, cnk, label, addr, k) ->
            ( nn,
              List.filter_map
                (function (nn1, nn2, cek) -> if nn1 = nn then Some nn2 else None)
                edges ))
      nodes_all
  in

  let rec stupid_reachability (through_bl : bool) (acc_reachable : node_name list)
      (todo : node_name list) : node_name list =
    match todo with
    | [] -> acc_reachable
    | nn :: todo' ->
        if List.mem nn acc_reachable then stupid_reachability through_bl acc_reachable todo'
        else
          let new_nodes = List.assoc nn edges_all in
          (*          let new_nodes_bl = if through_bl && *)
          stupid_reachability through_bl (nn :: acc_reachable)
            ((*new_nodes_bl @ *) new_nodes @ todo')
  in
  let start_node_names =
    List.filter_map
      (function
        | (nn, cnk, label, addr, k) -> if List.mem label labels_start then Some nn else None)
      nodes_all
  in
  let node_names_reachable = stupid_reachability false [] start_node_names in
  let edges_reachable =
    List.filter
      (function
        | (nn, nn', cek) -> List.mem nn node_names_reachable && List.mem nn' node_names_reachable)
      edges
  in
  let nodes_reachable_source =
    List.filter
      (function (nn, cnk, label, addr, k) -> List.mem nn node_names_reachable)
      nodes_source
  in
  let nodes_reachable_rest =
    List.filter (function (nn, cnk, label, addr, k) -> List.mem nn node_names_reachable) nodes
  in
  (nodes_reachable_source, nodes_reachable_rest, edges_reachable)

let graph_union ((nodes_source, nodes, edges) : graph_cfg)
    ((nodes_source', nodes', edges') : graph_cfg) =
  (nodes_source @ nodes_source', nodes @ nodes', edges @ edges')

(*
module P = Graph.Pack
http://ocamlgraph.lri.fr/doc/Fixpoint.html
 *)

(* same-source-line edges *)

let correlate_source_line test1 graph1 test2 graph2 : graph_cfg =
  let (nodes_source1, nodes_rest1, edges1) = graph1 in
  let (nodes_source2, nodes_rest2, edges2) = graph2 in
  let is_branch_cond = function
    | (nn, CFG_node_branch_cond, label, addr, k) -> true
    | _ -> false
  in
  let nodes_branch_cond1 = List.filter is_branch_cond nodes_rest1 in
  let nodes_branch_cond2 = List.filter is_branch_cond nodes_rest2 in
  let with_source_lines test = function
    | (nn, cnk, label, addr, k) as n -> (nn, dwarf_source_file_line_numbers test addr)
  in
  let nodes_branch_cond_with1 = List.map (with_source_lines test1) nodes_branch_cond1 in
  let nodes_branch_cond_with2 = List.map (with_source_lines test2) nodes_branch_cond2 in
  let intersects xs ys = List.exists (function x -> List.mem x ys) xs in
  let edges =
    List.concat
      (List.map
         (function
           | (nn1, lines1) ->
               List.filter_map
                 (function
                   | (nn2, lines2) ->
                       if intersects lines1 lines2 then Some (nn1, nn2, CFG_edge_correlate)
                       else None)
                 nodes_branch_cond_with2)
         nodes_branch_cond_with1)
  in
  ([], [], edges)

(*****************************************************************************)
(*        call-graph                                                         *)
(*****************************************************************************)

type call_graph_node = addr * index * string list

let pp_call_graph test
    (control_flow_insns_with_targets_array, index_of_address, address_of_index, indirect_branches)
    =
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
    List.concat
      (List.map
         (function
           | (a, i, c, ts) ->
               let bl_targets =
                 List.filter
                   (function
                     | (tk', a', k', s') -> (
                         match tk' with
                         | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                         | _ -> false
                       ))
                   ts
               in
               List.filter_map
                 (function
                   | (tk', a', k', s') ->
                       if
                         not
                           (List.exists
                              (function (a'', ss'') -> Nat_big_num.equal a' a'')
                              elf_symbols)
                       then Some (a', ["FROM BL:" ^ s'])
                       else None)
                 bl_targets)
         (Array.to_list control_flow_insns_with_targets_array))
  in

  let rec dedup axs acc =
    match axs with
    | [] -> acc
    | (a, x) :: axs' ->
        if not (List.exists (function (a', x') -> Nat_big_num.equal a a') acc) then
          dedup axs' ((a, x) :: acc)
        else dedup axs' acc
  in

  let extra_bl_targets = dedup extra_bl_targets' [] in

  let nodes0 =
    List.sort
      (function
        | (a, ss) -> (
            function (a', ss') -> Nat_big_num.compare a a'
          ))
      (elf_symbols @ extra_bl_targets)
  in

  let nodes : call_graph_node list =
    List.map (function (a, ss) -> (a, index_of_address a, ss)) nodes0
  in

  let pp_node ((a, k, ss) as node) =
    pp_addr a (*" " ^ string_of_int k ^*) ^ " <" ^ String.concat ", " ss ^ ">"
  in

  let node_of_index k =
    match List.find_opt (function (a, k', ss) -> k' = k) nodes with
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
        else if not (k < Array.length control_flow_insns_with_targets_array) then
          stupid_reachability acc_reachable acc_bl_targets todo'
        else
          let (a, i, c, targets) = control_flow_insns_with_targets_array.(k) in
          let (bl_targets, non_bl_targets) =
            List.partition
              (function
                | (tk'', a'', k'', s'') -> (
                    match tk'' with
                    | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                    | _ -> false
                  ))
              targets
          in
          let bl_target_indices = List.map (function (tk'', a'', k'', s'') -> k'') bl_targets in
          let non_bl_target_indices =
            List.map (function (tk'', a'', k'', s'') -> k'') non_bl_targets
          in
          stupid_reachability (k :: acc_reachable)
            (List.sort_uniq compare (bl_target_indices @ acc_bl_targets))
            (non_bl_target_indices @ todo')
  in

  let bl_target_indices k =
    let (reachable, bl_target_indices) = stupid_reachability [] [] [k] in
    bl_target_indices
  in

  let call_graph =
    List.map
      (function (a, k, ss) as node -> (node, List.map node_of_index (bl_target_indices k)))
      nodes
  in

  let pp_call_graph_entry (n, ns) =
    pp_node n ^ ":\n" ^ String.concat "" (List.map (function n' -> "  " ^ pp_node n' ^ "\n") ns)
  in

  let pp_call_graph call_graph = String.concat "" (List.map pp_call_graph_entry call_graph) in

  let rec stupid_reachability' (acc_reachable : call_graph_node list)
      (todo : call_graph_node list) : call_graph_node list =
    match todo with
    | [] -> acc_reachable
    | ((a, k, ss) as n) :: todo' ->
        if List.exists (function (a', k', ss') -> k' = k) acc_reachable then
          stupid_reachability' acc_reachable todo'
        else
          let (_, targets) = List.find (function ((a', k', ss'), _) -> k' = k) call_graph in
          stupid_reachability' (n :: acc_reachable) (targets @ todo')
  in

  let transitive_call_graph =
    List.map
      (function
        | (a, k, ss) as n ->
            let (_, targets) = List.find (function ((a', k', ss'), _) -> k' = k) call_graph in
            (n, stupid_reachability' [] targets))
      nodes
  in

  let pp_transitive_call_graph transitive_call_graph =
    String.concat ""
      (List.map
         (function
           | (((a, k, ss) as n), ns) ->
               (if List.exists (function (a', k', ss') -> k' = k) ns then "RECURSIVE " else "")
               ^ "\n"
               ^ pp_call_graph_entry (n, ns))
         transitive_call_graph)
  in

  pp_call_graph call_graph ^ "*************** transitive call graph **************\n"
  ^ pp_transitive_call_graph transitive_call_graph

(*****************************************************************************)
(*        extract inlining data                                              *)
(*****************************************************************************)

let mk_inlining_array test instructions =
  (* compute the inlining data *)
  let iss = Dwarf.analyse_inlined_subroutines test.dwarf_static.ds_dwarf in
  let issr = Dwarf.analyse_inlined_subroutines_by_range iss in

  (* walk over instructions annotating with inlining data *)
  let rec f issr_current issr_rest label_last max_labels instructions acc =
    match instructions with
    | [] -> (List.rev_append acc [], max_labels)
    | (((addr : natural), (i : natural)) as instruction) :: instructions' ->
        let issr_still_current =
          List.filter
            (function (label, ((n1, n2), (m, n), is)) -> Nat_big_num.less addr n2)
            issr_current
        in

        let rec find_first discard p acc xs =
          match xs with
          | [] -> (List.rev_append acc [], xs)
          | x :: xs' ->
              if discard x then find_first discard p acc xs'
              else if p x then find_first discard p (x :: acc) xs'
              else (List.rev_append acc [], xs)
        in

        let (issr_starting_here0, issr_rest') =
          find_first
            (function ((n1, n2), (m, n), is) -> Nat_big_num.less_equal n2 addr)
            (function ((n1, n2), (m, n), is) -> Nat_big_num.equal n1 addr)
            [] issr_rest
        in

        let rec enlabel labels_in_use label_last acc issr_new =
          match issr_new with
          | [] -> (List.rev_append acc [], label_last)
          | issr :: issr_new' ->
              if List.length labels_in_use >= 26 then Warn.fatal "%s" "inlining depth > 26";
              let rec fresh_label l =
                let l = (l + 1) mod 26 in
                if not (List.mem l labels_in_use) then l else fresh_label l
              in
              let l = fresh_label label_last in
              enlabel (l :: labels_in_use) l ((l, issr) :: acc) issr_new'
        in

        let (issr_starting_here, label_last') =
          enlabel
            (List.map (function (label, _) -> label) issr_current)
            label_last [] issr_starting_here0
        in

        let issr_current' = issr_still_current @ issr_starting_here in

        let max_labels' = max max_labels (List.length issr_current') in

        let pp_label label = String.make 1 (Char.chr (label + Char.code 'a')) in

        let ppd_labels =
          String.concat "" (List.map (function (label, _) -> pp_label label) issr_current')
        in

        let ppd_new_inlining =
          String.concat ""
            (List.map
               (function
                 | (label, x) ->
                     pp_label label ^ ": "
                     ^ Dwarf.pp_inlined_subroutines_by_range test.dwarf_static [x])
               issr_starting_here)
        in

        let acc' = (ppd_labels, ppd_new_inlining) :: acc in

        f issr_current' issr_rest' label_last' max_labels' instructions' acc'
  in

  let (inlining_list, max_labels) = f [] issr 25 0 instructions [] in
  let inlining_array = Array.of_list inlining_list in

  let pp_inlining_label_prefix s = s ^ String.make (max_labels - String.length s) ' ' ^ " " in

  (inlining_array, pp_inlining_label_prefix)

(*****************************************************************************)
(*        collect test analysis                                              *)
(*****************************************************************************)

type analysis = {
  text_addr : addr;
  index_of_address : addr -> int;
  address_of_index : int -> addr;
  instructions : (addr * instruction) list;
  size : int;
  elf_symbols_array : string list array;
  objdump_lines_array : (addr (*address*) * natural (*insn/data*) * string) option array;
  frame_info_array :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array;
  control_flow_insns_with_targets_array :
    (addr * instruction * control_flow_insn * (target_kind * addr * index * string) list) array;
  indirect_branches :
    (addr * instruction * control_flow_insn * (target_kind * addr * index * string) list) list;
  come_froms_array : (target_kind * addr * index * control_flow_insn * string) list array;
  inlining_array : (string (*ppd_labels*) * string) (*new inlining*) array;
  pp_inlining_label_prefix : string -> string;
}

let analyse_test test filename_objdump_d filename_branch_table =
  (* pull out instructions from text section, assuming 4-byte insns *)
  let (p, text_addr, bs) = Dwarf.extract_text test.elf_file in
  let instructions : (addr * instruction) list = Dwarf.words_of_byte_list text_addr bs [] in
  let index_of_address (addr : addr) : int =
    Nat_big_num.to_int (Nat_big_num.sub addr text_addr) / 4
  in
  let address_of_index (i : int) : addr =
    Nat_big_num.add text_addr (Nat_big_num.of_int (i * 4))
  in

  (* hack to cut down problem size for quick experimentation *)
  let rec first n xs =
    if n = 0 then []
    else match xs with x :: xs' -> x :: first (n - 1) xs' | _ -> Warn.fatal "first %d" n
  in
  let instructions = if !Globals.clip_binary then first 1000 instructions else instructions in
  let size = List.length instructions in

  (*
  Printf.printf "instructions=%d unique instructions=%d\n"  (List.length instructions) (List.length (List.sort_uniq compare (List.map (function (a,i)->Nat_big_num.to_int i) instructions))); exit 1;
  *)
  let objdump_lines_array =
    mk_objdump_lines_array (parse_objdump_file filename_objdump_d) instructions
  in

  let elf_symbols_array = mk_elf_symbols_array test instructions in

  let frame_info_array = mk_frame_info_array test instructions in

  (* compute the basic control-flow data *)
  let control_flow_insns_with_targets_array =
    mk_control_flow_insns_with_targets_array test instructions objdump_lines_array
      index_of_address address_of_index filename_branch_table size
  in
  let indirect_branches = mk_indirect_branches control_flow_insns_with_targets_array in
  let come_froms_array = mk_come_from_array control_flow_insns_with_targets_array in

  (* compute the inlining data *)
  let (inlining_array, pp_inlining_label_prefix) = mk_inlining_array test instructions in

  let an =
    {
      text_addr;
      index_of_address;
      address_of_index;
      instructions;
      size;
      elf_symbols_array;
      objdump_lines_array;
      frame_info_array;
      control_flow_insns_with_targets_array;
      indirect_branches;
      come_froms_array;
      inlining_array;
      pp_inlining_label_prefix;
    }
  in

  an

(*****************************************************************************)
(*        pretty-print one instruction                                       *)
(*****************************************************************************)

(* plumbing to print diffs from one instruction to the next *)
let last_frame_info = ref ""

let last_var_info = ref []

let last_source_info = ref ""

let pp_instruction_init () =
  last_frame_info := "";
  last_var_info := [];
  last_source_info := ""

let pp_instruction test an ((addr : natural), (i : natural)) =
  (* the come_froms for this address, calculated first to determine whether this is the start of a basic block *)
  let k = an.index_of_address addr in
  let come_froms' =
    List.filter
      (function (tk, a', k', c', s') -> tk <> T_plain_successor)
      an.come_froms_array.(k)
  in
  let (ppd_labels, ppd_new_inlining) = an.inlining_array.(k) in

  (* the elf symbols at this address, if any (and reset the last_var_info if any) *)
  let elf_symbols = an.elf_symbols_array.(k) in
  (match elf_symbols with [] -> () | _ -> last_var_info := []);

  (if come_froms' <> [] || elf_symbols <> [] then "\n" else "")
  ^ String.concat "" (List.map (fun (s : string) -> pp_addr addr ^ " <" ^ s ^ ">:\n") elf_symbols)
  (* the new inlining info for this address *)
  ^ ppd_new_inlining
  (* the source file lines (if any) associated to this address *)
  ^ begin
      if !Globals.show_source then
        let source_info =
          match pp_dwarf_source_file_lines () test.dwarf_static true addr with
          | Some s ->
              (* the inlining label prefix *)
              an.pp_inlining_label_prefix ppd_labels ^ s ^ "\n"
          | None -> ""
        in
        if source_info = !last_source_info then "" (*"unchanged\n"*)
        else (
          last_source_info := source_info;
          source_info
        )
      else ""
    end
  (* the frame info for this address *)
  ^ begin
      if !Globals.show_cfa then
        let frame_info = pp_frame_info an.frame_info_array k in
        if frame_info = !last_frame_info then "" (*"CFA: unchanged\n"*)
        else (
          last_frame_info := frame_info;
          (* the inlining label prefix *)
          an.pp_inlining_label_prefix ppd_labels ^ frame_info
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
  (* the inlining label prefix *)
  ^ an.pp_inlining_label_prefix ppd_labels
  (* the address and (hex) instruction *)
  ^ pp_addr addr
  ^ ":  " ^ pp_addr i
  (* the dissassembly from objdump, if it exists *)
  ^ "  "
  ^ begin
      match an.objdump_lines_array.(k) with
      | Some (a', i', s) ->
          if i = i' then s
          else
            Warn.fatal2 "instruction mismatch - linksem: %s vs objdump: %s\n" (pp_addr i)
              (pp_addr i')
      | None -> ""
    end
  ^ begin
      match
        List.find_opt (function (a, i, c, ts) -> Nat_big_num.equal a addr) an.indirect_branches
      with
      | Some (a, i, c, ts) ->
          " -> "
          ^ String.concat ","
              (List.map
                 (function (tk, a', k', s) -> pp_target_addr_wrt addr c a' ^ "" ^ s ^ "")
                 ts)
          ^ " "
      | None -> ""
    end
  ^ pp_come_froms addr come_froms' ^ "\n"

(*****************************************************************************)
(*        pretty-print test analysis                                         *)
(*****************************************************************************)

let pp_test_analysis test an =
  "************** aggregate type definitions *****************\n"
  ^ (let d = test.dwarf_static.ds_dwarf in
     let c = Dwarf.p_context_of_d d in
     Dwarf.pp_all_aggregate_types c d)
  ^ "\n************** instructions *****************\n"
  ^ ( pp_instruction_init ();
      String.concat "" (List.map (pp_instruction test an) an.instructions)
    )
  (*  ^ "\n************** branch targets *****************\n"*)
  (*  ^ pp_branch_targets control_flow_insns_with_targets_array*)
  ^ "\n************** call graph *****************\n"
  ^ pp_call_graph test
      ( (*control_flow_insns_with_targets,*)
        an.control_flow_insns_with_targets_array,
        an.index_of_address,
        an.address_of_index,
        an.indirect_branches )

(*****************************************************************************)
(*        top-level                                                          *)
(*****************************************************************************)

let process_file () : unit =
  (*filename_objdump_d filename_branch_tables (filename_elf : string) : unit =*)

  (* todo: make idiomatic Cmdliner :-(  *)
  let filename_elf =
    match !Globals.elf with Some s -> s | None -> Warn.fatal0 "no --elf option\n"
  in

  let filename_objdump_d =
    match !Globals.objdump_d with Some s -> s | None -> Warn.fatal0 "no --objdump-d option\n"
  in

  let filename_branch_tables =
    match !Globals.branch_table_data_file with
    | Some s -> s
    | None -> Warn.fatal0 "no --branch-tables option\n"
  in

  let filename_out_file_option = !Globals.out_file in

  (* try caching linksem output - though linksem only takes 5s, so scarcely worth the possible confusion. It's recomputing the variable info that takes the time *)
  (*
  let filename_marshalled = filename ^ ".linksem-marshalled" in
  let test =
    match marshal_from_file filename_marshalled with
    | None ->
       let test = parse_elf_file filename in
       marshal_to_file filename_marshalled test;
       test
    | Some test ->
       test
  in
   *)
  let test = parse_elf_file filename_elf in

  let an = analyse_test test filename_objdump_d filename_branch_tables in

  match (!Globals.elf2, !Globals.objdump_d2, !Globals.branch_table_data_file2) with
  | (None, _, _) -> (
      (* output CFG dot file *)
      ( match !Globals.dot_file with
      | Some dot_file ->
          let graph =
            mk_cfg "" an.elf_symbols_array an.control_flow_insns_with_targets_array
              an.come_froms_array an.index_of_address
          in
          (*            let graph' = reachable_subgraph graph ["mpool_fini"] in*)
          pp_cfg graph dot_file
      | None -> ()
      );

      (* output annotated objdump *)
      let c = match filename_out_file_option with Some f -> open_out f | None -> stdout in

      (* copy emacs syntax highlighting blob to output. sometime de-hard-code the filename*)
      begin
        match read_file_lines "emacs-highlighting" with
        | MyFail _ -> ()
        | Ok lines -> Array.iter (function s -> Printf.fprintf c "%s\n" s) lines
      end;

      Printf.fprintf c "%s" (pp_test_analysis test an);

      match filename_out_file_option with Some f -> close_out c | None -> ()
    )
  | (Some filename_elf2, Some filename_objdump_d2, Some filename_branch_tables2) -> (
      match !Globals.dot_file with
      | Some dot_file ->
          let test2 = parse_elf_file filename_elf2 in

          let an2 = analyse_test test2 filename_objdump_d2 filename_branch_tables2 in

          let graph0 =
            mk_cfg "O0_" an.elf_symbols_array an.control_flow_insns_with_targets_array
              an.come_froms_array an.index_of_address
          in

          let graph2 =
            mk_cfg "O2_" an2.elf_symbols_array an2.control_flow_insns_with_targets_array
              an2.come_froms_array an2.index_of_address
          in

          let graph0' =
            reachable_subgraph graph0
              ["mpool_fini"; "mpool_lock"; "mpool_free"; "mpool_add_chunk"; "mpool_unlock"]
          in

          let graph2' = reachable_subgraph graph2 ["mpool_fini"] in

          let graph = graph_union graph0' graph2' in

          let graph' = graph_union graph (correlate_source_line test graph0' test2 graph2') in

          pp_cfg graph' dot_file
      | None -> Warn.fatal0 "no dot file\n"
    )
  | _ -> Warn.fatal0 "missing files for elf2\n"
