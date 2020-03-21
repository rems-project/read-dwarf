open Logs.Logger (struct
  let str = "Analyse"
end)

open Printf

type natural = Nat_big_num.num

let measure_time = false

let time s f x =
  if measure_time then (
    let t1 : Unix.process_times = Unix.times () in
    let y = f x in
    let t2 : Unix.process_times = Unix.times () in
    Printf.printf "time %s user %6.3f  system %6.3f\n" s (t2.tms_utime -. t1.tms_utime)
      (t2.tms_stime -. t1.tms_stime);
    y
  )
  else f x

(*****************************************************************************)
(* collected data from linksem                                               *)
(*****************************************************************************)

(* TODO: this should include the simple_die_tree representation, and the variable info should be calculated in terms of that instead of directly *)

(* architectures from linksem elf_header.lem *)
type architecture = AArch64 (* ARM 64-bit architecture (AARCH64), elf_ma_aarch64 = 183*) | X86

(* AMD x86-64 architecture,  elf_ma_x86_64 = 62 *)

type test = {
  elf_file : Elf_file.elf_file;
  arch : architecture;
  symbol_map : Elf_file.global_symbol_init_info;
  segments : Elf_interpreted_segment.elf64_interpreted_segment list;
  e_entry : natural;
  e_machine : natural;
  dwarf_static : Dwarf.dwarf_static;
  dwarf_semi_pp_frame_info :
    (natural (*address*) * string (*cfa*) * (string * string) (*register rules*) list) list;
}

(*****************************************************************************)
(* control-flow abstraction of instructions, from objdump and branch table data *)
(*****************************************************************************)

type addr = natural

type index = int (* index into instruction-indexed arrays *)

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

type target = target_kind * addr * index * string

type instruction = {
  i_addr : addr;
  i_opcode : int list;
  i_mnemonic : string;
  i_operands : string;
  i_control_flow : control_flow_insn;
  i_targets : target list;
}

type come_from = {
  cf_target_kind : target_kind;
  cf_addr : addr;
  cf_index : index;
  cf_control_flow : control_flow_insn;
  cf_desc : string;
}

(*****************************************************************************)
(*        variable info per-instruction (and global)                         *)
(*****************************************************************************)

type ranged_var =
  (natural * natural * Dwarf.operation list)
  * (Dwarf.sdt_variable_or_formal_parameter * string list)

type ranged_vars_at_instructions = {
  rvai_globals : (Dwarf.sdt_variable_or_formal_parameter * string list) list;
  rvai_params :
    (addr * (string (* function name *) * Dwarf.sdt_variable_or_formal_parameter list)) list;
  rvai_current : ranged_var list array;
  rvai_new : ranged_var list array;
  rvai_old : ranged_var list array;
  rvai_remaining : ranged_var list array;
}

(*****************************************************************************)
(*        post-processed DWARF source line info                              *)
(*****************************************************************************)

type evaluated_line_info_sequence = {
  elis_first : addr;
  elis_last : addr;
  (* the address of the end_sequence - half-open interval *)
  elis_lnh : Dwarf.line_number_header;
  elis_lines : Dwarf.line_number_registers list;
}

type evaluated_line_info_entry = {
  elie_first : addr;
  elie_last : addr;
  (* closed interval *)
  elie_lnh : Dwarf.line_number_header;
  elie_lnr : Dwarf.line_number_registers;
}

type evaluated_line_info_for_instruction = {
  elifi_start : bool;
  elifi_entry : evaluated_line_info_entry;
}

(*****************************************************************************)
(*        collect test analysis                                              *)
(*****************************************************************************)

type analysis = {
  index_of_address : addr -> int;
  address_of_index : int -> addr;
  instructions : instruction array;
  line_info : evaluated_line_info_for_instruction list array;
  elf_symbols : string list array;
  (*  objdump_lines : (addr (*address*) * natural (*insn/data*) * string) option array;*)
  frame_info :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array;
  indirect_branches : instruction list;
  come_froms : come_from list array;
  sdt : Dwarf.sdt_dwarf;
  ranged_vars_at_instructions : ranged_vars_at_instructions;
  inlining : (string (*ppd_labels*) * string) (*new inlining*) array;
  pp_inlining_label_prefix : string -> string;
  rendered_control_flow : string array;
  rendered_control_flow_inbetweens : string array;
  rendered_control_flow_width : int;
}

(*****************************************************************************)
(*        misc                                                               *)
(*****************************************************************************)

let pp_addr (a : natural) = Ml_bindings.hex_string_of_big_int_pad8 a

let rec list_last xs =
  match xs with
  | [x] -> x
  | x :: (x' :: xs' as xs'') -> list_last xs''
  | _ -> raise (Failure "list_last")

let rec list_last_opt xs =
  match xs with [x] -> Some x | x :: (x' :: xs' as xs'') -> list_last_opt xs'' | _ -> None

let char_list_of_string s =
  let n = String.length s in
  let rec f i = if i = n then [] else s.[i] :: f (i + 1) in
  f 0

let array_indices_such_that (f : 'a -> bool) (a : 'array) : int list =
  let len = Array.length a in
  let rec g acc k =
    if k >= len then acc else if f a.(k) then g (k :: acc) (k + 1) else g acc (k + 1)
  in
  g [] 0

(* in 4.10 *)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l
    )

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
      (* architectures from linksem elf_header.lem *)
      let arch =
        if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_aarch64 then AArch64
        else if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_x86_64 then X86
        else Warn.fatal "unrecognised ELF file architecture"
      in

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
          arch;
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
(*        post-process DWARF source line info                                *)
(*****************************************************************************)

(* line number sequences can overlap, and we have to walk through instructions (not addresses), so we simplify by splitting all of them into individual entries, sort them by first address, and then walk through them painting a per-instruction array.  This is algorithmically a bit terrible, but seems to add only a couple of seconds to read-dwarf rd *)

let pp_line_number_header_concise (lnh : Dwarf.line_number_header) : string =
  "lnh offset =    " ^ Dwarf.pphex lnh.lnh_offset ^ "\n"

(*^ ("dwarf_format =                       " ^ (pp_dwarf_format lnh.lnh_dwarf_format ^ ("\n"
^ ("unit_length =                        " ^ (Nat_big_num.to_string lnh.lnh_unit_length ^ ("\n"
^ ("version =                            " ^ (Nat_big_num.to_string lnh.lnh_version ^ ("\n"
^ ("header_length =                      " ^ (Nat_big_num.to_string lnh.lnh_header_length ^ ("\n"
^ ("minimum_instruction_length =         " ^ (Nat_big_num.to_string lnh.lnh_minimum_instruction_length ^ ("\n"
^ ("maximum_operations_per_instruction = " ^ (Nat_big_num.to_string lnh.lnh_maximum_operations_per_instruction ^ ("\n"
^ ("default_is_stmt =                    " ^ (string_of_bool lnh.lnh_default_is_stmt ^ ("\n"
^ ("line_base =                          " ^ (Nat_big_num.to_string lnh.lnh_line_base ^ ("\n"
^ ("line_range =                         " ^ (Nat_big_num.to_string lnh.lnh_line_range ^ ("\n"
^ ("opcode_base =                        " ^ (Nat_big_num.to_string lnh.lnh_opcode_base ^ ("\n"
^ ("standard_opcode_lengths =            " ^ (string_of_list 
  instance_Show_Show_Num_natural_dict lnh.lnh_standard_opcode_lengths ^ ("\n"
^ ("comp_dir =                           " ^ (string_of_maybe 
  instance_Show_Show_string_dict lnh.lnh_comp_dir ^ ("\n"
^ ("include_directories =                " ^ (Lem_string.concat ", " (Lem_list.map string_of_bytes  lnh.lnh_include_directories) ^ ("\n"
^ ("file_entries =                   \n\n" ^ (Lem_string.concat "\n" (Lem_list.map pp_line_number_file_entry  lnh.lnh_file_entries) ^ "\n"))))))))))))))))))))))))))))))))))))))))))))
 *)

let pp_sequence_concise (s : evaluated_line_info_sequence) =
  "first: " ^ pp_addr s.elis_first ^ "\n" ^ "last:  " ^ pp_addr s.elis_last ^ "\n"
  ^ pp_line_number_header_concise s.elis_lnh
  ^ Dwarf.pp_line_number_registerss s.elis_lines

let pp_elie_concise (elie : evaluated_line_info_entry) =
  "first: " ^ pp_addr elie.elie_first ^ " " ^ "last: " ^ pp_addr elie.elie_last ^ "\n"
  ^ pp_line_number_header_concise elie.elie_lnh

(*  ^ Dwarf.pp_line_number_registerss s.elis_lines*)

let split_into_sequences
    ((lnh : Dwarf.line_number_header), (lnrs : Dwarf.line_number_registers list)) :
    evaluated_line_info_sequence list =
  let rec f (acc1 : Dwarf.line_number_registers list) (acc2 : evaluated_line_info_sequence list)
      (lnrs : Dwarf.line_number_registers list) =
    match lnrs with
    | [] -> (
        match acc1 with
        | [] -> List.rev acc2
        | _ -> Warn.fatal "split_into_sequences found premature end of sequence"
      )
    | lnr :: lnrs' ->
        if lnr.lnr_end_sequence then (
          let first =
            match list_last_opt acc1 with
            | Some lnr_first -> lnr_first.lnr_address
            | None -> Warn.fatal "split_into_sequences found sequence of length 0"
          in
          let last = lnr.lnr_address in
          if Nat_big_num.equal first last then Warn.fatal "split_into_sequences found first=last"
          else ();
          let elis =
            {
              elis_first = first;
              elis_last = last;
              elis_lnh = lnh;
              elis_lines = List.rev (lnr :: acc1);
            }
          in
          f [] (elis :: acc2) lnrs'
        )
        else f (lnr :: acc1) acc2 lnrs'
  in
  f [] [] lnrs

let split_into_entries (s : evaluated_line_info_sequence) : evaluated_line_info_entry list =
  let rec f acc (remaining_lines : Dwarf.line_number_registers list) =
    match remaining_lines with
    | l1 :: (l2 :: remaining_lines' as remaining_lines'') ->
        let elie =
          {
            elie_first = l1.lnr_address;
            elie_last =
              ( if Nat_big_num.equal l2.lnr_address l1.lnr_address then l1.lnr_address
              else Nat_big_num.sub l2.lnr_address (Nat_big_num.of_int 1)
              );
            elie_lnh = s.elis_lnh;
            elie_lnr = l1;
          }
        in
        f (elie :: acc) remaining_lines''
    | _ -> List.rev acc
  in
  f [] s.elis_lines

(*           
        
let mk_line_info (eli: Dwarf.evaluated_line_info) instructions : evaluated_line_info_for_instruction option array =
  let sequences = List.flatten (List.map split_into_sequences eli) in
  let compare_sequence s1 s2 = Nat_big_num.compare s1.elis_first s2.elis_first in
  let sequences_sorted = List.sort compare_sequence sequences in
  (*let overlap_sequence s1 s2 = not( Nat_big_num.greater_equal s2.first s1.last || Nat_big_num.greater_equal s1.first s2.last) in*)

  Printf.printf "mk_line_info\n%s" (String.concat "\n" (List.map pp_sequence_concise sequences_sorted));
  
  let size = Array.length instructions in
  let elifis = Array.make size None in

  let next_sequence addr remaining_sequences : ((evaluated_line_info_sequence list) * (evaluated_line_info_sequence list)) =
    let (discardable,remaining') = 
      List.partition
        (function sequence -> 
           Nat_big_num.less_equal sequence.elis_last addr)
        remaining_sequences in
    let (sequences,remaining'') = 
      List.partition
        (function sequence -> 
           Nat_big_num.less_equal sequence.elis_first addr)
        remaining' in
    (sequences,remaining'') in
  
  let rec g remaining_sequences k = 
    if k>= size then ()
    else
      let addr = instructions.(k).i_addr in
      match next_sequence addr remaining_sequences with
      | ([sequence'],remaining_sequences'') ->
         (* found one - start trying that sequence *)
         f sequence' sequence'.elis_lines remaining_sequences'' k
      | ([],[]) -> 
         (* no sequences for this address and no remaining sequences - stop *)
         ()
      | ([],remaining_sequences'') -> 
         (* no sequences for this address - continue with the next address, discarding now-irrelevant sequences *)
         g remaining_sequences'' (k+1)
      | ((sequence1::sequence2::_ as sequences),remaining_sequences'') ->
         (* multiple sequences covering this address - hope that shouldn't happen in well-formed DWARF *)
         Warn.nonfatal "multiple sequences found for %d:\n%s" k (String.concat "\n" (List.map pp_sequence_concise sequences));
         (* just discard all except the first *)
         f sequence1 sequence1.elis_lines remaining_sequences'' k 
       
  and f active_sequence remaining_lines remaining_sequences k =
    if k>= size then ()
    else
      let addr = instructions.(k).i_addr in
      match remaining_lines with
      | l1::((l2::remaining_lines') as remaining_lines'') ->
         if Nat_big_num.equal addr l1.lnr_address then
           (* this instruction address exactly matches the first line of this sequence *)
           let elifi = {
               elifi_start = true;
               elifi_lnh = active_sequence.elis_lnh;
               elifi_line = l1 } in
           elifis.(k) <- Some elifi;
           f active_sequence remaining_lines remaining_sequences (k+1)
         else if Nat_big_num.less l1.lnr_address addr && Nat_big_num.less addr l2.lnr_address then 
           (* this instruction address is within the range of the first line, but not equal to it*)
           let elifi = {
               elifi_start = false;
               elifi_lnh = active_sequence.elis_lnh;
               elifi_line = l1 } in
           elifis.(k) <- Some elifi;
           f active_sequence remaining_lines remaining_sequences (k+1)
         else if Nat_big_num.greater_equal addr l2.lnr_address then
           (* this instruction address is after the range of the first line *)
           if not(l2.lnr_end_sequence (* invariant: iff remaining'=[]*)) then
             (* there are more non-end lines left in this sequence: try again with the next *)
             f active_sequence remaining_lines'' remaining_sequences k
           else
             (* try for the next sequence *)
             g remaining_sequences k

         else
           (* this instruction address is before the range of the first line *)
           (* so skip this instruction and try the next *)
           f active_sequence remaining_lines remaining_sequences (k+1) 
  in
  g sequences_sorted 0  ;
  elifis


NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW 
 *)

let mk_line_info (eli : Dwarf.evaluated_line_info) instructions :
    evaluated_line_info_for_instruction list array =
  let size = Array.length instructions in
  (*  let elies = Array.make size [] in*)
  let elifis = Array.make size [] in

  let sequences = List.flatten (List.map split_into_sequences eli) in
  let compare_sequence s1 s2 = Nat_big_num.compare s1.elis_first s2.elis_first in
  let sequences_sorted = List.sort compare_sequence sequences in

  let entries = List.flatten (List.map split_into_entries sequences_sorted) in
  let compare_entry e1 e2 = Nat_big_num.compare e1.elie_first e2.elie_first in
  let entries_sorted = List.sort compare_entry entries in

  (*List.iter (function elie -> Printf.printf "%s" (pp_elie_concise elie)) entries_sorted;*)
  let rec f active_entries remaining_entries k =
    if k >= size then ()
    else
      let addr = instructions.(k).i_addr in

      let rec mk_new_perhaps_relevant acc remaining =
        match remaining with
        | [] -> (acc, remaining)
        | elie :: remaining' ->
            if Nat_big_num.less_equal elie.elie_first addr then
              mk_new_perhaps_relevant (elie :: acc) remaining'
            else (acc, remaining)
      in

      let (new_perhaps_relevant, remaining') = mk_new_perhaps_relevant [] remaining_entries in

      let addr_in elie =
        Nat_big_num.less_equal elie.elie_first addr && Nat_big_num.less_equal addr elie.elie_last
      in

      let still_active_entries =
        List.filter addr_in active_entries @ List.filter addr_in new_perhaps_relevant
      in

      elifis.(k) <-
        List.map
          (function
            | elie ->
                let elifi =
                  { elifi_start = Nat_big_num.equal addr elie.elie_first; elifi_entry = elie }
                in
                elifi)
          still_active_entries;

      f still_active_entries remaining' (k + 1)
  in

  f [] entries_sorted 0;
  elifis

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
          Warn.nonfatal "filename %s %s" filename s;
          None
    )

let pp_source_line so column =
  let marker = (*pp_glyph Gud B*) "\u{2551}" in
  match so with
  | Some s ->
      let len = String.length s in
      if column <= 1 then marker ^ s
      else if column < len - 1 then
        String.sub s 0 (column - 1) ^ marker ^ String.sub s (column - 1) (len - column + 1)
        (*" (" ^ s ^ ")"*)
      else s
  | None -> "file not found"

(* OLD source file lines 
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
                  pp_source_line (source_line (comp_dir', dir, file) (Nat_big_num.to_int n)) 200
                else "")
              sls))
           *)

let mk_subprogram_name (ds : Dwarf.dwarf_static) elifi : string =
  let lnh = elifi.elifi_entry.elie_lnh in
  let lnr = elifi.elifi_entry.elie_lnr in
  let ((comp_dir, dir, file), subprogram_name) =
    let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
    (ufe, Dwarf.subprogram_at_line ds.ds_subprogram_line_extents ufe lnr.lnr_line)
  in
  subprogram_name

let pp_dwarf_source_file_lines' (ds : Dwarf.dwarf_static) (pp_actual_line : bool) multiple elifi :
    string =
  let lnh = elifi.elifi_entry.elie_lnh in
  let lnr = elifi.elifi_entry.elie_lnr in
  let ((comp_dir, dir, file), subprogram_name) =
    let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
    (ufe, Dwarf.subprogram_at_line ds.ds_subprogram_line_extents ufe lnr.lnr_line)
  in
  let comp_dir' =
    match !Globals.comp_dir with
    | None -> comp_dir
    | Some comp_dir'' -> (
        match comp_dir with
        | None -> Some comp_dir''
        | Some s -> Some (Filename.concat comp_dir'' s)
      )
  in
  file ^ ":"
  ^ Nat_big_num.to_string lnr.lnr_line
  ^ "."
  ^ Nat_big_num.to_string lnr.lnr_column
  ^ " (" ^ subprogram_name ^ ")"
  (*  ^ (if elifi.elifi_start then "S" else "s")*)
  ^ (if lnr.lnr_is_stmt then "S" else "s")
  ^ (if lnr.lnr_basic_block then "B" else "b")
  ^ (if lnr.lnr_end_sequence then "E" else "e")
  ^ (if lnr.lnr_prologue_end then "P" else "p")
  ^ (if lnr.lnr_epilogue_begin then "E" else "e")
  ^ (if multiple then "M" else " ")
  ^ " "
  ^
  if pp_actual_line then
    let line = Nat_big_num.to_int lnr.lnr_line in
    if line = 0 then "line 0"
    else
      pp_source_line (source_line (comp_dir', dir, file) line) (Nat_big_num.to_int lnr.lnr_column)
  else ""

(* OLD source line number for O0/2 correlation
(* source line info for matching instructions between binaries - ignoring inlining for now, and supposing there is always a predecessor with a source line. Should pay more careful attention to the actual line number table *)
let rec dwarf_source_file_line_numbers' test recursion_limit (a : natural) :
    (string (*subprogram name*) * int) (*line number*) list =
  if recursion_limit = 0 then []
  else
    let sls = Dwarf.source_lines_of_address test.dwarf_static a in
    match sls with
    | [] ->
        dwarf_source_file_line_numbers' test (recursion_limit - 1)
          (Nat_big_num.sub a (Nat_big_num.of_int 4))
    | _ ->
        List.map
          (fun ((comp_dir, dir, file), n, lnr, subprogram_name) ->
            (subprogram_name, Nat_big_num.to_int n))
          sls

let dwarf_source_file_line_numbers test (a : natural) =
  dwarf_source_file_line_numbers' test 100 (a : natural)

  *)

let dwarf_source_file_line_numbers_by_index test line_info k :
    (string (*subprogram name*) * int) (*line number*) list =
  let elifis = line_info.(k) in
  let lines =
    List.sort_uniq compare
      (List.map
         (function
           | elifi ->
               let lnh = elifi.elifi_entry.elie_lnh in
               let lnr = elifi.elifi_entry.elie_lnr in
               let ((comp_dir, dir, file), subprogram_name) =
                 let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
                 ( ufe,
                   Dwarf.subprogram_at_line test.dwarf_static.ds_subprogram_line_extents ufe
                     lnr.lnr_line )
               in
               (subprogram_name, Nat_big_num.to_int lnr.lnr_line))
         elifis)
  in
  match lines with [_] -> lines | [] -> lines | _ -> []

(*****************************************************************************)
(*        look up address in ELF symbol table                                *)
(*****************************************************************************)

let elf_symbols_of_address (test : test) (addr : natural) : string list =
  List.filter_map
    (fun (name, (typ, size, address, mb, binding)) -> if address = addr then Some name else None)
    test.symbol_map

let mk_elf_symbols test instructions : string list array =
  Array.map (function i -> elf_symbols_of_address test i.i_addr) instructions

let address_of_elf_symbol test (s : string) : addr option =
  find_map
    (fun (name, (typ, size, address, mb, binding)) -> if s = name then Some address else None)
    test.symbol_map

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

let mk_frame_info test instructions :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array =
  Array.map (function i -> f aof i.i_addr None test.dwarf_semi_pp_frame_info) instructions

let pp_frame_info frame_info k : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match frame_info.(k) with
  | None -> "<no frame info for this address>\n"
  | Some ((a : natural), (cfa : string), (regs : (string * string) list)) ->
      pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " "
      ^ String.concat " " (List.map (function (rname, rinfo) -> rname ^ ":" ^ rinfo) regs)
      ^ "\n"

(*****************************************************************************)
(* basic pp for control-flow  abstraction                                    *)
(*****************************************************************************)

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

(*****************************************************************************)
(*    find targets of each entry of a branch-table description file          *)
(*****************************************************************************)

let branch_table_target_addresses test filename_branch_table : (addr * addr list) list =
  (* read in and parse branch-table description file *)
  let branch_data :
      (natural (*a_br*) * (natural (*a_table*) * natural (*size*) * string (*shift*) * natural))
      (*offset*)
      list =
    match read_file_lines filename_branch_table with
    | MyFail s ->
        Warn.fatal "%s\ncouldn't read branch table data file: \"%s\"\n" s filename_branch_table
    | Ok lines ->
        let parse_line (s : string) : (natural * (natural * natural * string * natural)) option =
          match
            Scanf.sscanf s " %x: %x %x %s %x #" (fun a_br a_table n shift a_offset ->
                (a_br, a_table, n, shift, a_offset))
          with
          | (a_br, a_table, n, shift, a_offset) ->
              Some
                ( Nat_big_num.of_int a_br,
                  ( Nat_big_num.of_int a_table,
                    Nat_big_num.of_int n,
                    shift,
                    Nat_big_num.of_int a_offset ) )
          | exception _ -> Warn.fatal "couldn't parse branch table data file line: \"%s\"\n" s
        in
        List.filter_map parse_line (List.tl (Array.to_list lines))
  in

  (* pull out .rodata section from ELF *)
  let ((c, rodata_addr, bs) as rodata : Dwarf.p_context * Nat_big_num.num * char list) =
    Dwarf.extract_section_body test.elf_file ".rodata" false
  in
  (* chop into bytes *)
  let rodata_bytes : char array = Array.of_list bs in

  (* chop into 4-byte words - as needed for branch offset tables,
     though not for all other things in .rodata *)
  let rodata_words : (natural * natural) list = Dwarf.words_of_byte_list rodata_addr bs [] in

  let read_rodata_b addr =
    Elf_types_native_uint.natural_of_byte
      rodata_bytes.(Nat_big_num.to_int (Nat_big_num.sub addr rodata_addr))
  in
  let read_rodata_h addr =
    Nat_big_num.add (read_rodata_b addr)
      (Nat_big_num.mul (Nat_big_num.of_int 256)
         (read_rodata_b (Nat_big_num.add addr (Nat_big_num.of_int 1))))
  in

  let sign_extend_W n =
    let half = Nat_big_num.mul (Nat_big_num.of_int 65536) (Nat_big_num.of_int 32768) in
    let whole = Nat_big_num.mul half (Nat_big_num.of_int 2) in
    if Nat_big_num.greater_equal n half then Nat_big_num.sub n whole else n
  in

  let read_rodata_W addr =
    sign_extend_W
      (Nat_big_num.add (read_rodata_b addr)
         (Nat_big_num.add
            (Nat_big_num.mul (Nat_big_num.of_int 256)
               (read_rodata_b (Nat_big_num.add addr (Nat_big_num.of_int 1))))
            (Nat_big_num.add
               (Nat_big_num.mul (Nat_big_num.of_int 65536)
                  (read_rodata_b (Nat_big_num.add addr (Nat_big_num.of_int 2))))
               (Nat_big_num.mul (Nat_big_num.of_int 16777216)
                  (read_rodata_b (Nat_big_num.add addr (Nat_big_num.of_int 3)))))))
  in

  let rec natural_assoc_opt n nys =
    match nys with
    | [] -> None
    | (n', y) :: nys' -> if Nat_big_num.equal n n' then Some y else natural_assoc_opt n nys'
  in

  (* this is the evaluator for a little stack-machine language used in the hafnium.branch-table files to describe the access pattern for each branch table *)
  (*
   n          push the index into the table
   s in 0..9  left-shift the stack head by 2^s
   r          push the branch table-base address
   o          push the branch table offset address
   +          replace the top two elements by their sum
   b          read byte from the branch table
   h          read two bytes from the branch table
   W          read four byte from the branch table and sign-extend
                                                            *)
  let rec eval_shift_expression (shift : string) (a_table : Nat_big_num.num)
      (a_offset : Nat_big_num.num) (i : Nat_big_num.num) (stack : Nat_big_num.num list) (pc : int)
      =
    if pc = String.length shift then
      match stack with
      | [a] -> a
      | _ -> Warn.fatal "eval_shift_expression terminated with non-singleton stack"
    else
      let command = shift.[pc] in
      if command = 'n' then
        (* push i *)
        let stack' = i :: stack in
        eval_shift_expression shift a_table a_offset i stack' (pc + 1)
      else if Char.code command >= Char.code '0' && Char.code command <= Char.code '9' then
        (* left shift head by 2^n *)
        match stack with
        | a :: stack' ->
            let a' =
              Nat_big_num.mul a
                (Nat_big_num.pow_int_positive 2 (Char.code command - Char.code '0'))
            in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> Warn.fatal "eval_shift_expression shift empty stack"
      else if command = 'r' then
        (* push rodata branch table base address *)
        let stack' = a_table :: stack in
        eval_shift_expression shift a_table a_offset i stack' (pc + 1)
      else if command = 'o' then
        (* push offset address *)
        let stack' = a_offset :: stack in
        eval_shift_expression shift a_table a_offset i stack' (pc + 1)
      else if command = '+' then
        (* plus *)
        match stack with
        | a1 :: a2 :: stack' ->
            let a' = Nat_big_num.add a1 a2 in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> Warn.fatal "eval_shift_expression plus emptyish stack"
      else if command = 'b' then
        (* read byte from branch table *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_b a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> Warn.fatal "eval_shift_expression b empty stack"
      else if command = 'h' then
        (* read halfword from branch table *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_h a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> Warn.fatal "eval_shift_expression h empty stack"
      else if command = 'W' then
        (* read word from branch table and sign-extend *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_W a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> Warn.fatal "eval_shift_expression W empty stack"
      else Warn.fatal "eval_shift_expression unknown command"
  in

  let branch_table_target_addresses =
    List.map
      (function
        | (a_br, (a_table, size, shift, a_offset)) ->
            let rec f i =
              if i > Nat_big_num.to_int size then []
              else
                let a_target =
                  if shift = "2" then
                    let table_entry_addr = Nat_big_num.add a_table (Nat_big_num.of_int (4 * i)) in
                    match natural_assoc_opt table_entry_addr rodata_words with
                    | None ->
                        Warn.fatal "no branch table entry for address %s\n"
                          (pp_addr table_entry_addr)
                    | Some table_entry ->
                        let a_target =
                          Nat_big_num.modulus
                            (Nat_big_num.add a_table table_entry)
                            (Nat_big_num.pow_int_positive 2 32)
                        in
                        (* that 32 is good for the sign-extended negative 32-bit offsets we see
                 in the old hafnium-playground-src branch tables *)
                        a_target
                  else eval_shift_expression shift a_table a_offset (Nat_big_num.of_int i) [] 0
                in
                a_target :: f (i + 1)
            in
            (a_br, f 0))
      branch_data
  in
  branch_table_target_addresses

(*****************************************************************************)
(*   parse control-flow instruction asm from objdump                         *)
(*****************************************************************************)

(* hacky parsing of AArch64 assembly from objdump -d to identify control-flow instructions and their arguments *)

let parse_addr (s : string) : natural = Scanf.sscanf s "%Lx" (fun i64 -> Nat_big_num.of_int64 i64)

let parse_target s =
  match Scanf.sscanf s " %s %s" (fun s1 s2 -> (s1, s2)) with
  | (s1, s2) -> Some (parse_addr s1, s2)
  | exception _ -> None

let parse_drop_one s =
  match
    Scanf.sscanf s " %s %n" (fun s1 n ->
        let s' = String.sub s n (String.length s - n) in
        (s1, s'))
  with
  | (s1, s') -> Some s'
  | exception _ -> None

let parse_control_flow_instruction s mnemonic s' : control_flow_insn =
  (*   Printf.printf "s=\"%s\" mnemonic=\"%s\"  mnemonic chars=\"%s\" s'=\"%s\"   "s mnemonic (String.concat "," (List.map (function c -> string_of_int (Char.code c)) (char_list_of_string mnemonic))) s';flush stdout;*)
  let c =
    if List.mem mnemonic [".word"] then C_no_instruction
    else if List.mem mnemonic ["ret"] then C_ret
    else if List.mem mnemonic ["eret"] then C_eret
    else if List.mem mnemonic ["br"] then C_branch_register mnemonic
    else if
      (String.length mnemonic >= 2 && String.sub mnemonic 0 2 = "b.")
      || List.mem mnemonic ["b"; "bl"]
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
  in
  (*Printf.printf "%s\n" (pp_control_flow_instruction c);*)
  c

(*****************************************************************************)
(*   compute targets of an instruction                                       *)
(*****************************************************************************)

let targets_of_control_flow_insn_without_index branch_table_targets (addr : natural)
    (opcode_bytes : int list) (c : control_flow_insn) : (target_kind * addr * string) list =
  let succ_addr = Nat_big_num.add addr (Nat_big_num.of_int (List.length opcode_bytes)) in
  let targets =
    match c with
    | C_no_instruction -> []
    | C_plain -> [(T_plain_successor, succ_addr, "")]
    | C_ret -> []
    | C_eret -> []
    | C_branch (a, s) -> [(T_branch, a, s)]
    | C_branch_and_link (a, s) ->
        (* special-case non-return functions to have no successor target of calls *)
        (* TODO: pull this from the DWARF attributes *)
        if List.mem s ["<abort>"; "<panic>"; "<__stack_chk_fail>"] then
          [(T_branch_and_link_call_noreturn, a, s)]
        else
          [(T_branch_and_link_call, a, s); (T_branch_and_link_successor, succ_addr, "<return>")]
    | C_branch_cond (is, a, s) ->
        [(T_branch_cond_branch, a, s); (T_branch_cond_successor, succ_addr, "<fallthrough>")]
    | C_branch_register r1 ->
        let addresses =
          try List.assoc addr branch_table_targets
          with Not_found ->
            Warn.fatal
              "targets_of_control_flow_insn_without_index C_branch_register fail for %s\n"
              (pp_addr addr)
        in
        List.mapi
          (function
            | i -> (
                function
                | a_target -> (T_branch_register, a_target, "<indirect" ^ string_of_int i ^ ">")
              ))
          addresses
    | C_smc_hvc s -> [(T_smc_hvc_successor, succ_addr, "<C_smc_hvc successor>")]
  in

  targets

let targets_of_control_flow_insn index_of_address branch_table_targets (addr : natural)
    (opcode_bytes : int list) (c : control_flow_insn) : target list =
  (*Printf.printf "targets_of_control_flow_insn addr=%s\n" (pp_addr addr); flush stdout;*)
  List.map
    (function
      | (tk, a'', s'') ->
          (*       Printf.printf "%s" ("foo " ^ pp_addr addr ^ " " ^ pp_control_flow_instruction c ^ " " ^ pp_target_kind_short tk ^ " " ^ pp_addr a'' ^ " " ^ s'' ^ "\n");*)
          (tk, a'', index_of_address a'', s''))
    (targets_of_control_flow_insn_without_index branch_table_targets addr opcode_bytes c)

let pp_opcode_bytes arch (opcode_bytes : int list) : string =
  match arch with
  | AArch64 -> String.concat "" (List.map (function b -> Printf.sprintf "%02x" b) opcode_bytes)
  | X86 -> String.concat " " (List.map (function b -> Printf.sprintf "%02x" b) opcode_bytes)

(*****************************************************************************)
(*        pull disassembly out of an objdump -d file                         *)
(*****************************************************************************)

(* objdump -d output format, from GNU objdump (GNU Binutils for Ubuntu) 2.30: 
x86:
  400150:\t48 83 ec 10          \tsub    $0x10,%rsp
  400160:	48 8d 05 99 0e 20 00 	lea    0x200e99(%rip),%rax        # 601000 <x>
 
AArch64: 
   10000:\t90000088 \tadrp\tx8, 20000 <x>
   10004:	52800129 	mov	w9, #0x9                   	// #9
 *)

let objdump_line_regexp =
  Str.regexp " *\\([0-9a-fA-F]+\\):\t\\([0-9a-fA-F ]+\\)\t\\([^ \t]+\\) *\\(.*\\)$"

type objdump_instruction = natural * int list (*opcode bytes*) * string (*mnemonic*) * string

(*args etc*)

let parse_objdump_line arch (s : string) : objdump_instruction option =
  let parse_hex_int64 s' =
    try Scanf.sscanf s' "%Lx" (fun i64 -> i64)
    with _ -> Warn.fatal "cannot parse address in objdump line %s\n" s
  in
  let parse_hex_int s' =
    try Scanf.sscanf s' "%x" (fun i -> i)
    with _ -> Warn.fatal "cannot parse opcode byte in objdump line %s\n" s
  in
  if Str.string_match objdump_line_regexp s 0 then
    let addr_int64 = parse_hex_int64 (Str.matched_group 1 s) in
    let addr = Nat_big_num.of_int64 addr_int64 in
    let op = Str.matched_group 2 s in
    let opcode_byte_strings =
      match arch with
      | AArch64 -> [String.sub op 0 2; String.sub op 2 2; String.sub op 4 2; String.sub op 6 2]
      | X86 -> List.filter (function s' -> s' <> "") (String.split_on_char ' ' op)
    in
    let opcode_bytes = List.map parse_hex_int opcode_byte_strings in
    let mnemonic = Str.matched_group 3 s in
    let operands = Str.matched_group 4 s in
    Some (addr, opcode_bytes, mnemonic, operands)
  else None

let parse_objdump_lines arch lines : objdump_instruction list =
  List.filter_map (parse_objdump_line arch) (Array.to_list lines)

let parse_objdump_file arch filename_objdump_d : objdump_instruction array =
  match read_file_lines filename_objdump_d with
  | MyFail s -> Warn.fatal "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename_objdump_d
  | Ok lines -> Array.of_list (parse_objdump_lines arch lines)

(*****************************************************************************)
(*   parse control-flow instruction asm from objdump and branch table data   *)
(*****************************************************************************)

let mk_instructions test filename_objdump_d filename_branch_table :
    instruction array * (addr -> index) * (index -> addr) =
  let objdump_instructions : objdump_instruction array =
    parse_objdump_file test.arch filename_objdump_d
  in

  (* TODO: check the objdump opcode_bytes against the ELF *)
  let branch_table_targets : (addr * addr list) list =
    branch_table_target_addresses test filename_branch_table
  in

  let index_of_address =
    let tbl = Hashtbl.create (Array.length objdump_instructions) in
    Array.iteri
      (function
        | k -> (
            function (addr, _, _, _) -> Hashtbl.add tbl addr k
          ))
      objdump_instructions;
    function
    | addr -> (
        try Hashtbl.find tbl addr
        with _ -> Warn.fatal "index_of_address didn't find %s\n" (pp_addr addr)
      )
  in

  let instructions =
    Array.map
      (function
        | (addr, opcode_bytes, mnemonic, operands) ->
            let c : control_flow_insn =
              parse_control_flow_instruction ("objdump line " ^ pp_addr addr) mnemonic operands
            in

            let targets =
              targets_of_control_flow_insn index_of_address branch_table_targets addr opcode_bytes
                c
            in
            {
              i_addr = addr;
              i_opcode = opcode_bytes;
              i_mnemonic = mnemonic;
              i_operands = operands;
              i_control_flow = c;
              i_targets = targets;
            })
      objdump_instructions
  in

  let address_of_index k = instructions.(k).i_addr in

  (instructions, index_of_address, address_of_index)

(* pull out indirect branches *)
let mk_indirect_branches instructions =
  List.filter
    (function
      | i -> (
          match i.i_control_flow with C_branch_register _ -> true | _ -> false
        ))
    (Array.to_list instructions)

let pp_indirect_branches indirect_branches =
  "\n************** indirect branch targets *****************\n"
  ^ String.concat ""
      (List.map
         (function
           | i ->
               pp_addr i.i_addr ^ " -> "
               ^ String.concat ","
                   (List.map (function (tk, a', k', s) -> pp_addr a' ^ "" ^ s ^ "") i.i_targets)
               ^ "\n")
         indirect_branches)

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

let mk_come_froms instructions : come_from list array =
  let size = Array.length instructions in
  let come_froms = Array.make size [] in
  Array.iteri
    (function
      | k -> (
          function
          | i ->
              List.iter
                (function
                  | (tk, a', k', s) ->
                      let come_from =
                        {
                          cf_target_kind = tk;
                          cf_addr = i.i_addr;
                          cf_index = k;
                          cf_control_flow = i.i_control_flow;
                          cf_desc = s;
                        }
                      in
                      if k' < size then come_froms.(k') <- come_from :: come_froms.(k') else ())
                i.i_targets
        ))
    instructions;
  Array.iteri
    (function
      | k -> (
          function cfs -> come_froms.(k) <- List.rev cfs
        ))
    come_froms;
  come_froms

let pp_come_froms (addr : addr) (cfs : come_from list) : string =
  match cfs with
  | [] -> ""
  | _ ->
      " <- "
      ^ String.concat ","
          (List.map
             (function
               | cf ->
                   pp_come_from_addr_wrt addr cf.cf_control_flow cf.cf_addr
                   ^ "("
                   ^ pp_target_kind_short cf.cf_target_kind
                   (*^ pp_control_flow_instruction_short c*)
                   ^ ")"
                   ^ cf.cf_desc)
             cfs)

(*****************************************************************************)
(*        pp control-flow graph                                              *)
(*****************************************************************************)

(* pp to dot a CFG.  Make a node for each non-{C_plain, C_branch}
   instruction, and an extra "start" node for each ELF symbol or other
   bl target.

   In choosing to only make nodes for conditional/indirect branches
   and calls/returns, we are not making nodes for every control-flow
   target, so this is a quotient of the underlying basic-block
   control-flow graph.  For example, it won't visualise multiple
   branches into distinct points of a straight-line control-flow
   sequence.  However, it will have most of the essential choices, and
   be simpler to look at.

*)

type node_kind_cfg =
  | CFG_node_start (* elf symbol or other bl target *)
  | CFG_node_branch_cond
  | CFG_node_branch_register
  | CFG_node_branch_and_link
  (*  | CFG_node_bl_noreturn*)
  | CFG_node_smc_hvc
  | CFG_node_ret
  | CFG_node_eret

type edge_kind_cfg = CFG_edge_flow | CFG_edge_branch_and_link_inline | CFG_edge_ret_inline | CFG_edge_correlate 

type node_name = string (*graphviz node name*)

type node_cfg = {
  nc_name : node_name;
  nc_kind : node_kind_cfg;
  nc_label : string;
  nc_addr : addr;
  nc_index : index;
  nc_colour : string;
  nc_tooltip : string;
}

type edge_cfg = node_name * node_name * edge_kind_cfg

(* graphs and graph fragments (in which the edges may mention other nodes) *)
type graph_cfg = {
  gc_start_nodes : node_cfg list;
  (* "start" nodes - synthetic nodes, for elf symbols or other bl targets*)
  gc_nodes : node_cfg list;
  (* non-start interior nodes  - corresponding to particular interesting control-flow instructions *)
  gc_edges : edge_cfg list; (* edges *)
  gc_edges_exiting : edge_cfg list; (* edges leaving a subgraph*)
  gc_subgraphs : (string(*subgraph name*) * string (*subgraph colour*) * graph_cfg) list;
  }

(* the gc_edges_exiting have to be kept separate because if they are within the subgraph in the generated .dot, graphviz pulls the target node *within* the subgraph, even if it isn't *)
               
let graph_cfg_empty () = { gc_start_nodes = []; gc_nodes = []; gc_edges = []; gc_edges_exiting=[];gc_subgraphs = [] }

let graph_cfg_union g1 g2 =
  {
    gc_start_nodes = g1.gc_start_nodes @ g2.gc_start_nodes;
    gc_nodes = g1.gc_nodes @ g2.gc_nodes;
    gc_edges = g1.gc_edges @ g2.gc_edges;
    gc_edges_exiting = g1.gc_edges_exiting @ g2.gc_edges_exiting;
    gc_subgraphs = g1.gc_subgraphs @ g2.gc_subgraphs;
  }

(* the graphviz svg colours from https://www.graphviz.org/doc/info/colors.html without those too close to white or those that dot complains about*)
let colours_svg =
  [
    (*"aliceblue";*)
    (*"antiquewhite";*)
    "aqua";
    "aquamarine";
    (*"azure";*)
    (*"beige";*)
    (*"bisque";*)
    "black";
    (*"blanchedalmond";*)
    "blue";
    "blueviolet";
    "brown";
    "burlywood";
    "cadetblue";
    "chartreuse";
    "chocolate";
    "coral";
    "cornflowerblue";
    (*"cornsilk";*)
    "crimson";
    "cyan";
    "darkblue";
    "darkcyan";
    "darkgoldenrod";
    (*"darkgray";*)
    "darkgreen";
    "darkgrey";
    "darkkhaki";
    "darkmagenta";
    "darkolivegreen";
    "darkorange";
    "darkorchid";
    "darkred";
    "darksalmon";
    "darkseagreen";
    "darkslateblue";
    "darkslategray";
    "darkslategrey";
    "darkturquoise";
    "darkviolet";
    "deeppink";
    "deepskyblue";
    "dimgray";
    "dimgrey";
    "dodgerblue";
    "firebrick";
    (*"floralwhite";*)
    "forestgreen";
    "fuchsia";
    (*"gainsboro";*)
    (*"ghostwhite";*)
    "gold";
    "goldenrod";
    "gray";
    "grey";
    "green";
    "greenyellow";
    (*"honeydew";*)
    "hotpink";
    "indianred";
    "indigo";
    (*"ivory";*)
    "khaki";
    (*"lavender";*)
    (*"lavenderblush";*)
    "lawngreen";
    (*"lemonchiffon";*)
    "lightblue";
    "lightcoral";
    (*"lightcyan";*)
    (*"lightgoldenrodyellow";*)
    (*"lightgray";*)
    "lightgreen";
    "lightgrey";
    "lightpink";
    "lightsalmon";
    "lightseagreen";
    "lightskyblue";
    "lightslategray";
    "lightslategrey";
    "lightsteelblue";
    (*"lightyellow";*)
    "lime";
    "limegreen";
    (*"linen";*)
    "magenta";
    "maroon";
    "mediumaquamarine";
    "mediumblue";
    "mediumorchid";
    "mediumpurple";
    "mediumseagreen";
    "mediumslateblue";
    "mediumspringgreen";
    "mediumturquoise";
    "mediumvioletred";
    "midnightblue";
    (*"mintcream";*)
    (*"mistyrose";*)
    "moccasin";
    "navajowhite";
    "navy";
    (*"oldlace";*)
    "olive";
    "olivedrab";
    "orange";
    "orangered";
    "orchid";
    "palegoldenrod";
    "palegreen";
    (*"paleturquoise";*)
    "palevioletred";
    (*"papayawhip";*)
    (*"peachpuff";*)
    "peru";
    "pink";
    "plum";
    "powderblue";
    "purple";
    "red";
    "rosybrown";
    "royalblue";
    "saddlebrown";
    "salmon";
    "sandybrown";
    "seagreen";
    (*"seashell";*)
    "sienna";
    "silver";
    "skyblue";
    "slateblue";
    "slategray";
    "slategrey";
    (*"snow";*)
    "springgreen";
    "steelblue";
    "tan";
    "teal";
    "thistle";
    "tomato";
    "turquoise";
    "violet";
    "wheat";
    (*"white";*)
    (*"whitesmoke";*)
    "yellow";
    "yellowgreen";
  ]

let colours_dot_complains =
  [
    "teal";
    "darkgrey";
    "silver";
    "darkcyan";
    "olive";
    "darkmagenta";
    "aqua";
    "darkred";
    "lime";
    "lightgreen";
    "darkblue";
    "fuchsia";
  ]

let colours = List.filter (function c -> not (List.mem c colours_dot_complains)) colours_svg

let html_escape s =
  String.concat ""
    (List.map
       (fun c ->
         match c with
         | '&' -> "&amp;"
         | '<' -> "&lt;"
         | '>' -> "&gt;"
         | '\"' -> "&quot;"
         | '\'' -> "&apos;"
         | _ -> String.make 1 c)
       (char_list_of_string s))

let include_tooltips = true
let mk_tooltip test an label k =
  if include_tooltips then 
  (* TODO: reduce the nasty code duplication between this and pp_instruction *)
  let i = an.instructions.(k) in
  let addr = i.i_addr in
  let come_froms' =
    List.filter (function cf -> cf.cf_target_kind <> T_plain_successor) an.come_froms.(k)
  in
  let lines =
    [label]
    @ List.map
        (pp_dwarf_source_file_lines' test.dwarf_static !Globals.show_source false)
        an.line_info.(k)
    (* the address and (hex) instruction *)
    @ [
        pp_addr addr ^ ":  "
        ^ pp_opcode_bytes test.arch i.i_opcode
        (* the dissassembly from objdump *)
        ^ "  "
        ^ i.i_mnemonic ^ "\t" ^ i.i_operands
        (* any indirect-branch control flow from this instruction *)
        ^ begin
            match i.i_control_flow with
            | C_branch_register _ ->
                " -> "
                ^ String.concat ","
                    (List.map
                       (function
                         | (tk, a', k', s) ->
                             pp_target_addr_wrt addr i.i_control_flow a' ^ "" ^ s ^ "")
                       i.i_targets)
                ^ " "
            | _ -> ""
          end
        (* any control flow to this instruction *)
        ^ pp_come_froms addr come_froms';
      ]
  in
  html_escape (String.concat "\n" lines)
  else ""
  
let mk_cfg test an node_name_prefix (recurse_flat:bool) (inline_all:bool) (start_indices : index list (*should be ELF symbol indices*))
    : graph_cfg =
  let colour k =
    match an.line_info.(k) with
    | [elifi] ->
        let subprogram_name = mk_subprogram_name test.dwarf_static elifi in
        let colour =
          List.nth colours (Hashtbl.hash subprogram_name land 65535 * List.length colours / 65536)
        in
        colour
    | _ -> "black"
  in

  (* the graphette start nodes are the instructions which are either
       - elf symbols, or
       - the branch target (but not the successor) of a C_branch_and_link  
          (probably these will all also have elf symbols) *)
  let is_graphette_start_target (cf : come_from) =
    match cf.cf_target_kind with
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

  let is_graphette_start k =
    an.elf_symbols.(k) <> [] || List.exists is_graphette_start_target an.come_froms.(k)
  in

  (* the (non-start) nodes are all the intereresting (non-unconditional-branch) control-flow instructions *)
  let is_graph_non_start_node k =
    let i = an.instructions.(k) in
    match i.i_control_flow with
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


  let pp_node_name_nesting nesting = String.concat "_" (List.map (function k -> pp_addr an.instructions.(k).i_addr) nesting) in
  
  (* we make up an additional node for all ELF symbols and bl targets; all others are just the address *)
  let node_name_start nesting addr = node_name_prefix ^ "start_" ^ pp_addr addr ^ "_" ^ pp_node_name_nesting nesting in
  let node_name nesting addr = node_name_prefix ^ pp_addr addr ^"_"^ pp_node_name_nesting nesting in

  (* need to track branch-visited edges because Hf loops back to a nop (abort.c) and a wfi (wait for interrupt)*)
  let rec next_non_start_node_name nesting visited k =
    let i = an.instructions.(k) in
    match i.i_control_flow with
    | C_plain -> (
        match i.i_targets with
        | [(tk, addr', k', s)] -> next_non_start_node_name nesting visited k'
        | _ -> Warn.fatal "non-unique plain targets at %s" (pp_addr i.i_addr)
      )
    | C_branch (a, s) -> (
        match i.i_targets with
        | [(tk, addr', k', s)] ->
            if List.mem k' visited then (node_name nesting addr', k') (* TODO: something more useful *)
            else next_non_start_node_name nesting (k' :: visited) k'
        | _ -> Warn.fatal "non-unique branch targets at %s" (pp_addr i.i_addr)
      )
    | _ -> (node_name nesting i.i_addr, k)
  in

  let mk_node nesting k kind label =
    let i = an.instructions.(k) in
    {
      nc_name = node_name nesting i.i_addr;
      nc_kind = kind;
      nc_label = label;
      nc_addr = i.i_addr;
      nc_index = k;
      nc_colour = colour k;
      nc_tooltip = mk_tooltip test an label k;
    }
  in

  let mk_node_simple nesting k kind label_prefix =
    let i = an.instructions.(k) in
    let label = label_prefix ^ pp_addr i.i_addr in
    mk_node nesting k kind label
  in

  let k_max = Array.length an.instructions in

  (* make the little piece of graph from a start node at k to its first non-start node *)
  let graphette_start nesting return_target k : graph_cfg * index list (* work_list_new *) =
    let i = an.instructions.(k) in
    let ss = an.elf_symbols.(k) in
    let (s, nn) =
      match ss with
      | [] -> (pp_addr i.i_addr, node_name nesting i.i_addr)
      | _ -> (List.hd (List.rev ss), node_name_start nesting i.i_addr)
    in
    let node =
      {
        nc_name = nn;
        nc_kind = CFG_node_start;
        nc_label = node_name_prefix ^ s;
        nc_addr = i.i_addr;
        nc_index = k;
        nc_colour = colour k;
        nc_tooltip = mk_tooltip test an s k;
      }
    in
    let (nn', k') = next_non_start_node_name nesting [] k in
    let edge = (node.nc_name, nn', CFG_edge_flow) in
    ({ gc_start_nodes = [node]; gc_nodes = []; gc_edges = [edge]; gc_edges_exiting = []; gc_subgraphs=[] }, [k'])
  in

  (* make the piece of graph from a non-start node onwards, following fall-through control flow and branch-and-link successors, up to the first interesting control flow - including the outgoing edges, but not their target nodes  *)
  let rec graphette_normal nesting return_target k : graph_cfg * index list * (* work_list_new *) index list
      (* bl targets *) =
    (*Printf.printf "gb k=%d\n a=%s" k (pp_addr (address_of_index k));flush stdout;*)
    let i = an.instructions.(k) in
    match i.i_control_flow with
    | C_no_instruction ->
        Warn.fatal "graphette_normal on C_no_instruction"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_plain ->
        Warn.fatal "graphette_normal on C_plain"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_branch _ ->
        Warn.fatal "graphette_normal on C_branch"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_ret ->
        let node = mk_node nesting k CFG_node_ret "ret" in
        let edges_exiting = match return_target with Some nn' -> [(node.nc_name, nn',CFG_edge_ret_inline)] | None -> [] in 
        ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = []; gc_edges_exiting = edges_exiting; gc_subgraphs=[] }, [], [])
    | C_eret ->
        let node = mk_node nesting k CFG_node_eret "eret" in
        ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = []; gc_edges_exiting = []; gc_subgraphs=[] }, [], [])
    | C_smc_hvc s ->
        let node = mk_node_simple nesting k CFG_node_smc_hvc "smc/hvc " in
        let (edges, work_list_new) =
          List.split
            (List.filter_map
               (function
                 | (T_smc_hvc_successor, a', k', s') ->
                     let (nn', k'') = next_non_start_node_name nesting [] k' in
                     Some ((node.nc_name, nn', CFG_edge_flow), k'')
                 | _ -> None)
               i.i_targets)
        in
        ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs=[] }, work_list_new, [])
    | C_branch_cond (mnemonic, a, s) ->
        let node = mk_node_simple nesting k CFG_node_branch_cond "" in
        let (edges, work_list_new) =
          List.split
            (List.map
               (function
                 | (tk, addr', k', s') ->
                     let (nn', k'') = next_non_start_node_name nesting [] k' in
                     ((node.nc_name, nn', CFG_edge_flow), k''))
               i.i_targets)
        in
        ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs=[] }, work_list_new, [])
    | C_branch_register _ ->
        let node = mk_node_simple nesting k CFG_node_branch_register "" in
        let (edges, work_list_new) =
          List.split
            (List.sort_uniq compare
               (List.map
                  (function
                    | (tk, addr', k', s') ->
                        let (nn', k'') = next_non_start_node_name nesting [] k' in
                        ((node.nc_name, nn', CFG_edge_flow), k''))
                  i.i_targets))
        in
        ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs=[] }, work_list_new, [])
    | C_branch_and_link (a, s) -> 
       let k_call =
         match List.filter_map
           (function
            | (T_branch_and_link_call, a', k', s') -> Some k'
            | (T_branch_and_link_call_noreturn, a', k', s') -> Some k'
            | _ -> None)
           i.i_targets
         with
         | [k_call]->k_call
         | _ -> Warn.fatal "non-unique k_call"
       in
       let nn_k_successor =
         match
           List.filter_map
             (function (T_branch_and_link_successor, a', k', s') -> Some k' | _ -> None)
             i.i_targets
         with
         | [k'] ->
            let (nn', k'') = next_non_start_node_name nesting [] k' in
            Some (nn',k'')
         | _ ->
            (* noreturn *)
            None
       in
       let inline = inline_all in
       let node = mk_node nesting k CFG_node_branch_and_link s in
       if not(inline) || List.mem k nesting then
         (* not inline: construct a new node for the bl. If not noreturn, add an edge to its successor and return that in the new worklist, otherwise stop at this node.  In either case, if recurse_flat, add the bl target to the bl_target_indices *)
         match nn_k_successor with
         | Some (nn',k'') -> 
            let edges = [(node.nc_name, nn', CFG_edge_flow)] in
            ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs=[] }, [k''], (if recurse_flat then [k_call] else  []))
         | None ->
            ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = []; gc_edges_exiting = []; gc_subgraphs=[] }, [], (if recurse_flat then [k_call] else  []))
       else
         (* inline: construct a new node for the bl and a new subgraph for the inlined subroutine, and an edge between them (faking up the node name that mk_graph will use for its start node).  Pass the return_target' in to the subgraph construction, for it to add edges from the ret to the successor (if any) *)
         let nesting' = k::nesting in
         let (return_target',work_list_new) = match nn_k_successor with Some(nn',k'') -> (Some nn',[k'']) | None -> (None,[]) in 
         let subgraph_name = "cluster_" ^ pp_node_name_nesting nesting' in 
         let subgraph_colour = colour k_call in 
         let subgraph = mk_graph nesting' return_target' [k_call] in
         let nn'' = node_name_start nesting' an.instructions.(k_call).i_addr in 
         let edges = [(node.nc_name, nn'', CFG_edge_branch_and_link_inline)] in
         ({ gc_start_nodes = []; gc_nodes = [node]; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs=[(subgraph_name,subgraph_colour,subgraph)] }, work_list_new, [])
          
          
  (*
  (* glom together the bits of graph constructed starting from each instruction index *)
  let rec mk_graph g_acc n k =
    if k >= n then g_acc
    else
      let g_acc' =
        if is_graphette_start k then graph_cfg_union (graphette_start k) g_acc else g_acc
      in
      let g_acc'' =
        if is_graph_non_start_node k then graph_cfg_union (graphette_normal k) g_acc' else g_acc'
      in
      mk_graph g_acc'' n (k + 1)
  in
  let (graph : graph_cfg) = mk_graph (graph_cfg_empty ()) k_max 0 in

  graph
   *)
  and mk_graph' nesting return_target g_acc (visited : index list) (work_list : index list) =
    match work_list with
    | [] -> g_acc
    | k :: work_list' -> (
      if List.mem k visited then mk_graph' nesting return_target g_acc visited work_list'
      else
        begin
        Printf.printf "mk_graph' working on %d %s %s\n" k (pp_addr an.instructions.(k).i_addr) (String.concat "," an.elf_symbols.(k)); flush stdout;
        match (is_graphette_start k, is_graph_non_start_node k) with
          | (true, true) ->
             (* graphette start, where the initial instruction is also a non-start node *)
             let (g1, _) = graphette_start nesting return_target k in
              let (g2, work_list_new, bl_target_indices) = graphette_normal nesting return_target k in
              let g_acc' = graph_cfg_union g1 (graph_cfg_union g2 g_acc) in
              let visited' = k :: visited in
              let work_list' = work_list_new @ bl_target_indices @ work_list' in
              mk_graph' nesting return_target g_acc' visited' work_list'
          | (true, false) ->
             (* graphette start, where the initial instruction is not also a non-start node *)       
             let (g1, work_list_new) = graphette_start nesting return_target k in
             let g_acc' = graph_cfg_union g1 g_acc in
             let visited' = k :: visited in
             let work_list' = work_list_new @ work_list' in
             mk_graph' nesting return_target g_acc' visited' work_list'
          | (false, true) ->
             (* non-graphette-start, non-start node*)
             let (g2, work_list_new, bl_target_indices) = graphette_normal nesting return_target k in
              let g_acc' = graph_cfg_union g2 g_acc in
              let visited' = k :: visited in
              let work_list' = work_list_new @ bl_target_indices @ work_list' in
              mk_graph' nesting return_target g_acc' visited' work_list'
          | (false, false) ->
             (* non-graphette-start, and not a non-start node*)
             Warn.nonfatal
                "mk_graph' called on index %d at %s which is neither is_graphette_start nor \
                 is_graph_non_start_node - could be a self-loop"
                k
                (pp_addr an.instructions.(k).i_addr);
              mk_graph' nesting return_target g_acc visited work_list'
        end
    )

  and mk_graph nesting return_target (work_list : index list) =
    mk_graph' nesting return_target (graph_cfg_empty ()) [] work_list

  in
  mk_graph [] None start_indices

(* render graph to graphviz dot file *)

let pp_colour colour =
  "[color=\"" ^ colour ^ "\"]" (*^ "[fillcolor=\"" ^ colour ^ "\"]"*) ^ "[fontcolor=\""
  ^ colour ^ "\"]"

let margin = "[margin=\"0.03,0.02\"]"

(* let nodesep = "[nodesep=\"0.25\"]" in (*graphviz default *) *)
let nodesep = "[nodesep=\"0.1\"]"

let pp_node_name nn = "\"" ^ nn ^ "\""

let pp_edge graph_colour (nn, nn', cek) =
  match cek with
  | CFG_edge_flow 
  | CFG_edge_branch_and_link_inline 
  | CFG_edge_ret_inline ->
      pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep ^ "[color=\"" ^ graph_colour ^ "\"]" ^ ";\n"
  | CFG_edge_correlate ->
      pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep
      ^ "[constraint=\"false\";style=\"dashed\";color=\"lightgrey\"];\n"

let pp_cfg (g : graph_cfg) cfg_dot_file : unit =
  (*    let margin = "[margin=\"0.11,0.055\"]" in  (*graphviz default*) *)
  let pp_node node =
    let shape =
      match node.nc_kind with
      | CFG_node_branch_and_link | CFG_node_smc_hvc -> "[shape=\"box\"]"
      | _ -> ""
    in
    Printf.sprintf "%s [label=\"%s\"][tooltip=\"%s\"]%s%s%s;\n" (pp_node_name node.nc_name)
      node.nc_label node.nc_tooltip margin shape (pp_colour node.nc_colour)
  in

  let c = open_out cfg_dot_file in
  Printf.fprintf c "digraph g {\n";
  Printf.fprintf c "rankdir=\"LR\";\n";

  let rec pp_cfg' graph_colour indent (g:graph_cfg) : unit = 
    List.iter (function node -> Printf.fprintf c "%s%s\n" indent (pp_node node)) g.gc_start_nodes;
    Printf.fprintf c "%s{ rank=min; %s }\n" indent
      (String.concat ""
         (List.map (function node -> pp_node_name node.nc_name ^ ";") g.gc_start_nodes));
    List.iter (function node -> Printf.fprintf c "%s%s\n" indent (pp_node node)) g.gc_nodes;
    List.iter (function e -> Printf.fprintf c "%s%s\n" indent (pp_edge graph_colour e)) g.gc_edges;
    List.iter (function (subgraph_name,subgraph_colour,g') -> Printf.fprintf c "%ssubgraph %s {\n%scolor=%s\n" indent subgraph_name indent subgraph_colour; pp_cfg' subgraph_colour ("  "^indent) g'; Printf.fprintf c "}\n";     List.iter (function e -> Printf.fprintf c "%s%s\n" indent (pp_edge subgraph_colour e)) g'.gc_edges_exiting;
) g.gc_subgraphs;

  in
  pp_cfg' "black" "" g;
  Printf.fprintf c "}\n";
  
  let _ = close_out c in
  ()

(* carve out reachable subgraph from some starting node labels (using that so that the user can just paste in from the rendered graph if need be) *)

let reachable_subgraph (g : graph_cfg) (labels_start : string list) : graph_cfg =
  let nodes_all : node_cfg list = g.gc_start_nodes @ g.gc_nodes in
  let edges_all : (node_name * node_name list) list =
    List.map
      (function
        | node ->
            ( node.nc_name,
              List.filter_map
                (function (nn1, nn2, cek) -> if nn1 = node.nc_name then Some nn2 else None)
                g.gc_edges ))
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
      (function node -> if List.mem node.nc_label labels_start then Some node.nc_name else None)
      nodes_all
  in
  let node_names_reachable = stupid_reachability false [] start_node_names in
  let edges_reachable =
    List.filter
      (function
        | (nn, nn', cek) -> List.mem nn node_names_reachable && List.mem nn' node_names_reachable)
      g.gc_edges
  in
  let nodes_reachable_start =
    List.filter (function node -> List.mem node.nc_name node_names_reachable) g.gc_start_nodes
  in
  let nodes_reachable_rest =
    List.filter (function node -> List.mem node.nc_name node_names_reachable) g.gc_nodes
  in
  {
    gc_start_nodes = nodes_reachable_start;
    gc_nodes = nodes_reachable_rest;
    gc_edges = edges_reachable;
    gc_edges_exiting = []; (*HACK*)
    gc_subgraphs = []; (* HACK*)
  }

(*
module P = Graph.Pack
http://ocamlgraph.lri.fr/doc/Fixpoint.html
 *)

(* same-source-line edges *)

let correlate_source_line test1 line_info1 g1 test2 line_info2 g2 : graph_cfg =
  let is_branch_cond = function
    | node -> (
        match node.nc_kind with
        | CFG_node_branch_cond | CFG_node_branch_register | CFG_node_ret
         |CFG_node_branch_and_link ->
            true
        | _ -> false
      )
  in
  let nodes_branch_cond1 = List.filter is_branch_cond g1.gc_nodes in
  let nodes_branch_cond2 = List.filter is_branch_cond g2.gc_nodes in
  let with_source_lines test line_info = function
    | node -> (node.nc_name, dwarf_source_file_line_numbers_by_index test line_info node.nc_index)
  in
  let nodes_branch_cond_with1 =
    List.map (with_source_lines test1 line_info1) nodes_branch_cond1
  in
  let nodes_branch_cond_with2 =
    List.map (with_source_lines test2 line_info2) nodes_branch_cond2
  in
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
  { gc_start_nodes = []; gc_nodes = []; gc_edges = edges; gc_edges_exiting = []; gc_subgraphs = [] }

(*****************************************************************************)
(*        render control-flow branches in text output                        *)
(*****************************************************************************)

type weight = L | B

type glyph =
  | Glr of weight
  | Gud of weight
  | Gru of weight
  | Grd of weight
  | Grud of weight
  | Glrud of weight * weight
  | Ggt
  | Glt
  | GX
  | Gnone
  | Gquery

type arrow = {
  source : index;
  targets : index list;
  first : index;
  (* min of source and all targets *)
  last : index;
  (* max of source and all targets *)
  weight : weight;
}

let render_ascii_control_flow max_branch_distance max_width instructions :
    string array * string array (*inbetweens*) * int (* actual_width *) =
  (* pull the arrows out of instructions *)
  let arrow_from (k : index) i : arrow option =
    (*(addr, i, m, args, c, targets1)*)
    let render_target_kind = function
      | T_plain_successor -> false
      | T_branch -> true
      | T_branch_and_link_call -> false
      | T_branch_and_link_call_noreturn -> false
      | T_branch_and_link_successor -> false
      | T_branch_cond_branch -> true
      | T_branch_cond_successor -> false
      | T_branch_register -> true
      | T_smc_hvc_successor -> false
    in

    (* filter out targets that we're not going to render *)
    let targets2 =
      List.filter (function (tk, a', k', s) -> render_target_kind tk) i.i_targets
    in

    match targets2 with
    | [] -> None
    | _ ->
        (* sort targets by target instruction index *)
        let targets3 =
          List.sort_uniq
            (function
              | (tk', a', k', s') -> (
                  function (tk'', a'', k'', s'') -> compare k' k''
                ))
            targets2
        in
        (* project out just the index *)
        let targets4 = List.map (function (tk', a', k', s') -> k') targets3 in

        let first = min k (List.hd targets4) in
        let last = max k (list_last targets4) in

        Some
          { source = k; targets = targets4; first; last; weight = (if first < k then B else L) }
  in

  let array_filter_mapi (f : int -> 'a -> 'b option) (a : 'a array) : 'b list =
    let rec g k acc =
      if k < 0 then acc else g (k - 1) (match f k a.(k) with None -> acc | Some b -> b :: acc)
    in
    g (Array.length a - 1) []
  in

  let arrows0 : arrow list = array_filter_mapi arrow_from instructions in

  (* sort by size, to render short arrows rightmost *)
  let compare_arrow a1 a2 = compare (a1.last - a1.first) (a2.last - a2.first) in
  let arrows = List.stable_sort compare_arrow arrows0 in

  (* paint the arrows into a buffer of glyphs *)
  let buf = Array.make_matrix (Array.length instructions) max_width Gnone in
  let leftmost_column_used = ref max_width in

  let paint_arrow a =
    let rec forall k1 k2 f = if k1 > k2 then true else f k1 && forall (k1 + 1) k2 f in

    let rec largest c1 c2 f =
      if c2 < c1 then None else if f c2 then Some c2 else largest c1 (c2 - 1) f
    in

    let try_at_column dry_run c =
      let try_for_row k =
        let is_target = List.mem k a.targets in
        let is_source = k = a.source in
        let is_self_target = is_target && is_source in

        let free k c' = buf.(k).(c') = Gnone in

        let paint k c' g =
          if buf.(k).(c') = Gnone then (
            if dry_run then () else buf.(k).(c') <- g;
            true
          )
          else false
        in

        let paint_allowing_crossing k c' g =
          match
            match (buf.(k).(c'), g) with
            | (Gnone, g) -> Some g
            | (Glr w1, Gud w2) -> Some (Glrud (w1, w2))
            | (Gud w2, Glr w1) -> Some (Glrud (w1, w2))
            | (_, _) -> None
          with
          | None -> false
          | Some g' ->
              if dry_run then () else buf.(k).(c') <- g';
              true
        in

        let paint_target_arrow k c' ghead w =
          match
            largest c' (max_width - 1) (fun c'' ->
                free k c''
                && forall c' (c'' - 1) (fun c''' ->
                       buf.(k).(c''') = Gnone || buf.(k).(c''') = Gud w))
          with
          | Some c_head ->
              if dry_run then () else buf.(k).(c_head) <- ghead;
              for c'' = c' to c_head - 1 do
                ignore (paint_allowing_crossing k c'' (Glr w))
              done;
              true
          | None -> false
        in

        let paint_source_line k c' w =
          match
            largest c' (max_width - 1) (fun c'' ->
                forall c' c'' (fun c''' ->
                    match buf.(k).(c''') with Gnone -> true | Gud w -> true | _ -> false))
          with
          | Some c_head ->
              for c'' = c' to c_head do
                ignore (paint_allowing_crossing k c'' (Glr w))
              done;
              true
          | None -> false
        in

        let w = a.weight in

        if is_target || is_source then
          paint k c
            ( match (a.first = k, k = a.last) with
            | (true, false) -> Grd w
            | (false, false) -> Grud w
            | (false, true) -> Gru w
            | (true, true) -> Gnone
            )
          &&
          if is_target then paint_target_arrow k (c + 1) (if is_self_target then GX else Ggt) w
          else paint_source_line k (c + 1) w
        else paint_allowing_crossing k c (Gud w)
      in

      forall a.first a.last try_for_row
    in

    if match max_branch_distance with None -> true | Some d -> a.last - a.first < d then
      match largest 1 (max_width - 2) (try_at_column true) with
      | Some c ->
          ignore (try_at_column false c);
          leftmost_column_used := min c !leftmost_column_used
      | None -> buf.(a.source).(max_width - 1) <- Gquery
    else begin
      (*hackish paint_long_branch, ignoring whatever is underneath*)
      buf.(a.source).(max_width - 1) <- Glt;
      buf.(a.source).(max_width - 2) <- Glt;
      List.iter
        (function
          | k' ->
              let g = if k' = a.source then GX else Ggt in
              buf.(k').(max_width - 1) <- g;
              buf.(k').(max_width - 2) <- g)
        a.targets;
      leftmost_column_used := min (max_width - 2) !leftmost_column_used
    end
  in
  List.iter paint_arrow arrows;

  (* convert glyph matrix into string array *)
  let pp_glyph = function
    | Glr L -> "\u{2500}" (*   *)
    | Gud L -> "\u{2502}" (*   *)
    | Gru L -> "\u{2514}" (*   *)
    | Grd L -> "\u{250c}" (*   *)
    | Grud L -> "\u{251c}" (*   *)
    | Glrud (L, L) -> "\u{253c}" (*   *)
    | Glr B -> "\u{2550}" (*   *)
    | Gud B -> "\u{2551}" (*   *)
    | Gru B -> "\u{255a}" (*   *)
    | Grd B -> "\u{2554}" (*   *)
    | Grud B -> "\u{2560}" (*   *)
    | Glrud (B, B) -> "\u{256c}" (*   *)
    | Glrud (L, B) -> "\u{256b}" (*   *)
    | Glrud (B, L) -> "\u{256a}" (*   *)
    | Ggt -> ">" (*   *)
    | Glt -> "<" (*   *)
    | GX -> "X" (*   *)
    | Gnone -> " " (*   *)
    | Gquery -> "?"
    (*   *)
  in

  (* actual width used *)
  let width = max_width - !leftmost_column_used in

  (* construct inter-line list of glphys  of vertical arrows for filler *)
  let inbetweens =
    Array.init (Array.length buf) (function k ->
        if k = 0 then String.make width ' '
        else
          String.concat ""
            (List.map2
               (fun g1 g2 ->
                 if
                   List.mem g1 [Gud L; Grd L; Grud L; Glrud (L, L); Glrud (B, L)]
                   && List.mem g2 [Gud L; Gru L; Grud L; Glrud (L, L); Glrud (B, L)]
                 then pp_glyph (Gud L)
                 else if
                   List.mem g1 [Gud B; Grd B; Grud B; Glrud (L, B); Glrud (B, B)]
                   && List.mem g2 [Gud B; Gru B; Grud B; Glrud (L, B); Glrud (B, B)]
                 then pp_glyph (Gud B)
                 else pp_glyph Gnone)
               (Array.to_list (Array.sub buf.(k - 1) !leftmost_column_used width))
               (Array.to_list (Array.sub buf.(k) !leftmost_column_used width))))
  in

  ( Array.map
      (function
        | row ->
            String.concat ""
              (List.map pp_glyph (Array.to_list (Array.sub row !leftmost_column_used width))))
      buf,
    inbetweens,
    width )

(*****************************************************************************)
(*        call-graph                                                         *)
(*****************************************************************************)

type call_graph_node = addr * index * string list

let pp_call_graph test (instructions, index_of_address, address_of_index, indirect_branches) =
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
           | i ->
               let bl_targets =
                 List.filter
                   (function
                     | (tk', a', k', s') -> (
                         match tk' with
                         | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                         | _ -> false
                       ))
                   i.i_targets
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
         (Array.to_list instructions))
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
        else if not (k < Array.length instructions) then
          stupid_reachability acc_reachable acc_bl_targets todo'
        else
          let i = instructions.(k) in
          let (bl_targets, non_bl_targets) =
            List.partition
              (function
                | (tk'', a'', k'', s'') -> (
                    match tk'' with
                    | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                    | _ -> false
                  ))
              i.i_targets
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

  pp_call_graph call_graph ^ "* ************* transitive call graph **************\n"
  ^ pp_transitive_call_graph transitive_call_graph

(*****************************************************************************)
(* extracting and pretty-printing variable info from linksem simple die tree view, adapted from dwarf.lem  *)
(*****************************************************************************)

let indent_level (indent : bool) (level : int) : string =
  if indent then String.make (level * 3) ' ' else " "

let indent_level_plus_one indent level : string =
  if indent then indent_level indent (level + 1) else " " ^ "   "

let pp_sdt_concise_variable_or_formal_parameter_main (level : int)
    (svfp : Dwarf.sdt_variable_or_formal_parameter) : string =
  let indent = indent_level true level in
  "" ^ indent
  (*  ^ indent ^ "cupdie:" ^  pp_cupdie3 svfp.svfp_cupdie ^ "\n"*)
  (*^ indent ^ "name:" ^*) ^ svfp.svfp_name
  ^ "  "
  (*^ indent ^ "kind:" *) ^ (match svfp.svfp_kind with SVPK_var -> "var" | SVPK_param -> "param")
  ^ "  "
  (*^ indent ^ "type:" *) ^ Dwarf.pp_type_info_deep svfp.svfp_type
  ^ "  "
  (*^ indent ^ "const_value:"*)
  ^ (match svfp.svfp_const_value with None -> "" | Some v -> "const:" ^ Nat_big_num.to_string v)
  ^ "  "

(*^ indent ^ "external:" ^  show svfp.svfp_external ^ "\n"*)
(*^ indent ^ "declaration:" ^  show svfp.svfp_declaration ^ "\n"*)
(*^ indent ^ "locations:" *)

let pp_sdt_concise_variable_or_formal_parameter (level : int) (is_params : bool)
    (svfp : Dwarf.sdt_variable_or_formal_parameter) : string =
  (if is_params then "+" else "")
  ^ pp_sdt_concise_variable_or_formal_parameter_main level svfp
  ^
  match svfp.svfp_locations with
  | None -> "no locations\n"
  | Some [loc] -> " " ^ Dwarf.pp_parsed_single_location_description (Nat_big_num.of_int 0) loc
  | Some locs ->
      "\n"
      ^ String.concat ""
          (Lem_list.map
             (function
               | loc ->
                   "+"
                   ^ Dwarf.pp_parsed_single_location_description
                       (Nat_big_num.of_int (level + 1))
                       loc)
             locs)

(*  ^ indent ^ "decl:" ^ (match svfp.svfp_decl with Nothing -> "none\n" | Just ((ufe,line) as ud) -> "\n" ^ indent_level true (level+1) ^ pp_ufe ufe ^ " " ^ show line ^ "\n" end)*)

let pp_sdt_globals_compilation_unit (level : int) (cu : Dwarf.sdt_compilation_unit) : string =
  let indent = indent_level true level in
  ""
  (*  ^ indent ^ "cupdie:" ^  pp_cupdie3 cu.scu_cupdie ^ "\n"*)
  ^ indent
  (*"name:" ^*) ^ cu.scu_name
  ^ "\n"
  (*  ^ indent ^ "vars:" ^  "\n"*)
  ^ String.concat ""
      (Lem_list.map (pp_sdt_concise_variable_or_formal_parameter (level + 1) false) cu.scu_vars)

(*  ^ indent ^ "subroutines :" ^  (match cu.scu_subroutines with | [] -> "none\n" | sus -> "\n" ^ String.concat "\n" (List.map  (pp_sdt_subroutine (level+1)) sus) end) *)

let pp_sdt_globals_dwarf (sdt_d : Dwarf.sdt_dwarf) : string =
  let indent_level = 0 in
  String.concat ""
    (List.map (pp_sdt_globals_compilation_unit indent_level) sdt_d.sd_compilation_units)

(* ******************  local vars *************** *)

let maybe_name x : string = match x with None -> "no name" | Some y -> y

let rec locals_subroutine context (ss : Dwarf.sdt_subroutine) =
  let name = maybe_name ss.ss_name in
  let kind1 =
    match ss.ss_kind with SSK_subprogram -> "" | SSK_inlined_subroutine -> "(inlined)"
  in
  let context1 = (name ^ kind1) :: context in
  List.map (function var -> (var, context1)) ss.ss_vars
  @ begin
      match ss.ss_abstract_origin with
      | None -> []
      | Some ss' ->
          let kind2 = "(abstract origin)" in
          let context2 = (name ^ kind2) :: context in
          List.map (function var -> (var, context2)) ss'.ss_vars
          (* TODO: what about the unspecified parameters? *)
    end
  @ List.flatten (List.map (locals_subroutine context1) ss.ss_subroutines)
  @ List.flatten (List.map (locals_lexical_block context1) ss.ss_lexical_blocks)

(*   
    ^ (indent (*^ "name:"                   ^*) ^ (pp_sdt_maybe ss.ss_name (fun name1 -> name1 ^ "\n")
  (*  ^ indent ^ "cupdie:"                 ^ pp_cupdie3 ss.ss_cupdie ^ "\n"*)
  ^ (indent ^ ("kind:"                   ^ (((match ss.ss_kind with SSK_subprogram -> "subprogram" | SSK_inlined_subroutine -> "inlined subroutine" )) ^ ("\n" 
  ^ (indent ^ ("call site:"              ^ (pp_sdt_maybe ss.ss_call_site (fun ud -> "\n" ^ (indent_level true (Nat_big_num.add level(Nat_big_num.of_int 1)) ^ (pp_ud ud ^ "\n")))
  ^ (indent ^ ("abstract origin:"        ^ (pp_sdt_maybe ss.ss_abstract_origin (fun s -> "\n" ^ locals__subroutine (Nat_big_num.add level(Nat_big_num.of_int 1)) s)
  (*  ^ indent ^ "type:"                   ^ pp_sdt_maybe ss.ss_type (fun typ -> pp_type_info_deep typ ^"\n" end)*)
  ^ (indent ^ ("vars:"                   ^ (pp_sdt_list ss.ss_vars (pp_sdt_concise_variable_or_formal_parameter (Nat_big_num.add level(Nat_big_num.of_int 1)))
  ^ (indent ^ ("unspecified_parameters:" ^ (pp_sdt_list ss.ss_unspecified_parameters (pp_sdt_unspecified_parameter (Nat_big_num.add level(Nat_big_num.of_int 1)))
  (*  ^ indent ^ "pc ranges:"              ^ pp_pc_ranges (level+1) ss.ss_pc_ranges*)
  ^ (indent ^ ("subroutines:"            ^ (pp_sdt_list ss.ss_subroutines (locals__subroutine (Nat_big_num.add level(Nat_big_num.of_int 1)))
  ^ (indent ^ ("lexical_blocks:"         ^ (pp_sdt_list ss.ss_lexical_blocks (locals__lexical_block (Nat_big_num.add level(Nat_big_num.of_int 1)))
  (*  ^ indent ^ "decl:"                   ^ pp_sdt_maybe ss.ss_decl (fun ((ufe,line) as ud) -> "\n" ^ indent_level true (level+1) ^ pp_ufe ufe ^ " " ^ show line ^ "\n" end)*)
  (*  ^ indent ^ "noreturn:"               ^ show ss.ss_noreturn ^ "\n"*)
  (*  ^ indent ^ "external:"               ^ show ss.ss_external ^"\n"*)
  ^ "\n")))))))))))))))))))))))))   
 *)
and locals_lexical_block context (lb : Dwarf.sdt_lexical_block) =
  let context1 = "lexblock" :: context in
  List.map (function var -> (var, context1)) lb.slb_vars
  @ List.flatten (List.map (locals_subroutine context1) lb.slb_subroutines)
  @ List.flatten (List.map (locals_lexical_block context1) lb.slb_lexical_blocks)

(*
  ""
  (*  ^ indent ^ "cupdie:"         ^ pp_cupdie3 lb.slb_cupdie ^ "\n"*)
  ^ (indent ^ ("vars:"           ^ (pp_sdt_list lb.slb_vars (pp_sdt_concise_variable_or_formal_parameter (Nat_big_num.add level(Nat_big_num.of_int 1)))
  (*  ^ indent ^ "pc ranges:"      ^ pp_pc_ranges (level+1) lb.slb_pc_ranges*)
  ^ (indent ^ ("subroutines :"   ^ (pp_sdt_list lb.slb_subroutines (locals__subroutine (Nat_big_num.add level(Nat_big_num.of_int 1)))
  ^ (indent ^ ("lexical_blocks:" ^ (pp_sdt_list lb.slb_lexical_blocks (locals__lexical_block (Nat_big_num.add level(Nat_big_num.of_int 1)))
  ^ "\n"))))))))))   
 *)

let locals_compilation_unit context (cu : Dwarf.sdt_compilation_unit) =
  let name = cu.scu_name in
  let context1 = name :: context in
  (*List.map (function var -> (var, context1)) cu.scu_vars
  @*)
  List.flatten (List.map (locals_subroutine context1) cu.scu_subroutines)

(*
  ""
  ^ (indent (*^ "name:"         *) ^ (cu.scu_name ^ ("\n"
  (*  ^ indent ^ "cupdie:"       ^ pp_cupdie3 cu.scu_cupdie ^ "\n"*)
  ^ (indent ^ ("vars:"         ^ (pp_sdt_list cu.scu_vars (pp_sdt_concise_variable_or_formal_parameter (Nat_big_num.add level(Nat_big_num.of_int 1)))
  ^ (indent ^ ("subroutines :" ^ pp_sdt_list cu.scu_subroutines (locals__subroutine (Nat_big_num.add level(Nat_big_num.of_int 1))))))))))))
 *)
let locals_dwarf (sdt_d : Dwarf.sdt_dwarf) :
    (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list =
  let context = [] in
  (*List.map (function var -> (var, context1)) cu.scu_vars
  @*)
  List.flatten (List.map (locals_compilation_unit context) sdt_d.sd_compilation_units)

let globals_compilation_unit context (cu : Dwarf.sdt_compilation_unit) =
  let name = cu.scu_name in
  let context1 = name :: context in
  List.map (function var -> (var, context1)) cu.scu_vars

(*@
  List.flatten (List.map (locals_subroutine context1) cu.scu_subroutines)*)

let globals_dwarf (sdt_d : Dwarf.sdt_dwarf) :
    (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list =
  let context = [] in
  List.flatten (List.map (globals_compilation_unit context) sdt_d.sd_compilation_units)

let params_subroutine (ss : Dwarf.sdt_subroutine) =
  let name = maybe_name ss.ss_name in
  let kind1 =
    match ss.ss_kind with SSK_subprogram -> "" | SSK_inlined_subroutine -> "(inlined)"
  in
  match ss.ss_entry_address with
  | None -> None
  | Some addr ->
      let vars =
        List.filter
          (function
            | (svfp : Dwarf.sdt_variable_or_formal_parameter) -> svfp.svfp_kind = SVPK_param)
          ss.ss_vars
      in
      Some (addr, (name, vars))

let params_compilation_unit (cu : Dwarf.sdt_compilation_unit) =
  List.filter_map params_subroutine cu.scu_subroutines

let params_dwarf (sdt_d : Dwarf.sdt_dwarf) =
  List.flatten (List.map params_compilation_unit sdt_d.sd_compilation_units)

let pp_context context = String.concat ":" context

let pp_vars (vars : (Dwarf.sdt_variable_or_formal_parameter * string list) list) : string =
  String.concat ""
    (List.map
       (function
         | (var, context) ->
             pp_context context ^ "\n" ^ pp_sdt_concise_variable_or_formal_parameter 1 false var)
       vars)

let pp_ranged_var (prefix : string) (var : ranged_var) : string =
  let ((n1, n2, ops), (svfp, context)) = var in
  prefix
  ^ pp_sdt_concise_variable_or_formal_parameter_main 0 svfp
  ^ (let s = Dwarf.pp_parsed_single_location_description (Nat_big_num.of_int 0) (n1, n2, ops) in
     String.sub s 0 (String.length s - 1))
  (*hackish stripping of trailing \n from linksem - TODO: fix linksem interface*)
  ^ " "
  ^ pp_context context
  ^ ( match svfp.svfp_decl with
    | None -> ""
    | Some ((ufe, line, subprogram_name) as ud) -> ":" ^ string_of_int line
    )
  ^ "\n"

let pp_ranged_vars (prefix : string) (vars : ranged_var list) : string =
  String.concat "" (List.map (pp_ranged_var prefix) vars)

let compare_pc_ranges (((n1, n2, ops) as pc_range), var) (((n1', n2', ops') as pc_range'), var') =
  compare n1 n1'

let local_by_pc_ranges (((svfp : Dwarf.sdt_variable_or_formal_parameter), context) as var) :
    ranged_var list =
  List.map
    (function (n1, n2, ops) as pc_range -> (pc_range, var))
    (match svfp.svfp_locations with Some locs -> locs | None -> [])

let locals_by_pc_ranges
    (vars : (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list) :
    ranged_var list =
  List.stable_sort compare_pc_ranges (List.flatten (List.map local_by_pc_ranges vars))

(* TODO: sometimes an absence of location list means it doesn't exist at runtime, and sometimes it uses the enclosing PC range in some way? *)

let partition_first g xs =
  let rec partition_first' g xs acc =
    match xs with
    | [] -> (List.rev acc, [])
    | x :: xs' -> if g x then partition_first' g xs' (x :: acc) else (List.rev acc, x :: xs')
  in
  partition_first' g xs []

let mk_ranged_vars_at_instructions (sdt_d : Dwarf.sdt_dwarf) instructions :
    ranged_vars_at_instructions =
  let locals = locals_dwarf sdt_d in
  let locals_by_pc_ranges : ranged_var list = locals_by_pc_ranges locals in

  let size = Array.length instructions in
  let rvai_current = Array.make size [] in
  let rvai_new = Array.make size [] in
  let rvai_old = Array.make size [] in
  let rvai_remaining = Array.make size [] in

  let rec f (addr_prev : addr) (prev : ranged_var list) (remaining : ranged_var list) (k : index)
      =
    if k >= size then ()
    else
      let addr = instructions.(k).i_addr in
      if not (Nat_big_num.less addr_prev addr) then
        Warn.fatal "mk_ranged_vars_at_instructions found non-increasing address %s" (pp_addr addr);
      let (still_current, old) =
        List.partition (function ((n1, n2, ops), var) as rv -> Nat_big_num.less addr n2) prev
      in
      let (new', remaining') =
        partition_first
          (function ((n1, n2, ops), var) as rv -> Nat_big_num.greater_equal addr n1)
          remaining
      in
      (* TODO: do we need to drop any that have been totally skipped over? *)
      let current = still_current @ new' in
      rvai_current.(k) <- current;
      rvai_new.(k) <- new';
      rvai_old.(k) <- old;
      rvai_remaining.(k) <- remaining';
      f addr current remaining' (k + 1)
  in
  f (Nat_big_num.of_int 0) [] locals_by_pc_ranges 0;

  {
    rvai_globals = globals_dwarf sdt_d;
    rvai_params = params_dwarf sdt_d;
    rvai_current;
    rvai_new;
    rvai_old;
    rvai_remaining;
  }

(*   
let local_locals (vars: ranged_var list) instructions  : ranged_vars_at_locations
   *)

(*****************************************************************************)
(*        extract inlining data                                              *)
(*****************************************************************************)

let mk_inlining test sdt instructions =
  (* compute the inlining data *)
  (*let iss = Dwarf.analyse_inlined_subroutines test.dwarf_static.ds_dwarf in*)
  let iss = Dwarf.analyse_inlined_subroutines_sdt_dwarf sdt in
  let issr = Dwarf.analyse_inlined_subroutines_by_range iss in

  (* walk over instructions annotating with inlining data *)
  let rec f issr_current issr_rest label_last max_labels k acc =
    if k = Array.length instructions then (List.rev_append acc [], max_labels)
    else
      let i = instructions.(k) in
      let addr = i.i_addr in
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

      f issr_current' issr_rest' label_last' max_labels' (k + 1) acc'
  in

  let (inlining_list, max_labels) = f [] issr 25 0 0 [] in
  let inlining = Array.of_list inlining_list in

  let pp_inlining_label_prefix s = s ^ String.make (max_labels - String.length s) ' ' ^ " " in

  (inlining, pp_inlining_label_prefix)

(*****************************************************************************)
(*        collect test analysis                                              *)
(*****************************************************************************)

let mk_analysis test filename_objdump_d filename_branch_table =
  (* compute the basic control-flow data *)
  let (instructions, index_of_address, address_of_index) =
    time "mk_instructions" (mk_instructions test filename_objdump_d) filename_branch_table
  in

  let line_info =
    time "mk_line_info" (mk_line_info test.dwarf_static.ds_evaluated_line_info) instructions
  in

  let indirect_branches = time "mk_indirect_branches" mk_indirect_branches instructions in

  let come_froms = time "mk_come_froms" mk_come_froms instructions in

  let sdt =
    time "mk_sdt_dwarf"
      (Dwarf.mk_sdt_dwarf test.dwarf_static.ds_dwarf)
      test.dwarf_static.ds_subprogram_line_extents
  in

  let ranged_vars_at_instructions =
    time "mk_ranged_vars_at_instructions" (mk_ranged_vars_at_instructions sdt) instructions
  in

  let elf_symbols = time "mk_elf_symbols" (mk_elf_symbols test) instructions in

  let frame_info = time "mk_frame_info" (mk_frame_info test) instructions in

  (*Printf.printf  "%s" (pp_indirect_branches indirect_branches); flush stdout;*)
  let (inlining, pp_inlining_label_prefix) =
    time "mk_inlining" (mk_inlining test sdt) instructions
  in

  let acf_width = 60 in
  let max_branch_distance = None (* Some instruction_count, or None for unlimited *) in
  let (rendered_control_flow, rendered_control_flow_inbetweens, rendered_control_flow_width) =
    time "render_ascii_control_flow"
      (render_ascii_control_flow max_branch_distance acf_width)
      instructions
  in

  let an =
    {
      index_of_address;
      address_of_index;
      instructions;
      line_info;
      elf_symbols;
      (*      objdump_lines;*)
      frame_info;
      indirect_branches;
      come_froms;
      sdt;
      ranged_vars_at_instructions;
      inlining;
      pp_inlining_label_prefix;
      rendered_control_flow;
      rendered_control_flow_inbetweens;
      rendered_control_flow_width;
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
  last_var_info := ([] : string list);
  last_source_info := ""

let pp_instruction test an k i =
  (* the come_froms for this instruction, calculated first to determine whether this is the start of a basic block *)
  let addr = i.i_addr in
  let come_froms' =
    List.filter (function cf -> cf.cf_target_kind <> T_plain_successor) an.come_froms.(k)
  in

  (* the inlining for this instruction *)
  let (ppd_labels, ppd_new_inlining) = an.inlining.(k) in

  (* the elf symbols at this address, if any (and reset the last_var_info if any) *)
  let elf_symbols = an.elf_symbols.(k) in
  (match elf_symbols with [] -> () | _ -> last_var_info := []);

  (* is this the start of a basic block? *)
  ( if come_froms' <> [] || elf_symbols <> [] then
    an.pp_inlining_label_prefix "" ^ an.rendered_control_flow_inbetweens.(k) ^ "\n"
  else ""
  )
  ^ String.concat ""
      (List.mapi
         (fun (j : int) (s : string) ->
           (if j = 0 then "**" else "  ") ^ pp_addr addr ^ " <" ^ s ^ ">:\n")
         (let (syms_nostar, syms_star) =
            List.partition (fun s -> not (String.contains s '$')) elf_symbols
          in
          syms_nostar @ syms_star))
  (* function parameters at this address *)
  ^
  let pp_params addr params =
    match List.assoc_opt addr params with
    | None -> ""
    | Some (name, vars) -> (
        "+ " ^ name ^ " params:"
        ^
        match vars with
        | [] -> " none\n"
        | _ ->
            "\n"
            ^ String.concat ""
                (List.map (pp_sdt_concise_variable_or_formal_parameter 0 true) vars)
      )
  in
  pp_params addr an.ranged_vars_at_instructions.rvai_params
  (* the new inlining info for this address *)
  ^ ppd_new_inlining
  (* the source file lines (if any) associated to this address *)
  (* OLD VERSION *)
  (*  ^ begin
      if !Globals.show_source then
        let source_info =
          match pp_dwarf_source_file_lines () test.dwarf_static true addr with
          | Some s ->
              (* the inlining label prefix *)
              an.pp_inlining_label_prefix ppd_labels
              ^ an.rendered_control_flow_inbetweens.(k)
              ^ s ^ "\n"
          | None -> ""
        in
        if source_info = !last_source_info then "" (*"unchanged\n"*)
        else (
          last_source_info := source_info;
          source_info
        )
      else ""
    end
    *)
  (* NEW VERSION *)
  ^ begin
      let pp_line multiple elifi =
        an.pp_inlining_label_prefix ppd_labels
        ^ an.rendered_control_flow_inbetweens.(k)
        ^ ""
        ^ pp_dwarf_source_file_lines' test.dwarf_static !Globals.show_source multiple elifi
        ^ "\n"
      in
      (* if there's just a single entry, suppress iff it's a non-start entry, but if there are (confusingly) multiple, show all *)
      match an.line_info.(k) with
      | [] -> ""
      | [elifi] -> if elifi.elifi_start then pp_line false elifi else ""
      | elifis -> String.concat "" (List.map (pp_line true) elifis)
    end
  (* the frame info for this address *)
  ^ begin
      if !Globals.show_cfa then
        let frame_info = pp_frame_info an.frame_info k in
        if frame_info = !last_frame_info then "" (*"CFA: unchanged\n"*)
        else (
          last_frame_info := frame_info;
          (* the inlining label prefix *)
          an.pp_inlining_label_prefix ppd_labels
          ^ an.rendered_control_flow_inbetweens.(k)
          ^ frame_info
        )
      else ""
    end
  (* the variables whose location ranges include this address - old version*)
  (*  ^ begin
      if (*true*) !Globals.show_vars then (
        let als_old = !last_var_info in
        let als_new (*fald*) = Dwarf.filtered_analysed_location_data test.dwarf_static addr in
        last_var_info := als_new;
        Dwarf.pp_analysed_location_data_diff test.dwarf_static.ds_dwarf als_old als_new
      )
      else ""
    end
  ^ "\n"
 *)
  (* the variables whose location ranges include this address - new version*)
  ^ begin
      if !Globals.show_vars then pp_ranged_vars "+" an.ranged_vars_at_instructions.rvai_new.(k)
        (*        ^ pp_ranged_vars "C" an.ranged_vars_at_instructions.rvai_current.(k)*)
        (*        ^ pp_ranged_vars "R" an.ranged_vars_at_instructions.rvai_remaining.(k)*)
      else ""
    end
  (* the inlining label prefix *)
  ^ an.pp_inlining_label_prefix ppd_labels
  (* the rendered control flow *)
  ^ an.rendered_control_flow.(k)
  (* the address and (hex) instruction *)
  ^ pp_addr addr
  ^ ":  "
  ^ pp_opcode_bytes test.arch i.i_opcode
  (* the dissassembly from objdump *)
  ^ "  "
  ^ i.i_mnemonic ^ "\t" ^ i.i_operands
  (* any indirect-branch control flow from this instruction *)
  ^ begin
      match i.i_control_flow with
      | C_branch_register _ ->
          " -> "
          ^ String.concat ","
              (List.map
                 (function
                   | (tk, a', k', s) -> pp_target_addr_wrt addr i.i_control_flow a' ^ "" ^ s ^ "")
                 i.i_targets)
          ^ " "
      | _ -> ""
    end
  (* any control flow to this instruction *)
  ^ pp_come_froms addr come_froms'
  ^ "\n"
  ^
  if (*true*) !Globals.show_vars then
    if k < Array.length an.instructions - 1 then
      pp_ranged_vars "-" an.ranged_vars_at_instructions.rvai_old.(k + 1)
    else ""
  else ""

(*****************************************************************************)
(*        pretty-print test analysis                                         *)
(*****************************************************************************)

let pp_test_analysis test an =
  "* ************* globals *****************\n"
  ^ pp_vars an.ranged_vars_at_instructions.rvai_globals
  (*  ^ "************** locals *****************\n"
  ^ pp_ranged_vars
 *)
  ^ "\n* ************* instructions *****************\n"
  ^ ( pp_instruction_init ();
      String.concat "" (Array.to_list (Array.mapi (pp_instruction test an) an.instructions))
    )
  ^ "* ************* struct/union/enum type definitions *****************\n"
  ^ (let d = test.dwarf_static.ds_dwarf in

     (*     let c = Dwarf.p_context_of_d d in
     Dwarf.pp_all_aggregate_types c d*)
     (*     Dwarf.pp_all_struct_union_enum_types' d)*)
     let ctyps : Dwarf.c_type list = Dwarf.struct_union_enum_types d in
     String.concat "" (List.map Dwarf.pp_struct_union_type_defn' ctyps))
  (*  ^ "\n* ************* branch targets *****************\n"*)
  (*  ^ pp_branch_targets instructions*)
  ^ "\n* ************* call graph *****************\n"
  ^ pp_call_graph test
      ( (*instructions,*)
        an.instructions,
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
    match !Globals.elf with Some s -> s | None -> Warn.fatal "no --elf option\n"
  in

  let filename_objdump_d =
    match !Globals.objdump_d with Some s -> s | None -> Warn.fatal "no --objdump-d option\n"
  in

  let filename_branch_tables =
    match !Globals.branch_table_data_file with
    | Some s -> s
    | None -> Warn.fatal "no --branch-tables option\n"
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
  let test = time "parse_elf_file" parse_elf_file filename_elf in

  let an = time "mk_analysis" (mk_analysis test filename_objdump_d) filename_branch_tables in

  match (!Globals.elf2, !Globals.objdump_d2, !Globals.branch_table_data_file2) with
  | (None, _, _) -> (
      (* output CFG dot file *)
      ( match !Globals.cfg_dot_file with
      | Some cfg_dot_file ->
          let start_indices =
            array_indices_such_that (function ss -> ss <> []) an.elf_symbols
          in
          let graph = mk_cfg test an "" false false start_indices in
          (*            let graph' = reachable_subgraph graph ["mpool_fini"] in*)
          pp_cfg graph cfg_dot_file
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

      Printf.fprintf c "%s" (time "pp_test_analysis" pp_test_analysis test an);

      match filename_out_file_option with Some f -> close_out c | None -> ()
    )
  | (Some filename_elf2, Some filename_objdump_d2, Some filename_branch_tables2) -> (
      match !Globals.cfg_dot_file with
      | Some cfg_dot_file ->
          let test2 = parse_elf_file filename_elf2 in

          let an2 = mk_analysis test2 filename_objdump_d2 filename_branch_tables2 in

          let parse_source_node_list test an (so : string option) : index list =
            match so with
            | None -> []
            | Some s ->
                List.map
                  (function
                    | s' ->
                        an.index_of_address
                          ( match address_of_elf_symbol test s' with
                          | Some addr -> addr
                          | None -> Warn.fatal "address_of_elf_symbol failed on \"%s\"\n" s'
                          ))
                  (String.split_on_char ' ' s)
          in

          let all_elf_symbol_indices an =
            array_indices_such_that (function ss -> ss <> []) an.elf_symbols
          in

          let start_indices =
            match parse_source_node_list test an !Globals.cfg_source_nodes with
            | [] -> all_elf_symbol_indices an
            | cfg_source_node_list0 -> cfg_source_node_list0
          in

          let start_indices2 =
            match parse_source_node_list test2 an2 !Globals.cfg_source_nodes2 with
            | [] -> all_elf_symbol_indices an2
            | cfg_source_node_list2 -> cfg_source_node_list2
          in

          let graph0 = mk_cfg test an "O0_" false true start_indices in

          let graph2 = mk_cfg test2 an2 "O2_" false false start_indices2 in

          let graph = graph_cfg_union graph0 graph2 in

          let graph' =
            correlate_source_line test an.line_info graph0 test2 an2.line_info graph2
          in

          let cfg_dot_file_root = String.sub cfg_dot_file 0 (String.length cfg_dot_file - 4) in
          let cfg_dot_file_base = cfg_dot_file_root ^ "_base.dot" in
          let cfg_dot_file_layout = cfg_dot_file_root ^ "_layout.dot" in
          pp_cfg graph cfg_dot_file_base;
          let status =
            Unix.system ("dot -Txdot " ^ cfg_dot_file_base ^ " > " ^ cfg_dot_file_layout)
          in
          let layout_lines =
            match read_file_lines cfg_dot_file_layout with
            | Ok lines -> lines
            | MyFail s -> Warn.fatal "couldn't read cfg_dot_file_layout %s" s
          in
          let ppd_correlate_edges = List.map (pp_edge "black") graph'.gc_edges in
          let c = open_out cfg_dot_file in
          Array.iteri
            (function
              | j -> (
                  function
                  | line ->
                      if j + 1 = Array.length layout_lines then ()
                      else Printf.fprintf c "%s\n" layout_lines.(j)
                ))
            layout_lines;
          List.iter (function line -> Printf.fprintf c "%s\n" line) ppd_correlate_edges;
          Printf.fprintf c "}\n";
          close_out c;
          let status =
            Unix.system ("dot -Tpdf " ^ cfg_dot_file ^ " > " ^ cfg_dot_file_root ^ ".pdf")
          in
          let status =
            Unix.system ("dot -Tsvg " ^ cfg_dot_file ^ " > " ^ cfg_dot_file_root ^ ".svg")
          in
          ()
      | None -> Warn.fatal "no dot file\n"
    )
  | _ -> Warn.fatal "missing files for elf2\n"
