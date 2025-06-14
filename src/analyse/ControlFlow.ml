(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(*****************************************************************************)
(** compute control-flow abstraction of instructions, from objdump and branch table data *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** basic pp for control-flow abstraction                                    *)
open Utils

open ElfTypes
open ControlFlowTypes

let pp_control_flow_instruction c =
  match c with
  | C_no_instruction -> "no instruction"
  | C_plain -> "plain"
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch (a, s) -> "b" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_and_link (a, s) -> "bl" ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_cond (is, a, s) -> is ^ " " ^ pp_addr a ^ " " ^ s
  | C_branch_register _ -> "br"
  | C_smc_hvc s -> "smc/hvc " ^ s

let pp_control_flow_instruction_short c =
  match c with
  | C_no_instruction -> "no instruction"
  | C_plain -> "plain"
  | C_ret -> "ret"
  | C_eret -> "eret"
  | C_branch _ -> "b"
  | C_branch_and_link _ -> "bl"
  | C_branch_cond (is, _, _) -> is
  | C_branch_register _ -> "br"
  | C_smc_hvc _ -> "smc/hvc"

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
  | T_out_of_range _ -> "out-of-range"

(*****************************************************************************)
(**   find targets of each entry of a branch-table description file          *)

(*****************************************************************************)

let branch_table_target_addresses test filename_branch_table_option : (addr * addr list) list =
  (* read in and parse branch-table description file *)
  let branch_data :
      (natural (*a_br*) * (natural (*a_table*) * natural (*size*) * string (*shift*) * natural))
      (*offset*)
      list =
    match filename_branch_table_option with
    | None -> []
    | Some filename_branch_table -> (
        match read_file_lines filename_branch_table with
        | Error s ->
            fatal "%s\ncouldn't read branch table data file: \"%s\"\n" s filename_branch_table
        | Ok lines ->
            let parse_line (s : string) :
                (natural * (natural * natural * string * natural)) option =
              match
                Scanf.sscanf s " %x: %x %x %s %x #" (fun a_br a_table n shift a_offset ->
                    (a_br, a_table, n, shift, a_offset))
              with
              | (a_br, a_table, n, shift, a_offset) ->
                  Some
                    ( Sym.of_int a_br,
                      ( Sym.of_int a_table,
                        Sym.of_int n,
                        shift,
                        Sym.of_int a_offset ) )
              | exception _ -> fatal "couldn't parse branch table data file line: \"%s\"\n" s
            in
            List.filter_map parse_line (List.tl (Array.to_list lines))
      )
  in

  (* pull out .rodata section from ELF *)
  let ((_, rodata_addr, bs) as _rodata : Dwarf.p_context * Sym.t * BytesSeq.t) =
    Dwarf.extract_section_body_without_relocations test.elf_file ".rodata" false
  in
  (* chop into bytes *)
  let rodata_bytes : char array = BytesSeq.to_array bs in

  (* chop into 4-byte words - as needed for branch offset tables,
     though not for all other things in .rodata *)
  let rodata_words : (natural * natural) list =
    Dwarf.words_of_sym_byte_sequence rodata_addr (Dwarf_byte_sequence.sym_bs_construct bs (Pmap.empty Nat_big_num.compare)) [] in (*HACK*)

  let read_rodata_b addr =
    Dwarf.sym_natural_of_byte
      rodata_bytes.(Sym.to_int (Sym.sub addr rodata_addr))
  in
  let read_rodata_h addr =
    Sym.add (read_rodata_b addr)
      (Sym.mul (Sym.of_int 256)
         (read_rodata_b (Sym.add addr (Sym.of_int 1))))
  in

  let sign_extend_W n =
    let half = Sym.mul (Sym.of_int 65536) (Sym.of_int 32768) in
    let whole = Sym.mul half (Sym.of_int 2) in
    if Sym.greater_equal n half then Sym.sub n whole else n
  in

  let read_rodata_W addr =
    sign_extend_W
      (Sym.add (read_rodata_b addr)
         (Sym.add
            (Sym.mul (Sym.of_int 256)
               (read_rodata_b (Sym.add addr (Sym.of_int 1))))
            (Sym.add
               (Sym.mul (Sym.of_int 65536)
                  (read_rodata_b (Sym.add addr (Sym.of_int 2))))
               (Sym.mul (Sym.of_int 16777216)
                  (read_rodata_b (Sym.add addr (Sym.of_int 3)))))))
  in

  let rec natural_assoc_opt n nys =
    match nys with
    | [] -> None
    | (n', y) :: nys' -> if Sym.equal n n' then Some y else natural_assoc_opt n nys'
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
  let rec eval_shift_expression (shift : string) (a_table : Sym.t)
      (a_offset : Sym.t) (i : Sym.t) (stack : Sym.t list) (pc : int)
      =
    if pc = String.length shift then
      match stack with
      | [a] -> a
      | _ -> fatal "eval_shift_expression terminated with non-singleton stack"
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
              Sym.mul a
                (Sym.pow_int_positive 2 (Char.code command - Char.code '0'))
            in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> fatal "eval_shift_expression shift empty stack"
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
            let a' = Sym.add a1 a2 in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> fatal "eval_shift_expression plus emptyish stack"
      else if command = 'b' then
        (* read byte from branch table *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_b a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> fatal "eval_shift_expression b empty stack"
      else if command = 'h' then
        (* read halfword from branch table *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_h a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> fatal "eval_shift_expression h empty stack"
      else if command = 'W' then
        (* read word from branch table and sign-extend *)
        match stack with
        | a :: stack' ->
            let a' = read_rodata_W a in
            eval_shift_expression shift a_table a_offset i (a' :: stack') (pc + 1)
        | _ -> fatal "eval_shift_expression W empty stack"
      else fatal "eval_shift_expression unknown command"
  in

  let branch_table_target_addresses =
    List.map
      (function
        | (a_br, (a_table, size, shift, a_offset)) ->
            let rec f i =
              if i > Sym.to_int size then []
              else
                let a_target =
                  if shift = "2" then
                    let table_entry_addr = Sym.add a_table (Sym.of_int (4 * i)) in
                    match natural_assoc_opt table_entry_addr rodata_words with
                    | None ->
                        fatal "no branch table entry for address %s\n" (pp_addr table_entry_addr)
                    | Some table_entry ->
                        let a_target =
                          Sym.modulus
                            (Sym.add a_table table_entry)
                            (Sym.pow_int_positive 2 32)
                        in
                        (* that 32 is good for the sign-extended negative 32-bit offsets we see
                           in the old hafnium-playground-src branch tables *)
                        a_target
                  else eval_shift_expression shift a_table a_offset (Sym.of_int i) [] 0
                in
                a_target :: f (i + 1)
            in
            (a_br, f 0))
      branch_data
  in
  branch_table_target_addresses

(*****************************************************************************)
(**  parse control-flow instruction asm from objdump                         *)

(*****************************************************************************)

(* hacky parsing of AArch64 assembly from objdump -d to identify control-flow instructions and their arguments *)

let parse_addr (s : string) : natural =
try 
  Scanf.sscanf s "0x%Lx" (fun i64 -> Sym.of_int64 i64)
with
  (Scanf.Scan_failure _ | End_of_file)  ->
   Scanf.sscanf s "%Lx" (fun i64 -> Sym.of_int64 i64)

let parse_target base s =
  match Scanf.sscanf s " %s %s" (fun s1 s2 -> (s1, s2)) with
  | (s1, s2) -> Some (Sym.add base (parse_addr s1), s2)
  | exception _ -> None

let parse_drop_one s =
  match
    Scanf.sscanf s " %s %n" (fun s1 n ->
        let s' = String.sub s n (String.length s - n) in
        (s1, s'))
  with
  | (_, s') -> Some s'
  | exception _ -> None

let parse_relocation_target symbol_map s =
  let s, offset = match String.split_on_char '+' s with
  | [s1; s2] ->
      s1, Scanf.sscanf s2 "0x%x" Fun.id
  | [s] -> s, 0
  | _ -> fatal "Unable to parse relocation target '%s'" s
  in
  let addr = List.find_map (fun (name, (_,_,addr,_,_)) -> if name = s then Some addr else None) symbol_map in
  Option.map (Sym.add (Sym.of_int offset)) addr

let parse_control_flow_instruction symbol_map base s mnemonic s' relocation : control_flow_insn =
  let relocation_target = Option.bind relocation (fun (_typ, target) ->
    Option.map (fun a -> (a, target)) (parse_relocation_target symbol_map target)
  ) in
  (*   Printf.printf "s=\"%s\" mnemonic=\"%s\"  mnemonic chars=\"%s\" s'=\"%s\"   "s mnemonic (String.concat "," (List.map (function c -> string_of_int (Char.code c)) (char_list_of_string mnemonic))) s';flush stdout;*)
  let c =
    if List.mem String.equal mnemonic [".word"] then C_no_instruction
    else if List.mem String.equal mnemonic ["ret"] then C_ret
    else if List.mem String.equal mnemonic ["eret"] then C_eret
    else if List.mem String.equal mnemonic ["br"] then C_branch_register mnemonic
    else if
      (String.length mnemonic >= 2 && String.sub mnemonic 0 2 = "b.")
      || List.mem String.equal mnemonic ["b"; "bl"]
    then
      match parse_target base s', relocation_target with
      | None, None -> raise (Failure ("b./b/bl parse error for: \"" ^ s ^ "\"\n"))
      | _, Some(a, s) | Some (a, s), None ->
          if mnemonic = "b" then C_branch (a, s)
          else if mnemonic = "bl" then C_branch_and_link (a, s)
          else C_branch_cond (mnemonic, a, s)
    else if List.mem String.equal mnemonic ["cbz"; "cbnz"] then
      match parse_drop_one s' with
      | None -> raise (Failure ("cbz/cbnz 1 parse error for: " ^ s ^ "\n"))
      | Some s' -> (
          match parse_target base s', relocation_target with
          | None, None -> raise (Failure ("cbz/cbnz 2 parse error for: " ^ s ^ "\n"))
          | _, Some(a, s) | Some (a, s), None -> C_branch_cond (mnemonic, a, s)
        )
    else if List.mem String.equal mnemonic ["tbz"; "tbnz"] then
      match parse_drop_one s' with
      | None -> raise (Failure ("tbz/tbnz 1 parse error for: " ^ s ^ "\n"))
      | Some s'' -> (
          match parse_drop_one s'' with
          | None -> raise (Failure ("tbz/tbnz 2 parse error for: " ^ s ^ "\n"))
          | Some s''' -> (
              match parse_target base s''', relocation_target with
              | None, None -> raise (Failure ("tbz/tbnz 3 parse error for: " ^ s ^ "\n"))
              | _, Some(a, s'''') | Some (a, s''''), None ->
                  (*                Printf.printf "s=%s mnemonic=%s s'=%s s''=%s s'''=%s s''''=%s\n"s mnemonic s' s'' s''' s'''';*)
                  C_branch_cond (mnemonic, a, s'''')
            )
        )
    else if List.mem String.equal mnemonic ["smc"; "hvc"] then C_smc_hvc s'
    else C_plain
  in
  (*Printf.printf "%s\n" (pp_control_flow_instruction c);*)
  c

(*****************************************************************************)
(**  compute targets of an instruction                                       *)

(*****************************************************************************)

let targets_of_control_flow_insn_without_index branch_table_targets (addr : natural)
    (opcode_bytes : int list) (c : control_flow_insn) : (target_kind * addr * string) list =
  let succ_addr = Sym.add addr (Sym.of_int (List.length opcode_bytes)) in
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
        if List.mem String.equal s ["<abort>"; "<panic>"; "<__stack_chk_fail>"] then
          [(T_branch_and_link_call_noreturn, a, s)]
        else
          [(T_branch_and_link_call, a, s); (T_branch_and_link_successor, succ_addr, "<return>")]
    | C_branch_cond (_is, a, s) ->
        [(T_branch_cond_branch, a, s); (T_branch_cond_successor, succ_addr, "<fallthrough>")]
    | C_branch_register _ ->
        let addresses =
          try List.assoc addr branch_table_targets
          with Not_found -> (
            match branch_table_targets with
            | [] ->
                warn
                  "branch-register instruction used, but no branch_table_targets - defaulting to \
                   just this address, which will give the wrong control-flow data";
                [addr]
            | _ ->
                fatal "targets_of_control_flow_insn_without_index C_branch_register fail for %s\n"
                  (pp_addr addr)
          )
        in
        List.mapi
          (function
            | i -> (
                function
                | a_target -> (T_branch_register, a_target, "<indirect" ^ string_of_int i ^ ">")
              ))
          addresses
    | C_smc_hvc _ -> [(T_smc_hvc_successor, succ_addr, "<C_smc_hvc successor>")]
  in

  targets

let targets_of_control_flow_insn index_option_of_address branch_table_targets (addr : natural)
    (opcode_bytes : int list) (c : control_flow_insn) : target list =
  (*Printf.printf "targets_of_control_flow_insn addr=%s\n" (pp_addr addr); flush stdout;*)
  List.map
    (function
      | (tk, a'', s'') -> (
          (*       Printf.printf "%s" ("foo " ^ pp_addr addr ^ " " ^ pp_control_flow_instruction c ^ " " ^ pp_target_kind_short tk ^ " " ^ pp_addr a'' ^ " " ^ s'' ^ "\n");*)
          match index_option_of_address a'' with
          | Some i -> (tk, a'', i, s'')
          | None -> (T_out_of_range a'', a'', 0 (*dummy*), s'')
        ))
    (targets_of_control_flow_insn_without_index branch_table_targets addr opcode_bytes c)

let pp_opcode_bytes arch (opcode_bytes : int list) : string =
  match arch with
  | AArch64 -> String.concat "" (List.map (function b -> Printf.sprintf "%02x" b) opcode_bytes)
  | X86 -> String.concat " " (List.map (function b -> Printf.sprintf "%02x" b) opcode_bytes)

(*****************************************************************************)
(**       pull disassembly out of an objdump -d file                         *)

(*****************************************************************************)

(* objdump -d output format, from GNU objdump (GNU Binutils for Ubuntu) 2.30:
x86:
  400150:\t48 83 ec 10          \tsub    $0x10,%rsp
  400160:	48 8d 05 99 0e 20 00 	lea    0x200e99(%rip),%rax        # 601000 <x>

AArch64:
   10000:\t90000088 \tadrp\tx8, 20000 <x>
   10004:	52800129 	mov	w9, #0x9                   	// #9
 *)

let relocation_regexp_string = "[ \t][0-9a-fA-F]+:[ \t]\\([0-9A-Z_]+\\)\t\\(.*\\)"

let objdump_line_regexp =
  Str.regexp (" *\\([0-9a-fA-F]+\\):[ \t]\\([0-9a-fA-F ]+\\)\t\\([^ \r\t\n]+\\)[ \t]*\\([^:]*\\)\\(" ^ relocation_regexp_string ^ "\\)?$")

let section_start_line_regexp =
  Str.regexp "Disassembly of section \\(.*\\):$"

type relocation = string * string

type raw_objdump_instruction =
  int64 (*address*) * int list (*opcode bytes*) * string (*mnemonic*) * string * relocation option

type objdump_instruction =
  natural (*address*) * int list (*opcode bytes*) * string (*mnemonic*) * string * relocation option

(*args etc*)

let parse_section_start s =
  if Str.string_match section_start_line_regexp s 0 then
    Some (Str.matched_group 1 s)
  else
    None

let parse_objdump_line (s : string) : raw_objdump_instruction option =
  let parse_hex_int64 s' =
    try Scanf.sscanf s' "%Lx" (fun i64 -> i64)
    with _ -> fatal "cannot parse address in objdump line %s\n" s
  in
  let parse_hex_int s' =
    try Scanf.sscanf s' "%x" (fun i -> i)
    with _ -> fatal "cannot parse opcode '%s' byte in objdump line %s\n" s' s
  in
  let rec strip_whitespace s =
    if s = "" then ""
    else
      let tail = String.sub s 1 ((String.length s) - 1) in
      match String.get s 0 with
      | ' ' | '\n' | '\r' | '\t' -> strip_whitespace tail
      | c -> String.make 1 c ^ (strip_whitespace tail)
  in
  if Str.string_match objdump_line_regexp s 0 then
    begin
      (* debug "matched line"; *)
      let addr_int64 = parse_hex_int64 (Str.matched_group 1 s) in
      let op = Str.matched_group 2 s in
      let op = strip_whitespace op in
      let opcode_byte_strings =
        [String.sub op 0 2;
         String.sub op 2 2;
         String.sub op 4 2;
         String.sub op 6 2]
      in
      let opcode_bytes = List.map parse_hex_int opcode_byte_strings in
      let mnemonic = Str.matched_group 3 s in
      let operands = Str.matched_group 4 s in
      let relocation = try
        Some (Str.matched_group 6 s, Str.matched_group 7 s)
      with
      | Not_found -> None
      in
      Some (addr_int64, opcode_bytes, mnemonic, operands, relocation)
    end
  else None

(* let parse_objdump_relocation (s : string) : (string * string) option =
  let parse_hex_int s' =
    try Scanf.sscanf s' "%x" (fun i -> i)
    with _ -> fatal "cannot parse relocation '%s' in objdump line %s\n" s' s
  in
  if Str.string_match objdump_line_regexp s 0 then
    begin
      let addr = Str.matched_group 1 s in
      let op = Str.matched_group 2 s in
      let op = strip_whitespace op in
      let opcode_byte_strings =
        [String.sub op 0 2;
         String.sub op 2 2;
         String.sub op 4 2;
         String.sub op 6 2]
      in
      let opcode_bytes = List.map parse_hex_int opcode_byte_strings in
      Some (addr, op)
    end
  else None *)

(*
let parse_objdump_lines arch lines : objdump_instruction list =
  List.filter_map (parse_objdump_line arch) (Array.to_list lines)
 *)

let with_symbolic_address (section: string) (addr, opcode_bytes, mnemonic, operands, relocation) : objdump_instruction =
  (Sym_ocaml.Num.Offset (section, Nat_big_num.of_int64 addr), opcode_bytes, mnemonic, operands, relocation)

let rec parse_objdump_lines arch lines (next_index : int) (last_address : int64 option) (section: string option) :
    objdump_instruction list =
  if next_index >= Array.length lines then []
  else
    let section = Option.fold ~none:section ~some:Option.some @@ parse_section_start lines.(next_index) in
    match parse_objdump_line lines.(next_index) with
    (* skip over unparseable lines *)
    | None -> parse_objdump_lines arch lines (next_index + 1) last_address section
    | Some ((addr, _opcode_bytes, _mnemonic, _operands, _relocation) as i) -> (
        let mki = with_symbolic_address (Option.get section) in
        match last_address with
        | None -> mki i :: parse_objdump_lines arch lines (next_index + 1) (Some addr) section
        | Some last_address' ->
            let last_address'' = Int64.add last_address' (Int64.of_int 4) in
            if addr > last_address'' then
              (* fake up "missing" instructions for any gaps in the address space*)
              (*warn "gap in objdump instruction address sequence at %s" (pp_addr last_address'');*)
              mki (last_address'', [], "missing", "", None)
              :: parse_objdump_lines arch lines next_index (Some last_address'') section
            else mki i :: parse_objdump_lines arch lines (next_index + 1) (Some addr) section
      )

let parse_objdump_file arch filename_objdump_d : objdump_instruction array =
  match read_file_lines filename_objdump_d with
  | Error s -> fatal "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename_objdump_d
  | Ok lines -> Array.of_list (parse_objdump_lines arch lines 0 None None)

(*****************************************************************************)
(**  parse control-flow instruction asm from objdump and branch table data   *)

(*****************************************************************************)

let mk_instructions test filename_objdump_d filename_branch_table_option :
    instruction array * (addr -> index) * (addr -> index option) * (index -> addr) =
  let objdump_instructions : objdump_instruction array =
    parse_objdump_file test.arch filename_objdump_d
  in

  (* TODO: check the objdump opcode_bytes against the ELF *)
  let branch_table_targets : (addr * addr list) list =
    branch_table_target_addresses test filename_branch_table_option
  in

  let (index_of_address, index_option_of_address) =
    let tbl = Hashtbl.create (Array.length objdump_instructions) in
    Array.iteri
      (function
        | k -> (
            function (addr, _, _, _, _) -> Hashtbl.add tbl addr k
          ))
      objdump_instructions;
    ( (function
      | addr -> (
          try Hashtbl.find tbl addr
          with _ -> fatal "index_of_address didn't find %s\n" (pp_addr addr)
        )),
      function
      | addr -> (
          try Some (Hashtbl.find tbl addr) with _ -> None
        ) )
  in

  let instructions =
    Array.map
      (function
        | (addr, opcode_bytes, mnemonic, operands, relocation) ->
            (* a bit hacky *)
            let base = match addr with
            | Sym_ocaml.Num.Offset(s,_) -> Sym_ocaml.Num.section s
            | Sym_ocaml.Num.Absolute(_) -> Sym.of_int 0
            in
            let c : control_flow_insn =
              parse_control_flow_instruction test.symbol_map base ("objdump line " ^ pp_addr addr) mnemonic operands relocation
            in

            let targets =
              targets_of_control_flow_insn index_option_of_address branch_table_targets addr
                opcode_bytes c
            in
            {
              i_addr = addr;
              i_opcode = opcode_bytes;
              i_mnemonic = mnemonic;
              i_operands = operands;
              i_control_flow = c;
              i_targets = targets;
              i_relocation = relocation;
            })
      objdump_instructions
  in

  let address_of_index k = instructions.(k).i_addr in

  Array.sort
    (fun i1 i2 ->
      Sym.Ordered.compare (i1.i_addr) (i2.i_addr))
    instructions;

  (instructions, index_of_address, index_option_of_address, address_of_index)

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
                   (List.map (function (_, a', _, s) -> pp_addr a' ^ "" ^ s ^ "") i.i_targets)
               ^ "\n")
         indirect_branches)

let highlight c =
  match c with
  | C_no_instruction -> false
  | C_plain | C_ret | C_eret | C_branch_and_link (_, _) | C_smc_hvc _ -> false
  | C_branch (_, _) | C_branch_cond (_, _, _) | C_branch_register _ -> true

(* highlight branch targets to earlier addresses*)
let pp_target_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Sym.Ordered.less a addr then "^" else "") ^ pp_addr a

(* highlight branch come-froms from later addresses*)
let pp_come_from_addr_wrt (addr : natural) (c : control_flow_insn) (a : natural) =
  (if highlight c && Sym.Ordered.greater a addr then "v" else "") ^ pp_addr a

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
