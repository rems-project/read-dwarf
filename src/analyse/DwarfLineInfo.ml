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
(**       post-process DWARF source line info                                *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Types
open Utils
open ControlFlowTypes
open ElfTypes
open Printf

(*****************************************************************************)
(**       post-processed DWARF source line info                              *)

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

(* line number sequences can overlap, and we have to walk through instructions (not addresses), so we simplify by splitting all of them into individual entries, sort them by first address, and then walk through them painting a per-instruction array.  This is algorithmically a bit terrible, but seems to add only a couple of seconds to read-dwarf rd *)

let pp_line_number_header_concise (lnh : Dwarf.line_number_header) : string =
  "lnh offset =    " ^ Dwarf.pphex_sym lnh.lnh_offset ^ "\n"

(*^ ("dwarf_format =                       " ^ (pp_dwarf_format lnh.lnh_dwarf_format ^ ("\n"
^ ("unit_length =                        " ^ (Sym.to_string lnh.lnh_unit_length ^ ("\n"
^ ("version =                            " ^ (Sym.to_string lnh.lnh_version ^ ("\n"
^ ("header_length =                      " ^ (Sym.to_string lnh.lnh_header_length ^ ("\n"
^ ("minimum_instruction_length =         " ^ (Sym.to_string lnh.lnh_minimum_instruction_length ^ ("\n"
^ ("maximum_operations_per_instruction = " ^ (Sym.to_string lnh.lnh_maximum_operations_per_instruction ^ ("\n"
^ ("default_is_stmt =                    " ^ (string_of_bool lnh.lnh_default_is_stmt ^ ("\n"
^ ("line_base =                          " ^ (Sym.to_string lnh.lnh_line_base ^ ("\n"
^ ("line_range =                         " ^ (Sym.to_string lnh.lnh_line_range ^ ("\n"
^ ("opcode_base =                        " ^ (Sym.to_string lnh.lnh_opcode_base ^ ("\n"
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
        | _ -> fatal "split_into_sequences found premature end of sequence"
      )
    | lnr :: lnrs' ->
        if lnr.lnr_end_sequence then (
          let first =
            match List.last_opt acc1 with
            | Some lnr_first -> lnr_first.lnr_address
            | None -> fatal "split_into_sequences found sequence of length 0"
          in
          let last = lnr.lnr_address in
          (* print_endline (Dwarf.pphex_sym first ^ " " ^ Dwarf.pphex_sym last); *)
          if Sym.equal first last then fatal "split_into_sequences found first=last"
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
    | l1 :: (l2 :: _ as remaining_lines'') ->
        let elie =
          {
            elie_first = l1.lnr_address;
            elie_last =
              ( if Sym.equal l2.lnr_address l1.lnr_address then l1.lnr_address
              else Sym.sub l2.lnr_address (Sym.of_int 1)
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
  let compare_sequence s1 s2 = Sym.compare s1.elis_first s2.elis_first in
  let sequences_sorted = List.sort compare_sequence sequences in
  (*let overlap_sequence s1 s2 = not( Sym.greater_equal s2.first s1.last || Sym.greater_equal s1.first s2.last) in*)

  Printf.printf "mk_line_info\n%s" (String.concat "\n" (List.map pp_sequence_concise sequences_sorted));

  let size = Array.length instructions in
  let elifis = Array.make size None in

  let next_sequence addr remaining_sequences : ((evaluated_line_info_sequence list) * (evaluated_line_info_sequence list)) =
    let (discardable,remaining') =
      List.partition
        (function sequence ->
           Sym.less_equal sequence.elis_last addr)
        remaining_sequences in
    let (sequences,remaining'') =
      List.partition
        (function sequence ->
           Sym.less_equal sequence.elis_first addr)
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
         if Sym.equal addr l1.lnr_address then
           (* this instruction address exactly matches the first line of this sequence *)
           let elifi = {
               elifi_start = true;
               elifi_lnh = active_sequence.elis_lnh;
               elifi_line = l1 } in
           elifis.(k) <- Some elifi;
           f active_sequence remaining_lines remaining_sequences (k+1)
         else if Sym.less l1.lnr_address addr && Sym.less addr l2.lnr_address then
           (* this instruction address is within the range of the first line, but not equal to it*)
           let elifi = {
               elifi_start = false;
               elifi_lnh = active_sequence.elis_lnh;
               elifi_line = l1 } in
           elifis.(k) <- Some elifi;
           f active_sequence remaining_lines remaining_sequences (k+1)
         else if Sym.greater_equal addr l2.lnr_address then
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
  let compare_sequence s1 s2 = Sym.Ordered.compare s1.elis_first s2.elis_first in
  let sequences_sorted = List.sort compare_sequence sequences in

  let entries = List.flatten (List.map split_into_entries sequences_sorted) in
  let compare_entry e1 e2 = Sym.Ordered.compare e1.elie_first e2.elie_first in
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
            if Sym.Ordered.less_equal elie.elie_first addr then
              mk_new_perhaps_relevant (elie :: acc) remaining'
            else (acc, remaining)
      in

      let (new_perhaps_relevant, remaining') = mk_new_perhaps_relevant [] remaining_entries in

      let addr_in elie =
        Sym.in_range elie.elie_first elie.elie_last addr
      in

      let still_active_entries =
        List.filter addr_in active_entries @ List.filter addr_in new_perhaps_relevant
      in

      elifis.(k) <-
        List.map
          (function
            | elie ->
                let elifi =
                  { elifi_start = Sym.equal addr elie.elie_first; elifi_entry = elie }
                in
                elifi)
          still_active_entries;

      f still_active_entries remaining' (k + 1)
  in

  f [] entries_sorted 0;
  elifis

(*****************************************************************************)
(**       find and pretty-print source lines for addresses                   *)

(*****************************************************************************)

let source_file_cache =
  ref ([] : ((string option * string option * string) * string array option) list)

let actual_directories replacement (comp_diro, dir, file) : string (*directory_original*) * string
    (*directory_replacement*) =
  let ppo a = match a with None -> "none" | Some s -> s in
  debug "actual_directories %s %s %s" (ppo comp_diro) (ppo dir) file;

  (* linksem provides comp_dir as a string option, but AFAICS it should always be present*)
  (* dir=None represents a directory index of 0, which "is
     understood to be the current directory of the compilation". *)
  (* The DWARF4 spec for the semantics of non-0-index
     include_directories is unhelpfully nondeterministic: "Each path
     entry is either a full path name or is relative to the current
     directory of the compilation.".  In our examples, they appear
     sometimes to be full path names, which have the comp_dir as a
     prefix, and sometimes (for .S files) to be relative to
     comp_dir. *)
  let comp_dir = match comp_diro with Some comp_dir -> comp_dir | None -> "" in

  let (directory_original, directory_replacement) =
    match dir with
    | None -> (
        (* directory index 0, so use the current directory of compilation, replaced if requested *)
        let directory_original = comp_dir in
        match replacement with
        | None -> (directory_original, directory_original)
        | Some comp_dir_replacement -> (directory_original, comp_dir_replacement)
      )
    | Some d -> (
        (* non-zero directory index.  First check if it has comp_dir as a prefix *)
        let has_comp_dir_prefix =
          String.length d >= String.length comp_dir
          && String.sub d 0 (String.length comp_dir) = comp_dir
        in

        let suffix =
          match has_comp_dir_prefix with
          | true ->
              String.sub d (String.length comp_dir) (String.length d - String.length comp_dir)
          | false ->
              warn "not has_comp_dir_prefix for %s %s %s" comp_dir d file;
              d
        in

        (* prefix with either comp_dir or comp_dir_replacement *)
        let directory_original = Filename.concat comp_dir suffix in
        match replacement with
        | None -> (directory_original, directory_original)
        | Some comp_dir_replacement ->
            (directory_original, Filename.concat comp_dir_replacement suffix)
      )
  in
  (directory_original, directory_replacement)

let source_line (comp_diro, dir, file) n1 =
  (* let pp_string_option s = match s with Some s' -> s' | None -> "<none>" in
     Printf.printf "comp_dir=\"%s\"  source_line dir=\"%s\"  file=\"%s\"\n"
       (pp_string_option comp_dir) (pp_string_option dir) file; *)
  let access_lines lines n =
    if n < 0 || n >= Array.length lines then
      Some (sprintf "line out of range: %i vs %i" n (Array.length lines))
    else Some lines.(n)
  in

  let n = n1 - 1 in
  match
    try Some (List.assoc (comp_diro, dir, file) !source_file_cache) with Not_found -> None
  with
  | Some (Some lines) -> access_lines lines n
  | Some None -> None
  | None -> (
      let (_directory_original, directory_replacement) =
        actual_directories !Globals.comp_dir (comp_diro, dir, file)
      in
      let filename = Filename.concat directory_replacement file in
      match read_file_lines filename with
      | Ok lines ->
          source_file_cache := ((comp_diro, dir, file), Some lines) :: !source_file_cache;
          access_lines lines n
      | Error s ->
          (* source_file_cache := (file, None) :: !source_file_cache;
             None *)
          source_file_cache := ((comp_diro, dir, file), None) :: !source_file_cache;
          err "filename %s %s" filename s;
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

let mk_subprogram_name (ds : Dwarf.dwarf_static) elifi : string =
  let lnh = elifi.elifi_entry.elie_lnh in
  let lnr = elifi.elifi_entry.elie_lnr in
  let ((_comp_dir, _dir, _file), subprogram_name) =
    let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
    (ufe, Dwarf.subprogram_at_line ds.ds_subprogram_line_extents ufe lnr.lnr_line)
  in
  subprogram_name

let pp_dwarf_source_file_lines' m (ds : Dwarf.dwarf_static) (pp_actual_line : bool) multiple elifi
    : string =
  let lnh = elifi.elifi_entry.elie_lnh in
  let lnr = elifi.elifi_entry.elie_lnr in
  let ((comp_dir, dir, file), subprogram_name) =
    let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
    (ufe, Dwarf.subprogram_at_line ds.ds_subprogram_line_extents ufe lnr.lnr_line)
  in
  let wrap_link m s =
    match m with
    | Ascii -> s
    | Html ->
        "@<a class=\"link-inst\" href=\"" ^ "" ^ file ^ ".html#"
        ^ Sym.to_string lnr.lnr_line
        ^ "\">" ^ s ^ "</a>@ "
  in
  wrap_link m
    (subprogram_name ^ ":"
    ^ Sym.to_string lnr.lnr_line
    ^ "."
    ^ Sym.to_string lnr.lnr_column
    ^ " (" ^ file ^ ")"
    )
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
    let line = Sym.to_int lnr.lnr_line in
    if line = 0 then "line 0"
    else
      pp_source_line (source_line (comp_dir, dir, file) line) (Sym.to_int lnr.lnr_column)
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
          (Sym.sub a (Sym.of_int 4))
    | _ ->
        List.map
          (fun ((comp_dir, dir, file), n, lnr, subprogram_name) ->
            (subprogram_name, Sym.to_int n))
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
               let ((_comp_dir, _dir, _file), subprogram_name) =
                 let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
                 ( ufe,
                   Dwarf.subprogram_at_line test.dwarf_static.ds_subprogram_line_extents ufe
                     lnr.lnr_line )
               in
               (subprogram_name, Sym.to_int lnr.lnr_line))
         elifis)
  in
  match lines with [_] -> lines | [] -> lines | _ -> []
