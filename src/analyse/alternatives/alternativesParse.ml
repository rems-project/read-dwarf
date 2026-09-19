(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Parse an alternatives section (normally [.altinstructions]) of an ELF64 file into an
    {!AlternativesType.table}.

    In a relocatable file the two offset words of each entry are zero in the data and are given by
    [R_AARCH64_PREL32] relocations, which linksem presents symbolically as [S + A - P]; we recover
    [S + A] as a section-relative address, or, for an undefined symbol (a callback function
    defined in the kernel proper), as a name. In a linked file the words are literal and we
    compute virtual addresses, mapping them back to the containing section so that they match the
    section-relative addresses that the objdump-derived instructions carry. *)

open Utils
open AlternativesType

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*  little-endian field readers                                              *)
(*****************************************************************************)

let byte bs i = Char.code (Byte_sequence_wrapper.get bs i)
let read_u8 bs i = byte bs i
let read_u16_le bs i = byte bs i lor (byte bs (i + 1) lsl 8)

let read_u32_le bs i =
  byte bs i lor (byte bs (i + 1) lsl 8) lor (byte bs (i + 2) lsl 16) lor (byte bs (i + 3) lsl 24)

let read_s32_le bs i =
  let u = read_u32_le bs i in
  if u land 0x80000000 <> 0 then u - 0x100000000 else u

let read_alt_instr bs o : alt_instr =
  {
    orig_offset = read_s32_le bs o;
    alt_offset = read_s32_le bs (o + 4);
    cpucap = read_u16_le bs (o + 8);
    orig_len = read_u8 bs (o + 10);
    alt_len = read_u8 bs (o + 11);
  }

(*****************************************************************************)
(*  symbolic relocation values (relocatable files)                           *)
(*****************************************************************************)

(* Claude: the resolved value of a relocation of the form S + A - P *)
type prel_target =
  | Target_addr of addr  (** S + A, as section + offset *)
  | Target_undef of string * int  (** undefined symbol name, and the addend *)

exception Unsupported_expression

(* Claude: flatten a sum/difference of Section and Const terms into
   (positive sections, negative sections, constant); anything else is
   unsupported *)
let rec collect sign (e : Elf_symbolic.symbolic_expression) ((pos, neg, c) as acc) =
  match e with
  | Elf_symbolic.Section s -> if sign then (s :: pos, neg, c) else (pos, s :: neg, c)
  | Elf_symbolic.Const x ->
      let x = Z.to_int x in
      (pos, neg, if sign then c + x else c - x)
  | Elf_symbolic.BinOp (a, Elf_symbolic.Add, b) -> collect sign b (collect sign a acc)
  | Elf_symbolic.BinOp (a, Elf_symbolic.Sub, b) -> collect (not sign) b (collect sign a acc)
  | Elf_symbolic.BinOp (_, Elf_symbolic.And, _) | Elf_symbolic.UnOp _ ->
      raise Unsupported_expression

let undef_prefix = "UND."

(* Claude: interpret the value of a PREL32 relocation at offset [p] of section
   [here], expecting exactly one positive section term (the symbol's
   section, or "UND.name" for an undefined symbol) and [here] as the only
   negative one *)
let resolve_prel32 ~here ~p (e : Elf_symbolic.symbolic_expression) : (prel_target, string) result
    =
  let unsupported () =
    Error ("unsupported relocation expression " ^ Elf_symbolic.pp_sym_expr e)
  in
  match collect true e ([], [], 0) with
  | exception Unsupported_expression -> unsupported ()
  | ([s], [h], c) when h = here ->
      let value = c + p in
      if String.starts_with ~prefix:undef_prefix s then
        Ok
          (Target_undef
             ( String.sub s (String.length undef_prefix)
                 (String.length s - String.length undef_prefix),
               value
             )
          )
      else Ok (Target_addr (Sym_ocaml.Num.Offset (s, Z.of_int value)))
  | _ -> unsupported ()

(*****************************************************************************)
(*  section and symbol lookups                                               *)
(*****************************************************************************)

let sections_named (f64 : Elf_file.elf64_file) name =
  List.filter
    (fun (s : Elf_interpreted_section.elf64_interpreted_section) ->
      s.elf64_section_name_as_string = name
    )
    f64.elf64_file_interpreted_sections

let is_alloc (s : Elf_interpreted_section.elf64_interpreted_section) =
  not (Z.equal Z.zero (Z.logand s.elf64_section_flags Elf_section_header_table.shf_alloc))

(* Claude: the allocated section containing virtual address [va], for linked files *)
let section_of_va (f64 : Elf_file.elf64_file) (va : Z.t) : string option =
  List.find_map
    (fun (s : Elf_interpreted_section.elf64_interpreted_section) ->
      if
        is_alloc s && Z.leq s.elf64_section_addr va
        && Z.lt va (Z.add s.elf64_section_addr s.elf64_section_size)
        && not (Z.equal Z.zero s.elf64_section_size)
      then Some s.elf64_section_name_as_string
      else None
    )
    f64.elf64_file_interpreted_sections

(* Claude: names of non-mapping symbols by value, FUNC symbols first, for
   naming callbacks in linked files *)
let symbol_names_by_value (f64 : Elf_file.elf64_file) : (Z.t, string list) Hashtbl.t =
  let tbl = Hashtbl.create 1024 in
  ( match Elf_file.get_elf64_file_symbol_table f64 with
  | Error.Fail s -> warn "alternatives: cannot read symbol table: %s" s
  | Error.Success (symtab, strtab) ->
      let add is_func value name =
        let old = try Hashtbl.find tbl value with Not_found -> [] in
        Hashtbl.replace tbl value (if is_func then name :: old else old @ [name])
      in
      List.iter
        (fun (e : Elf_symbol_table.elf64_symbol_table_entry) ->
          match String_table.get_string_at (Uint32_wrapper.to_bigint e.elf64_st_name) strtab with
          | Error.Fail _ -> ()
          | Error.Success name ->
              if name <> "" && name.[0] <> '$' then
                let is_func =
                  Z.equal
                    (Elf_symbol_table.extract_symbol_type e.elf64_st_info)
                    Elf_symbol_table.stt_func
                in
                add is_func (Uint64_wrapper.to_bigint e.elf64_st_value) name
        )
        symtab
  );
  tbl

(*****************************************************************************)
(*  the parser                                                               *)
(*****************************************************************************)

let cap_of ?cpucaps (raw : alt_instr) : cpucap =
  let n = raw.cpucap land lnot arm64_cb_bit in
  { cap_number = n; cap_name = Option.bind cpucaps (fun t -> Cpucaps.name t n) }

(* Claude: checks corresponding to the BUG_ONs in __apply_alternatives(), plus our own *)
let sanity_problems ?cpucaps (raw : alt_instr) (is_callback : bool) : string list =
  List.filter_map Fun.id
    [
      ( if raw.orig_len mod aarch64_insn_size <> 0 then
          Some (Printf.sprintf "orig_len %d is not a multiple of 4" raw.orig_len)
        else None
      );
      ( if is_callback && raw.alt_len <> 0 then
          Some (Printf.sprintf "callback entry with alt_len %d (kernel expects 0)" raw.alt_len)
        else None
      );
      ( if (not is_callback) && raw.alt_len <> raw.orig_len then
          Some
            (Printf.sprintf "replacement entry with alt_len %d <> orig_len %d" raw.alt_len
               raw.orig_len
            )
        else None
      );
      ( match cpucaps with
      | Some t when raw.cpucap land lnot arm64_cb_bit >= Array.length t ->
          Some
            (Printf.sprintf "cpucap %d is beyond the %d caps in the cpucaps file"
               (raw.cpucap land lnot arm64_cb_bit)
               (Array.length t)
            )
      | _ -> None
      );
    ]

(** [parse ?cpucaps f64 section_name] is [None] iff [f64] has no section of that name. *)
let parse ?cpucaps (f64 : Elf_file.elf64_file) (section_name : string) : table option =
  match sections_named f64 section_name with
  | [] -> None
  | _ :: _ :: _ -> fatal "alternatives: multiple sections named %s" section_name
  | [section] ->
      let bs = section.elf64_section_body in
      let size = Byte_sequence_wrapper.length bs in
      let relocatable = Elf_header.is_elf64_relocatable_file f64.elf64_file_header in
      let table_problems =
        if size mod alt_instr_size <> 0 then
          [
            Printf.sprintf "section size %d is not a multiple of %d; trailing bytes ignored" size
              alt_instr_size;
          ]
        else []
      in
      let n = size / alt_instr_size in
      (* the two ways of resolving the offset fields *)
      let relocs =
        if relocatable then
          match
            Elf_symbolic.extract_elf64_relocations_for_section f64
              Abi_aarch64_symbolic_relocation.aarch64_relocation_interpreter section_name
          with
          | Error.Fail s ->
              fatal "alternatives: cannot read relocations for %s: %s" section_name s
          | Error.Success m -> Some m
        else None
      in
      let symbol_names = if relocatable then Hashtbl.create 1 else symbol_names_by_value f64 in
      (* Claude: resolve the field at section offset [p] to a target, given
         the literal field value [v] and a description for error messages *)
      let resolve_field p v : (prel_target, string) result =
        match relocs with
        | Some m -> (
            match Pmap.lookup (Z.of_int p) m with
            | None -> Error (Printf.sprintf "no relocation at offset 0x%x" p)
            | Some (rel : _ Elf_symbolic.universal_relocation) -> (
                match resolve_prel32 ~here:section_name ~p rel.rel_desc_value with
                | Error e -> Error e
                | Ok t ->
                    if v <> 0 then
                      warn "alternatives: non-zero literal 0x%x under the relocation at %s+0x%x" v
                        section_name p;
                    Ok t
              )
          )
        | None -> (
            let va = Z.add (Z.add section.elf64_section_addr (Z.of_int p)) (Z.of_int v) in
            match section_of_va f64 va with
            | Some s -> Ok (Target_addr (Sym_ocaml.Num.Offset (s, va)))
            | None -> Ok (Target_addr (Sym_ocaml.Num.Absolute va))
          )
      in
      let entry_addr o =
        if relocatable then Sym_ocaml.Num.Offset (section_name, Z.of_int o)
        else Sym_ocaml.Num.Absolute (Z.add section.elf64_section_addr (Z.of_int o))
      in
      let mk_entry index : entry =
        let o = index * alt_instr_size in
        let raw = read_alt_instr bs o in
        let is_callback = raw.cpucap land arm64_cb_bit <> 0 in
        let problems = ref (sanity_problems ?cpucaps raw is_callback) in
        let problem s = problems := !problems @ [s] in
        let orig =
          match resolve_field o raw.orig_offset with
          | Ok (Target_addr a) -> Some a
          | Ok (Target_undef (name, _)) ->
              problem ("original code given by undefined symbol " ^ name);
              None
          | Error e ->
              problem ("original code: " ^ e);
              None
        in
        let action =
          match resolve_field (o + 4) raw.alt_offset with
          | Ok (Target_undef (name, addend)) ->
              if addend <> 0 then
                problem (Printf.sprintf "callback %s with non-zero addend %d" name addend);
              if not is_callback then problem "undefined-symbol target but callback bit not set";
              Callback name
          | Ok (Target_addr a) ->
              if is_callback then
                (* a linked file: the callback is an address; name it if we can *)
                let va =
                  match a with Sym_ocaml.Num.Offset (_, z) | Sym_ocaml.Num.Absolute z -> z
                in
                match Hashtbl.find_opt symbol_names va with
                | Some (name :: _) -> Callback name
                | _ -> Callback (Printf.sprintf "<%s>" (pp_addr a))
              else Replacement a
          | Error e -> Unresolved e
        in
        {
          index;
          entry_addr = entry_addr o;
          raw;
          orig;
          nr_inst = raw.orig_len / aarch64_insn_size;
          cap = cap_of ?cpucaps raw;
          is_callback;
          action;
          problems = !problems;
        }
      in
      let entries = Array.init n mk_entry in
      let by_orig =
        Array.fold_left
          (fun m (e : entry) ->
            match e.orig with
            | None -> m
            | Some a ->
                let old = try SymMap.find a m with Not_found -> [] in
                SymMap.add a (old @ [e]) m
          )
          SymMap.empty entries
      in
      info "alternatives: %d entries in %s" n section_name;
      Some { section_name; entries; by_orig; table_problems }
