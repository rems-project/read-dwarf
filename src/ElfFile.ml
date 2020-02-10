(** This module represent an ELF 64 file we do not deal with 32 bit for now *)

module Sym = ElfSymbol
module SymTbl = Sym.Table

(** The type containing all the information about an ELF file

    The end goal is that this type contains more interpreted information than Analyse.test i.e
    interpreted enough so that they can be directly used into the symbolic execution

    In fine, all the interpretated graphs, C types, comes_from, ... will go into this type
*)
type t = { symbols : SymTbl.t; segments : Segment.t list }

(** Parse an ELF file to create a ElfFile.t. Uses Analyse.parse_file *)
let of_file (file : string) =
  let f = Analyse.parse_file file in
  let segments = List.map Segment.of_linksem f.segments in
  let symbols =
    List.fold_left
      (fun smap lsym ->
        let sym = Sym.of_linksem lsym in
        if ElfSymbol.is_interesting sym.typ then
          let nsym =
            if Sym.linksem_data lsym = None then
              let (addr, size) = (sym.addr, sym.size) in
              match Segment.get_addr_list_opt (BytesSeq.sub_getter size) segments addr with
              | Some data -> Sym.of_linksem_with_data data lsym
              | None -> Warn.fatal2 "Symbol %s at 0x%x could not be loaded" (fst lsym) addr
            else sym
          in
          SymTbl.add smap nsym
        else smap)
      SymTbl.empty f.symbol_map
  in
  { symbols; segments }
