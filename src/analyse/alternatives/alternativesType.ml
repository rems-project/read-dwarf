(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Types for Linux arm64 "alternatives": the runtime instruction-patching
    entries found in an ELF section (normally [.altinstructions]).  The raw
    record mirrors [struct alt_instr] in the kernel's
    [arch/arm64/include/asm/alternative.h]; the rest is the resolved form. *)

open Utils

(* Claude: a map keyed by (possibly section-relative) addresses; Sym.compare
   is partial, Sym.Ordered.compare is total *)
module SymMap = Map.Make (struct
  type t = Sym.t

  let compare = Sym.Ordered.compare
end)

(* Claude: mirrors struct alt_instr; both offsets are relative to the address
   of the field that holds them, not to the start of the entry *)
type alt_instr = {
  orig_offset : int;  (** s32: offset to the original instruction(s) *)
  alt_offset : int;  (** s32: offset to the replacement code, or to the callback function *)
  cpucap : int;  (** u16: cpucap number; bit 15 set means "callback" *)
  orig_len : int;  (** u8: bytes of original code *)
  alt_len : int;  (** u8: bytes of replacement code; 0 for a callback *)
}

let arm64_cb_bit = 0x8000

let alt_instr_size = 12

let aarch64_insn_size = 4

type cpucap = {
  cap_number : int;  (** [cpucap land 0x7fff] *)
  cap_name : string option;  (** from a cpucaps file, if one was given *)
}

(* Claude: what alt_offset denotes once relocations are resolved *)
type action_ref =
  | Replacement of addr  (** address of [alt_len] bytes of replacement code *)
  | Callback of string  (** name of the patching callback function *)
  | Unresolved of string  (** could not be resolved; explanation *)

type entry = {
  index : int;  (** position in the section *)
  entry_addr : addr;  (** address of the entry itself *)
  raw : alt_instr;
  orig : addr option;  (** base address of the original code, if resolved *)
  nr_inst : int;  (** [orig_len / 4] *)
  cap : cpucap;
  is_callback : bool;  (** [cpucap land arm64_cb_bit <> 0] *)
  action : action_ref;
  problems : string list;  (** anything that the kernel would BUG_ON, or that we could not parse *)
}

type table = {
  section_name : string;
  entries : entry array;
  by_orig : entry list SymMap.t;  (** entries indexed by the base address of their original code *)
  table_problems : string list;  (** problems with the section as a whole *)
}
