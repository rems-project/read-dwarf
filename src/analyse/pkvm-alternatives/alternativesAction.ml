(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** From an alternatives entry to the instructions involved. This is parameterised by a fetch
    function so that it does not depend on the analyse [instruction] type: [fetch addr] is the
    instruction at [addr], if the objdump has one. *)

open Utils
open AlternativesType

(* Claude: instructions are paired with their addresses so that a missing one
   (not in the objdump) can still be reported *)
type 'insn action =
  | Act_callback of {
      orig : (addr * 'insn option) list;
      callback : string;
      kind : Pkvm_alternatives.alt_callback;
    }
  | Act_replace of {
      orig : (addr * 'insn option) list;
      replacement_addr : addr;
      replacement : (addr * 'insn option) list;
    }
  | Act_unresolved of { orig : (addr * 'insn option) list; why : string }

let fetch_range ~(fetch : addr -> 'insn option) (base : addr) (nr_inst : int) :
    (addr * 'insn option) list =
  List.init nr_inst (fun i ->
      let a = Sym.add base (Sym.of_int (i * aarch64_insn_size)) in
      (a, fetch a)
  )

(** The original instructions of an entry and, according to its kind, the callback or the
    replacement instructions as assembled at their own address. *)
let action_of_entry ~(fetch : addr -> 'insn option) (e : entry) : 'insn action =
  let orig = fetch_range ~fetch (orig_addr e) (nr_inst e) in
  match action e with
  | Callback (name, kind) -> Act_callback { orig; callback = name; kind }
  | Replacement a ->
      Act_replace { orig; replacement_addr = a; replacement = fetch_range ~fetch a (nr_inst e) }
  | Unresolved why -> Act_unresolved { orig; why }
