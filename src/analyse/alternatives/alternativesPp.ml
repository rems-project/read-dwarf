(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Pretty-printing of alternatives entries: the condition under which an entry is applied, and a
    one-line-per-entry dump of a whole table for cross-checking against readelf. *)

open Utils
open AlternativesType

(* Claude: Utils.pp_addr goes through int64 and fails on kernel virtual
   addresses (>= 2^63), so print addresses via Z here *)
let pp_addr (a : addr) : string = Sym_ocaml.Num.ppf (fun z -> Z.format "%08x" z) a

let pp_cpucap (e : entry) : string =
  match cap e with
  | Some c -> Printf.sprintf "%s (%d)" (Generated_arm64_cpucaps.string_of_cpucap c) (cap_number e)
  | None -> Printf.sprintf "cpucap %d" (cap_number e)

(* Claude: ARM64_ALWAYS_BOOT and ARM64_ALWAYS_SYSTEM are set on every system;
   an entry conditioned on them is applied unconditionally, at the boot-CPU
   pass or the system-wide pass respectively *)
let pp_condition_clause (e : entry) : string =
  match cap e with
  | Some Generated_arm64_cpucaps.ARM64_ALWAYS_BOOT -> Printf.sprintf "always (boot pass; %s)" (pp_cpucap e)
  | Some Generated_arm64_cpucaps.ARM64_ALWAYS_SYSTEM ->
      Printf.sprintf "always (system pass; %s)" (pp_cpucap e)
  | _ -> Printf.sprintf "if %s is present" (pp_cpucap e)

let pp_action_ref (a : action_ref) : string =
  match a with
  | Replacement addr -> "replace with the code at " ^ pp_addr addr
  | Callback (name, _) -> "callback " ^ name
  | Unresolved why -> "unresolved: " ^ why

let pp_orig (e : entry) : string = pp_addr (orig_addr e)

let pp_len (e : entry) : string =
  Printf.sprintf "%d bytes (%d instruction%s)" (orig_len e) (nr_inst e)
    (if nr_inst e = 1 then "" else "s")

(* Claude: the condition and action of an entry in words, optionally with
   the addresses of the original and replacement code; the same text serves
   the per-instruction blocks (with addresses) and the "alternative kinds"
   summary (without) *)
let pp_condition_gen ~(with_addresses : bool) (e : entry) : string =
  let at = if with_addresses then " at " ^ pp_orig e else "" in
  match action e with
  | Callback (name, _) ->
      Printf.sprintf "%s: callback %s rewrites %s%s" (pp_condition_clause e) name (pp_len e) at
  | Replacement a ->
      Printf.sprintf "%s: replace %s%s with %s" (pp_condition_clause e) (pp_len e) at
        (if with_addresses then "the code at " ^ pp_addr a else "replacement code")
  | Unresolved why ->
      Printf.sprintf "%s: %s%s; action unresolved: %s" (pp_condition_clause e)
        (if is_callback e then "callback" else "replacement")
        at why

(** The condition and action of an entry, in words, one clause per line, with any problems as [!!]
    lines. *)
let pp_condition (e : entry) : string =
  String.concat "\n" (pp_condition_gen ~with_addresses:true e :: List.map (fun p -> "!! " ^ p) (problems e))

(* Claude: what the callbacks linksem models write, in words, from
   arch/arm64/kvm/va_layout.c, arch/arm64/kernel/alternative.c and
   arch/arm64/kernel/proton-pack.c; the constants are boot-time values.
   The set of callbacks is linksem's (Pkvm_alternatives.alt_callback) *)
let describe_callback (cb : Pkvm_alternatives.alt_callback) : string option =
  match cb with
  | Cb_patch_nops -> Some "every instruction becomes nop (so a branch guarding a cpus_have_final_cap()-style test falls through when the cap is present)"
  | Cb_update_va_mask ->
      Some
        "and/ror/add/add/ror immediates set to va_mask, tag_lsb and tag_val, computing kern_hyp_va(v) = (v & va_mask) | (tag_val << tag_lsb); all nop on VHE; instructions 2-5 nop if tag_val = 0"
  | Cb_patch_physvirt_offset -> Some "the first four become movz/movk/movk/movk t, hyp_physvirt_offset; the final add/sub is unchanged"
  | Cb_get_kimage_voffset -> Some "movz/movk/movk/movk t, kimage_voffset"
  | Cb_compute_final_ctr_el0 -> Some "movz/movk/movk/movk r, the sanitised system-wide CTR_EL0"
  | Cb_patch_vector_branch ->
      Some
        "only with ARM64_SPECTRE_V3A and not VHE: movz/movk/movk x0, kern_hyp_va(__kvm_hyp_vector) + (PC & 0x780) + KVM_VECTOR_PREAMBLE; br x0 (an indirect jump into the real vector table); otherwise left as nops"
  | Cb_spectre_bhb_patch_wa3 -> Some "mov w0, #ARM_SMCCC_ARCH_WORKAROUND_3 (as orr) if the firmware Spectre-BHB mitigation is in use; otherwise unchanged"
  | Cb_other _ -> None

(*****************************************************************************)
(*  the words an entry writes                                                *)
(*****************************************************************************)

(* Claude: the leaves of a symbolic word, printed as linksem prints them, for
   a summary of what the word depends on *)
let rec expr_leaves (e : Symbolic_resolution.sym_expr) : string list =
  match e with
  | SConst _ -> []
  | SSection s -> [s]
  | SSymbol s -> ["symbol(" ^ s ^ ")"]
  | SGotSlot s -> ["got(" ^ s ^ ")"]
  | SVar v -> ["var(" ^ v ^ ")"]
  | SBin (_, a, b) -> expr_leaves a @ expr_leaves b
  | SNot a -> expr_leaves a
  | SIte (f, a, b) -> flag_leaves f @ expr_leaves a @ expr_leaves b

and flag_leaves (f : Symbolic_resolution.sym_flag) : string list =
  match f with
  | SFlag s -> ["flag(" ^ s ^ ")"]
  | SIsZero e -> expr_leaves e
  | SInRange (e, _, _) -> expr_leaves e
  | SBoth (f1, f2) -> flag_leaves f1 @ flag_leaves f2

let rec has_conditional (e : Symbolic_resolution.sym_expr) : bool =
  match e with
  | SIte _ -> true
  | SBin (_, a, b) -> has_conditional a || has_conditional b
  | SNot a -> has_conditional a
  | SConst _ | SSection _ | SSymbol _ | SGotSlot _ | SVar _ -> false

let word_constant (e : Symbolic_resolution.sym_expr) : Z.t option =
  match e with SConst x -> Some x | _ -> None

(** A word as an instruction word in hex if it is constant; otherwise what it depends on. *)
let pp_word_summary (e : Symbolic_resolution.sym_expr) : string =
  match e with
  | SConst x -> "0x" ^ Z.format "%08x" x
  | _ ->
      (if has_conditional e then "conditional; " else "")
      ^ "depends on "
      ^ String.concat ", " (List.sort_uniq compare (expr_leaves e))

(** The kind of an alternative entry: its condition and action, including which callback and
    what it writes, but no addresses and no instructions. *)
let pp_alternative_kind (e : entry) : string =
  pp_condition_gen ~with_addresses:false e
  ^
  match action e with
  | Callback (_, kind) -> ( match describe_callback kind with Some d -> "\n        " ^ d | None -> "" )
  | Replacement _ | Unresolved _ -> ""

(** The kinds of alternative occurring among [entries], each with its number of occurrences, most
    frequent first. *)
let pp_alternative_kinds (entries : entry list) : string =
  let counts = Hashtbl.create 64 in
  List.iter
    (fun e ->
      let k = pp_alternative_kind e in
      Hashtbl.replace counts k (1 + try Hashtbl.find counts k with Not_found -> 0))
    entries;
  let kinds = Hashtbl.fold (fun k n acc -> (n, k) :: acc) counts [] in
  let kinds =
    List.sort (fun (n1, k1) (n2, k2) -> if n1 <> n2 then compare n2 n1 else compare k1 k2) kinds
  in
  Printf.sprintf "%d alternatives entries, %d kinds\n\n" (List.length entries) (List.length kinds)
  ^ String.concat "" (List.map (fun (n, k) -> Printf.sprintf "%6d  %s\n" n k) kinds)

(** One line per entry, readelf-like: index, entry address, the cpucap field as in the file (bit
    15 is the callback bit), lengths, resolved original and action, and for a callback linksem's
    kind for it. *)
let pp_entry (t : table) (index : int) (e : entry) : string =
  Printf.sprintf "%5d %s  cpucap=0x%04x orig_len=%d alt_len=%d  orig=%s  %s%s%s%s" index
    (pp_addr (entry_addr t index))
    (cap_number e lor if is_callback e then Pkvm_alternatives.alt_cb_bit |> Z.to_int else 0)
    (orig_len e) (alt_len e) (pp_orig e)
    (if is_callback e then "CB " else "")
    (pp_action_ref (action e))
    (match action e with
    | Callback (_, kind) -> " kind=" ^ Pkvm_alternatives.string_of_alt_callback kind
    | _ -> "")
    (match problems e with [] -> "" | ps -> "  !! " ^ String.concat "; " ps)

let pp_table (t : table) : string =
  Printf.sprintf "alternatives section %s: %d entries\n" t.section_name (Array.length t.entries)
  ^ String.concat "" (List.map (fun p -> "!! " ^ p ^ "\n") t.table_problems)
  ^ String.concat "" (Array.to_list (Array.mapi (fun i e -> pp_entry t i e ^ "\n") t.entries))
