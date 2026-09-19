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

let pp_cpucap (c : cpucap) : string =
  match c.cap_name with
  | Some n -> Printf.sprintf "%s (%d)" n c.cap_number
  | None -> Printf.sprintf "cpucap %d" c.cap_number

(* Claude: ARM64_ALWAYS_BOOT and ARM64_ALWAYS_SYSTEM are set on every system;
   an entry conditioned on them is applied unconditionally, at the boot-CPU
   pass or the system-wide pass respectively *)
let pp_condition_clause (c : cpucap) : string =
  match c.cap_name with
  | Some "ARM64_ALWAYS_BOOT" -> Printf.sprintf "always (boot pass; %s)" (pp_cpucap c)
  | Some "ARM64_ALWAYS_SYSTEM" -> Printf.sprintf "always (system pass; %s)" (pp_cpucap c)
  | _ -> Printf.sprintf "if %s is present" (pp_cpucap c)

let pp_action_ref (a : action_ref) : string =
  match a with
  | Replacement addr -> "replace with the code at " ^ pp_addr addr
  | Callback name -> "callback " ^ name
  | Unresolved why -> "unresolved: " ^ why

let pp_orig (e : entry) : string =
  match e.orig with Some a -> pp_addr a | None -> "<unresolved>"

let pp_len (e : entry) : string =
  Printf.sprintf "%d bytes (%d instruction%s)" e.raw.orig_len e.nr_inst
    (if e.nr_inst = 1 then "" else "s")

(** The condition and action of an entry, in words, one clause per line, with any problems as [!!]
    lines. *)
let pp_condition (e : entry) : string =
  let main =
    match e.action with
    | Callback name ->
        Printf.sprintf "%s: callback %s rewrites %s at %s" (pp_condition_clause e.cap) name
          (pp_len e) (pp_orig e)
    | Replacement a ->
        Printf.sprintf "%s: replace %s at %s with the code at %s" (pp_condition_clause e.cap)
          (pp_len e) (pp_orig e) (pp_addr a)
    | Unresolved why ->
        Printf.sprintf "%s: %s at %s; action unresolved: %s" (pp_condition_clause e.cap)
          (if e.is_callback then "callback" else "replacement")
          (pp_orig e) why
  in
  String.concat "\n" (main :: List.map (fun p -> "!! " ^ p) e.problems)

(* Claude: what the callbacks used by the arm64 hyp code write, from
   arch/arm64/kvm/va_layout.c, arch/arm64/kernel/alternative.c and
   arch/arm64/kernel/proton-pack.c; the constants are boot-time values *)
let describe_callback (name : string) : string option =
  let prefix = "__kvm_nvhe_" in
  let name =
    if String.starts_with ~prefix name then String.sub name (String.length prefix) (String.length name - String.length prefix)
    else name
  in
  match name with
  | "alt_cb_patch_nops" -> Some "every instruction becomes nop (so a branch guarding a cpus_have_final_cap()-style test falls through when the cap is present)"
  | "kvm_update_va_mask" ->
      Some
        "and/ror/add/add/ror immediates set to va_mask, tag_lsb and tag_val, computing kern_hyp_va(v) = (v & va_mask) | (tag_val << tag_lsb); all nop on VHE; instructions 2-5 nop if tag_val = 0"
  | "kvm_patch_physvirt_offset" -> Some "the first four become movz/movk/movk/movk t, hyp_physvirt_offset; the final add/sub is unchanged"
  | "kvm_get_kimage_voffset" -> Some "movz/movk/movk/movk t, kimage_voffset"
  | "kvm_compute_final_ctr_el0" -> Some "movz/movk/movk/movk r, the sanitised system-wide CTR_EL0"
  | "kvm_patch_vector_branch" ->
      Some
        "only with ARM64_SPECTRE_V3A and not VHE: movz/movk/movk x0, kern_hyp_va(__kvm_hyp_vector) + (PC & 0x780) + KVM_VECTOR_PREAMBLE; br x0 (an indirect jump into the real vector table); otherwise left as nops"
  | "spectre_bhb_patch_wa3" -> Some "mov w0, #ARM_SMCCC_ARCH_WORKAROUND_3 (as orr) if the firmware Spectre-BHB mitigation is in use; otherwise unchanged"
  | "spectre_bhb_patch_loop_iter" -> Some "mov t, #k with the loop count for this CPU"
  | "spectre_bhb_patch_loop_mitigation_enable" -> Some "the branch becomes nop if the Spectre-BHB loop mitigation is enabled"
  | "spectre_bhb_patch_fw_mitigation_enabled" -> Some "the branch becomes nop if the firmware Spectre-BHB mitigation is enabled"
  | "spectre_bhb_patch_clearbhb" -> Some "clearbhb; isb become two nops if the CLRBHB instruction is not supported"
  | "smccc_patch_fw_mitigation_conduit" -> Some "nop becomes hvc #0 or smc #0 according to the SMCCC conduit"
  | "spectre_v4_patch_fw_mitigation_enable" -> Some "the branch becomes nop if the Spectre-v4 firmware mitigation is dynamic and SSBS is absent"
  | _ -> None

(** One line per entry, readelf-like: index, entry address, raw fields, resolved original and
    action. *)
let pp_entry (e : entry) : string =
  Printf.sprintf
    "%5d %s  orig_offset=%+d alt_offset=%+d cpucap=0x%04x orig_len=%d alt_len=%d  orig=%s  %s%s%s"
    e.index (pp_addr e.entry_addr) e.raw.orig_offset e.raw.alt_offset e.raw.cpucap e.raw.orig_len
    e.raw.alt_len (pp_orig e)
    (if e.is_callback then "CB " else "")
    (pp_action_ref e.action)
    (match e.problems with [] -> "" | ps -> "  !! " ^ String.concat "; " ps)

let pp_table (t : table) : string =
  Printf.sprintf "alternatives section %s: %d entries\n" t.section_name (Array.length t.entries)
  ^ String.concat "" (List.map (fun p -> "!! " ^ p ^ "\n") t.table_problems)
  ^ String.concat "" (Array.to_list (Array.map (fun e -> pp_entry e ^ "\n") t.entries))
