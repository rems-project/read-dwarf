(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Pretty-printing of jump-label sites: the decoration of a site and of a target instruction,
    the "static keys" summary, and the dump of the whole table (linksem's). *)

open Utils
open JumpTable

let pp_addr (a : addr) : string = Sym_ocaml.Num.ppf (fun z -> Z.format "%08x" z) a

(* Claude: the site's two words in words: the kernel keeps it at "b target" iff
   (key enabled) xor branch, else nop *)
let pp_site (e : entry) : string =
  let b = "b " ^ pp_addr (target_addr e) in
  Printf.sprintf "jump-label: static key %s (branch=%d): %s if disabled, %s if enabled" (key_name e)
    (if e.je_branch then 1 else 0)
    (if e.je_branch then b else "nop")
    (if e.je_branch then "nop" else b)

(** The decoration of a site instruction: the site in words, then, if linksem gives it, the word
    as a symbolic expression and its value under a recorded environment *)
let pp_site_with_word (t : table) (e : entry) : string =
  pp_site e
  ^
  match word t e with
  | Ok w ->
      "  = " ^ Symbolic_resolution.string_of_sym_expr (Symbolic_resolution.simplify_sym_expr w)
      ^ ResolutionEnv.pp_resolved w
  | Error m -> "  (linksem cannot give the word: " ^ m ^ ")"

(** The decoration of a branch target *)
let pp_target (e : entry) : string =
  Printf.sprintf "<- jump-label from %s (static key %s)" (pp_addr (code_addr e)) (key_name e)

(** The keys occurring among [entries], with their site counts, most frequent first; with a
    recorded environment, each key's state *)
let pp_static_keys (entries : entry list) : string =
  let counts = key_counts entries in
  Printf.sprintf "%d jump-label sites, %d static keys\n\n" (List.length entries) (List.length counts)
  ^ String.concat ""
      (List.map
         (fun (k, n) ->
           Printf.sprintf "%6d  %s%s\n" n k
             (match ResolutionEnv.flag ("static_key:" ^ k) with
             | Some b -> "  (" ^ (if b then "enabled" else "disabled") ^ " in the recorded environment)"
             | None -> ""))
         counts)

(** The whole table, as linksem prints it, with any table-level problems *)
let pp_table (t : table) : string =
  Printf.sprintf "jump-label section %s: " t.section_name
  ^ Pkvm_jump_table.string_of_jump_table (Array.to_list t.entries)
  ^ String.concat "" (List.map (fun p -> "!! " ^ p ^ "\n") t.table_problems)
