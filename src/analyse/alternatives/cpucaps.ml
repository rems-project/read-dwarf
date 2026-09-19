(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** Names for arm64 cpucap numbers, from a file taken from the kernel build
    that produced the object being analysed.  Two formats are accepted:

    - the generated header [arch/arm64/include/generated/asm/cpucap-defs.h],
      with lines [#define ARM64_<NAME> <n>] (the [ARM64_NCAPS] line is skipped);
    - the source list [arch/arm64/tools/cpucaps], where the n-th non-blank,
      non-comment line names cap n (this is what gen-cpucaps.awk does).

    The numbering is per kernel tree and configuration, so the file must come
    from the same build as the object. *)

open Utils

open Logs.Logger (struct
  let str = __MODULE__
end)

type t = string array

let name (t : t) (n : int) : string option =
  if n >= 0 && n < Array.length t then Some t.(n) else None

let is_blank_or_comment (s : string) =
  let s = String.trim s in
  s = "" || s.[0] = '#'

(* Claude: "#define ARM64_FOO<whitespace>17" -> Some ("ARM64_FOO", 17) *)
let parse_define_line (s : string) : (string * int) option =
  match List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) s)) with
  | ["#define"; name; num] -> (
      match int_of_string_opt num with Some n -> Some (name, n) | None -> None
    )
  | _ -> None

let load (filename : string) : t =
  match read_file_lines filename with
  | Error s -> fatal "%s\ncouldn't read cpucaps file: \"%s\"\n" s filename
  | Ok lines ->
      let lines = Array.to_list lines in
      let defines = List.filter_map parse_define_line lines in
      if defines <> [] then begin
        let defines = List.filter (fun (name, _) -> name <> "ARM64_NCAPS") defines in
        let max_n = List.fold_left (fun m (_, n) -> max m n) (-1) defines in
        let t = Array.make (max_n + 1) "" in
        List.iter (fun (name, n) -> if n >= 0 then t.(n) <- name) defines;
        Array.iteri (fun n name -> if name = "" then warn "cpucaps file %s: no name for cpucap %d" filename n) t;
        t
      end
      else
        (* Claude: the raw list; names in the header carry an ARM64_ prefix, so add it here too *)
        Array.of_list
          (List.map (fun s -> "ARM64_" ^ String.trim s) (List.filter (fun s -> not (is_blank_or_comment s)) lines))
