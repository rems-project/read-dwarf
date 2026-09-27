(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** The object's sections link-time relocated by linksem, one at a time and cached, for the
    renderings that show what linksem's models say a word is (alternatives, jump labels). Each
    section is relocated as [Abi_aarch64_instruction_fields.resolve_aarch64_object] does for the
    whole object, with the linker's possible ADRP+ADD relaxation, but only when first asked for,
    so a run that renders none of them pays nothing. *)


let cache : (string, (Symbolic_resolution.sym_section, string) result) Hashtbl.t = Hashtbl.create 8

let relocated (f64 : Elf_file.elf64_file) (name : string) : (Symbolic_resolution.sym_section, string) result =
  match Hashtbl.find_opt cache name with
  | Some r -> r
  | None ->
      let r =
        match
          Symbolic_resolution.relocate_section f64
            Abi_aarch64_symbolic_relocation.aarch64_relocation_interpreter
            Abi_aarch64_instruction_fields.aarch64_field_spec name
        with
        | Error.Fail m -> Error (name ^ ": " ^ m)
        | Error.Success ss -> (
            if ss.Symbolic_resolution.sec_words = [] then Ok ss
            else
              match Abi_aarch64_instruction_fields.relax_adrp_add f64 name ss with
              | Error.Fail m -> Error (name ^ ": " ^ m)
              | Error.Success ss -> Ok ss )
      in
      Hashtbl.replace cache name r;
      r

(** The named sections, as an association list, or the first failure *)
let relocated_list (f64 : Elf_file.elf64_file) (names : string list) :
    ((string * Symbolic_resolution.sym_section) list, string) result =
  List.fold_left
    (fun acc name ->
      match acc with
      | Error m -> Error m
      | Ok l -> ( match relocated f64 name with Error m -> Error m | Ok ss -> Ok ((name, ss) :: l) ))
    (Ok []) names
