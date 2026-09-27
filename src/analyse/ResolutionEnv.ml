(*==================================================================================*)
(*  Claude: this file is largely written by Claude (September 2026).                *)
(*  BSD 2-Clause License, as for the rest of read-dwarf.                            *)
(*==================================================================================*)

(** An optional recorded resolution environment (linksem's [Symbolic_resolution.resolution_env]
    in the report format, as [objcheck env-from-exe] or the pKVM runtime dump write it), named by
    [--resolution-env]. When present, the symbolic values read-dwarf shows (relocation values,
    the words linksem's models say an alternative or a jump-label site holds) are also evaluated
    under it and shown as "resolved to: ...". *)

open Utils

open Logs.Logger (struct
  let str = __MODULE__
end)

let env : Symbolic_resolution.resolution_env option Lazy.t =
  lazy
    ( match !Globals.resolution_env_file with
    | None -> None
    | Some file -> (
        match read_file_lines file with
        | Error s -> fatal "%s\ncouldn't read resolution environment file: \"%s\"\n" s file
        | Ok lines -> (
            let text = String.concat "\n" (Array.to_list lines) ^ "\n" in
            match Report_format.parse_report text with
            | Error.Fail m -> fatal "resolution environment file %s: %s" file m
            | Error.Success report -> (
                match Symbolic_resolution.parse_resolution_env report with
                | Error.Fail m -> fatal "resolution environment file %s: %s" file m
                | Error.Success env ->
                    info "resolution environment: %d sections, %d symbols, %d params, %d flags"
                      (List.length env.env_sections) (List.length env.env_symbols)
                      (List.length env.env_params) (List.length env.env_flags);
                    Some env ) ) ) )

let present () : bool = Lazy.force env <> None

(** ["  resolved to: 0x..."] for a word of [width] bytes under the environment, [""] if there is
    none; an evaluation failure (a flag or parameter the file lacks) is shown as such *)
let pp_resolved ?(width = 4) (e : Symbolic_resolution.sym_expr) : string =
  match Lazy.force env with
  | None -> ""
  | Some env -> (
      match Symbolic_resolution.eval_sym_expr env e with
      | Error.Success v ->
          (* a negative value (a PC-relative displacement) is shown signed; a word as its bytes *)
          if Z.sign v < 0 && width > 4 then "  resolved to: -0x" ^ Z.format "%x" (Z.neg v)
          else
            let v = Z.logand v (Z.sub (Z.shift_left Z.one (8 * width)) Z.one) in
            "  resolved to: 0x" ^ Z.format ("%0" ^ string_of_int (2 * width) ^ "x") v
      | Error.Fail m -> "  resolved to: ? (" ^ m ^ ")" )

(** A flag's value under the environment, if known *)
let flag (name : string) : bool option =
  match Lazy.force env with
  | None -> None
  | Some env -> List.assoc_opt name env.env_flags
