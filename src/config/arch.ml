(** This module provides an enumeration of architecture for internal identification *)

(** The supported architectures *)
type t = X86 | X86_64 | PpC | PpC64 | ARM | AARCH64

let to_string = function
  | X86 -> "x86"
  | X86_64 -> "x86_64"
  | PpC -> "ppc"
  | PpC64 -> "ppc64"
  | ARM -> "arm"
  | AARCH64 -> "aarch64"

let of_string = function
  | "x86" -> X86
  | "x86_64" -> X86_64
  | "ppc" -> PpC
  | "ppc64" -> PpC64
  | "arm" -> ARM
  | "aarch64" -> AARCH64
  | s -> Raise.inv_arg "Architecture string %s unknown" s

let pp a = a |> to_string |> Pp.string

let size : t -> int = function
  | X86 -> 32
  | X86_64 -> 64
  | PpC -> 32
  | PpC64 -> 64
  | ARM -> 32
  | AARCH64 -> 64

let fmt f t = t |> to_string |> Format.pp_print_string f

let conv =
  let docv = "Architecture type (aarch64, arm, x86, x86_64, ppc or ppc64)" in
  let parser a = try Ok (of_string a) with Invalid_argument s -> Error (`Msg s) in
  Cmdliner.Arg.conv ~docv (parser, fmt)
