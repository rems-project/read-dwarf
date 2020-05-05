(** This module provides an enumeration of architecture for internal identification *)

(** The supported architectures *)
type t = X86 | X86_64 | PPC | PPC64 | ARM | AARCH64

let to_string = function
  | X86 -> "x86"
  | X86_64 -> "x86_64"
  | PPC -> "ppc"
  | PPC64 -> "ppc64"
  | ARM -> "arm"
  | AARCH64 -> "aarch64"

let of_string = function
  | "x86" -> X86
  | "x86_64" -> X86_64
  | "ppc" -> PPC
  | "ppc64" -> PPC64
  | "arm" -> ARM
  | "aarch64" -> AARCH64
  | s -> Raise.inv_arg "Architecture string %s unknown" s

let pp a = a |> to_string |> PP.string

let size : t -> int = function
  | X86 -> 32
  | X86_64 -> 64
  | PPC -> 32
  | PPC64 -> 64
  | ARM -> 32
  | AARCH64 -> 64

let fmt f t = t |> to_string |> Format.pp_print_string f

let conv =
  let docv = "Architecture type (aarch64, arm, x86, x86_64, ppc or ppc64)" in
  let parser a = try Ok (of_string a) with Invalid_argument s -> Error (`Msg s) in
  Cmdliner.Arg.conv ~docv (parser, fmt)
