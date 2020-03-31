(** This module define all architecture-dependent configuration

    It should be used instead of {!Arch} inside the architecture dependent modules.

    Everything inside this module is copied into {!Arch}, so module that can depend on
    {!Arch} may do so.
*)

module Type = struct
  (** The supported architectures *)
  type t = X86 | X86_64 | PPC | PPC64 | ARM | AARCH64

  let to_string = function
    | X86 -> "X86"
    | X86_64 -> "X86_64"
    | PPC -> "PPC"
    | PPC64 -> "PPC64"
    | ARM -> "ARM"
    | AARCH64 -> "AARCH64"

  let pp a = a |> to_string |> PP.string

  let size : t -> int = function
    | X86 -> 32
    | X86_64 -> 64
    | PPC -> 32
    | PPC64 -> 64
    | ARM -> 32
    | AARCH64 -> 64
end

type dwarf_reg_map = Reg.path array

module type S = sig
  (** Tells if this Arch module supports this architecture *)
  val supports : Type.t -> bool

  (** If this arch module {!supports} the architecture, then initialize read-dwarf
      state using this architecture *)
  val init : Type.t -> unit

  (** Return [Some(arch)] is the loaded arch is [arch] and [None] if nothing is loaded yet. *)
  val initialized : unit -> Type.t option

  (** The name of the arch module. Must be the name of the module i.e. [Config.arch_module] *)
  val module_name : string

  (** For dynamic arch module, the name of the dynamically loaded module.
      Otherwise {!module_name} *)
  val loaded_name : string

  val dwarf_reg_map : unit -> dwarf_reg_map
end
