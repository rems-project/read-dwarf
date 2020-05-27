(** This module define all architecture-dependent configuration

    It should be used instead of {!Arch} inside the architecture dependent modules.

    Everything inside this module is copied into {!Arch}, so module that can depend on
    {!Arch} may do so.
*)

module Type = ArchType

(** Describe the C API of a function *)
type func_api = { args : Ctype.t list; ret : Ctype.t option }

(** Describe the ABI of a function

    This is a record because I expect to add many other fields later.
*)
type func_abi = {
  init : State.t -> State.t;
      (** Gives the initial state for verifying the function, from a given global
          register state. Only global registers are kept. *)
}

(** The map of dwarf register: Which register number map to which ISA register *)
type dwarf_reg_map = Reg.t array

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

  (** The true size of addresses for memory operation *)
  val address_size : int

  (** Get the register map of the architecture *)
  val dwarf_reg_map : unit -> dwarf_reg_map

  (** Tell if a register is local for the ABI *)
  val is_local : Reg.t -> bool

  (** Give the opcode of the nop instruction (For Sail/Isla initialisation *)
  val nop : unit -> BytesSeq.t

  (** Give the ABI of a function from it's C API *)
  val get_abi : func_api -> func_abi

  (** Give the register index for the Program counter *)
  val pc : unit -> Reg.t

  (** Take an instruction string and give the name of an temporary ELF file created
      that contains the instruction at symbol instr.*)
  val assemble_to_elf : string -> string
end
