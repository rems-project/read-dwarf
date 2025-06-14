(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module introduce a type to represent the state of the machine.

    The symbolic state in this module do not mathematically represent a single
    state but a set of concrete state with all the symbolic variable over the
    whole range of their types. State also contain assertions, and so only
    represent the subset of concrete states, that satisfy all assertions.

    Currently the state type only represent the register (including system
    register) and sequential memory part of an actual machine state. Any other
    architectural state is not represented.

    Additionally, state contain C type information for the {!Ctype} inference
    system. Those fields are not semantically part of the state and do not
    influence in any way which concrete states are represented by the symbolic
    state. Expect for provenance information: Pointers can be tagged with
    provenance information which mean that they are part of a specific restricted
    block of memory or the main block. See {!Mem} for more information. Thus all
    the implicit non-aliasing assertion implied by those provenance field are to
    be considered as part of the group of assertion restricting the set of
    concrete state represented by a symbolic state.

    The presence of C types is optional, which means that this state type can be
    used in a completely untyped context.

    Concrete detail about how symbolic state are represented in Ocaml is in the
    documentation of {!t}.*)

(** {1 State id } *)

(** The type of a state ID. for now it's an integer, but it may change later
    In particular whether a state belong to O0 or O2 may be part of the [id]
    at some point.*)
module Id : sig
  type t

  val to_string : t -> string

  val of_string : string -> t

  val equal : t -> t -> bool

  val pp : t -> Pp.document
end

type id = Id.t

(** {1 State variable management } *)

(** This module provide state variables. Those are all symbolic variables that
    may appear in a state. If the same (in the {!Var.equal} sense) variable
    appear in two state, that mean that when considered together, there is an
    implicit relation between them.

    This means that the set of pair of concrete states represented by a pair of
    symbolic state could be a strict subset of the Cartesian product of the sets
    of concrete state represented by each symbolic state individually. *)
module Var : sig
  (** The type of a variable in the state *)
  type t =
    | Register of id * Reg.t  (** The value of this register in this state *)
    | ReadVar of id * int * Ast.Size.t
        (** The result of a certain read in a certain state.
            The size part is not semantically important:
            Two [ReadVar] with same [id] and same number may no have different sizes *)
    | Arg of int  (** A function argument *)
    | RetArg
        (** The address to which the return value should be written.
            This is used only in certain calling conventions *)
    | RetAddr  (** The return address: The address to which a "return" instruction would jump. *)
    | NonDet of int * Ast.Size.t
        (** Variable representing non-determinism in the spec. Can only be a bit-vector for now. *)
    | Section of string
    (** Symbolic base address of ELF section. Assume 64bit for now. *)

  (** Convert the variable to the string encoding. For parsing infrastructure reason,
      the encoding must always contain at least one [:]. *)
  val to_string : t -> string

  (** Expect a register variable and return the corresponding register. Throw otherwise. *)
  val expect_register : t -> Reg.t

  (** Expect a read variable and return the corresponding index. Throw otherwise. *)
  val expect_readvar : t -> int

  (** The opposite of {!to_string}. Will raise [Invalid_argument] when the string don't match *)
  val of_string : string -> t

  (** Create a register variable bound to the provided state id and register *)
  val of_reg : id -> Reg.t -> t

  val equal : t -> t -> bool

  (** Hashing function for variable. For now it's polymorphic but this may stop at any time *)
  val hash : t -> int

  (** Basically [to_string] in pp mode *)
  val pp : t -> Pp.document

  (** Pretty prints but with bars around *)
  val pp_bar : t -> Pp.document

  (** Get the type of a variable *)
  val ty : t -> Reg.ty

  (** Get a fresh NonDet variable *)
  val new_nondet : Ast.Size.t -> t
end

(** The type of variables *)
type var = Var.t

(** {1 State expression and typed-value management } *)

(** Module for state expressions *)
module Exp : sig
  include Exp.S with type var = var

  (** Create an expression from an register and a state id *)
  val of_reg : id -> Reg.t -> t

  val expect_sym_address : t -> Elf.Address.t

  val of_section : size:int -> string -> t

  val of_address : size:int -> Elf.Address.t -> t
end

type exp = Exp.t

(** Module for optionally typed state expressions.
    Those are symbolic values which may or may not have a C type.*)
module Tval : sig
  type t = { ctyp : Ctype.t option; exp : exp }

  (** Make a new typed value with optionally a {!Ctype} *)
  val make : ?ctyp:Ctype.t -> exp -> t

  val of_exp : ?ctyp:Ctype.t -> exp -> t

  val of_var : ?ctyp:Ctype.t -> var -> t

  val of_reg : ?ctyp:Ctype.t -> id -> Reg.t -> t

  val map_exp : (exp -> exp) -> t -> t

  val iter_exp : (exp -> 'a) -> t -> 'a

  val exp : t -> exp

  val ctyp : t -> Ctype.t option

  val equal : t -> t -> bool

  val pp : t -> Pp.document
end

type tval = Tval.t

module Relocation : sig
    type t = {
        value: Exp.t;
        asserts: Exp.t list;
        target: Elf.Relocations.target;
    }

    val of_elf : Elf.Relocations.rel -> t

    val exp_of_data : Elf.Symbol.data -> (exp * exp list)
end

(** {1 State memory management } *)

(** This module manages the memory part of the state.

    The symbolic memory bounds certain symbolic address to symbolic values, but
    most addresses are not bound. This basically correspond to a concrete
    instantiation of a {!SymbolicFragment}.

    However in most case, we have some information that some symbolic value do
    not alias other symbolic values.For example in no address involving the stack
    pointer may alias any address no involving the stack pointer except in
    specific case of escaping which are explicitly not supported (yet). It is
    intended to support escaping later. There can be other kind of non-aliasing
    information in case of explicit [restrict] annotation or when implicitly
    passing or returning value by pointer (which some ABI do when such value are
    too big).

    In the absence of escaping, such problem can be solved by representing the
    memory with multiple {!SymbolicFragment}. One for the main memory, and one
    for each "restricted block". The module encapsulate all those different
    fragment.

    To manage such a system, we use the C type system to carry around provenance
    information in the type {!Ctype.provenance}. When doing a {!read} or a
    {!write}, this provenance is used to route the read or write to the right
    symbolic block.

    This means that the provenance-tracking part of the C type system must be
    part of the TCB of read-dwarf as the soundness of the symbolic memory model
    depend on it.

    Finally, the current implementation is only suitable for sequential
    execution, a new theoretical model and implementation must be developed for
    concurrent shared memory.

    Implementation detail: This type has an imperative interface even if the
    underlying {!SymbolicFragment} has a pure interface.*)
module Mem : sig
  type t

  (** Add a new fragment with the specified base *)
  val new_frag : t -> exp -> Ctype.provenance

  module Fragment : SymbolicFragment.S with type var = Var.t

  (** Get the main fragment of memory *)
  val get_main : t -> Fragment.t

  (** Get fragment *)
  val get_frag : t -> int -> Exp.t * Fragment.t 
end

(** {1 State type } *)

(** Represent the state of the machine.

    State are represented by their id and may identified to their id, they may
   not be two different state (and I mean physical equality here) with same id.
   See {!stateid}.

    A first remark must be made about mutability: The type itself has an
   imperative interface, a lot of implicitly or explicitly mutable fields.
   However, Sometime immutable version of the state are required, so the state
   has a "locking" mechanism. When the {!locked} field, the state becomes
   immutable. This is unfortunately not enforced by the type system as that
   would require to have two different types. However all mutating functions
   assert that the state is unlocked before doing the mutation. The normal
   workflow with state is thus to create them unlocked, generaly by {!copy}ing
   another state, then mutate is to make a new interesting state, and then lock
   it so that it can be passed around for it's mathematical pure meaning. To
   lock a state, use the {!lock} function.

    A second subtlety is that state are not represented in a standalone manner,
   They are represented as diff from a previous state, the {!base_state}. In the
   idea, this state should be the immediate dominator of the current state in
   the control flow graph.However a state may not represent a full node of the
   control flow graph but only the part of that node that represent control-flow
   coming from specific paths. In that case the dominator notion is only about
   those paths.

    Assertions ({!asserts}) and memory ({!mem}) are represented as diffs from
   the base state. In particular all assertion constraining the base state are
   still constraining the child state.

    This also implies a restriction on state dependent variables like
   {!Var.Register}. The id of such variables can only be the id of the current state
   or one of it's ancestor. Further more if a variable of type {!Var.ReadVar}
   exists with and id and a number, then the {!read_vars} array of the state
   with that [id] must contain that number and the sizes must match.
    Those restriction are not only about semantic meaning but also
    more practical Ocaml Gc consideration, see {!stateid}.*)
type t = private {
  id : id;
  base_state : t option;  (** The immediate dominator state in the control flow graph *)
  mutable locked : bool;  (** Tells if the state is locked *)
  mutable regs : Tval.t Reg.Map.t;  (** The values and types of registers *)
  read_vars : Tval.t Vec.t;  (** The results of reads made since base state *)
  mutable asserts : exp list;  (** Only asserts since base_state *)
  mutable relocation_asserts : exp list;  (** Only asserts since base_state *)
  mem : Mem.t;
  elf : Elf.File.t option;
      (** Optionally an ELF file, this may be used when running instructions on
          the state to provide more concrete values in certain case (like when
          reading from [.rodata]). It will affect the execution behavior.
          However the symbolic execution should always be more concrete with
          it than without it *)
  fenv : Fragment.env;  (** The memory type environment. See {!Fragment.env} *)
  mutable last_pc : Elf.Address.t;
      (** The PC of the instruction that lead into this state. The state should be
          right after that instruction. This has no semantic meaning as part of the state.
          It's just for helping knowing what comes from where *)
}

val equal : t -> t -> bool

(** {1:stateid State to id management }

    Each state has an id and the state can be refereed physically by id. This
    mean that there cannot be two different physical Ocaml state in the Gc memory
    that have the same id. Furthermore the {!id2state} map is weak and so do not
    own the state. This means that possession of an id is the same as having a
    weak pointer to the state, except for two things:
    - The id can be serialized and read to external program and files without
      losing it's meaning.
    - The id allow to break cyclical type dependency.

    This is in particular useful for variable that contain and id instead of a
    pointer to the state:
    - The variable can be serialized in text manner to a SMT solver and keep
      their meaning.
    - The {!Var.t} type can be defined before the {!t} type.

    That means that in theory a state could be Garbage collected while a
    variable still point to it. However a variable is only allowed to exists in a
    state if the id it points to is among the ancestors via the {!base_state}
    relationship of the containing state. Since {!base_state} is an GC-owning
    pointer, this ensure that while the containing state is alive, the variable
    target state is also alive.*)

(** Global map of states to associate them with identifiers *)
val id2state : (id, t) WeakMap.t

(** Next unused id *)
val next_id : id ref

(** Get a state from its [id] *)
val of_id : id -> t

(** Get the id of a state *)
val to_id : t -> id

(** {1 State management } *)

(** Lock the state. Once a state is locked it should not be mutated anymore *)
val lock : t -> unit

(** Unlock the state. This is dangerous, do not use if you do not know how to use it,
    The only realistic use case, is for calling a simplifier and thus not changing
    the semantic meaning of the state in any way while mutating it.

    This is deprecated and should disappear at some point.*)
val unsafe_unlock : t -> unit
  [@@deprecated "Stop unlocking states"]

(** Tell if the state is locked, in which case it shouldn't be mutated *)
val is_locked : t -> bool

(** Tell is state is possible.

    A state is impossible if it has a single assert that is [false]. This
    means that this symbolic state represent the empty set of concrete states.

    {!StateSimplify.ctxfull} will call the SMT solver and set the assertions to that if
    required so you should call that function before [is_possible] *)
val is_possible : t -> bool

(** Makes a fresh state with all variable fresh and new.
    This fresh state is unlocked.

    This should only be used by {!Init}, all other state should be
    derived from {!Init.state}.*)
val make : ?elf:Elf.File.t -> unit -> t

(** Do a deep copy of all the mutable part of the state, so it can be mutated
    without retro-action.

    If the source state is locked, then new state is based on it (in the sense
    of {!t.base_state}), otherwise it is a literal copy of each field.

    The returned state is always unlocked *)
val copy : ?elf:Elf.File.t -> t -> t

(** Copy the state with {!copy} if and only if it is locked.
    The returned state is always unlocked *)
val copy_if_locked : ?elf:Elf.File.t -> t -> t

val init_sections : sp:(unit -> Reg.t) -> addr_size:int -> t -> t

(** Assigns all sections with global objects to Main fragment *)
val init_sections_symbolic : sp:(unit -> Reg.t) -> addr_size:int -> t -> t


(** {1 State convenience manipulation } *)

(** Add an assertion to a state *)
val push_assert : t -> exp -> unit

(** Add an assertion to a state *)
val push_relocation_assert : t -> exp -> unit

(** Set a state to be impossible (single [false] assert). *)
val set_impossible : t -> unit

(** Set a state's [asserts] *)
val set_asserts : t -> exp list -> unit

(** Map a function on all the expressions of a state by mutating. This function,
    must preserve the semantic meaning of expression (like a simplification function)
    otherwise state invariants may be broken. *)
val map_mut_exp : (exp -> exp) -> t -> unit

(** Iterates a function on all the expressions of a state *)
val iter_exp : (exp -> unit) -> t -> unit

(** Iterates a function on all the variables of a state *)
val iter_var : (var -> unit) -> t -> unit

(** {1 State memory accessors } *)

(** Create a new {!Var.ReadVar} by mutating the state *)
val make_read : t -> ?ctyp:Ctype.t -> Ast.Size.t -> var

(** Set a {!Var.ReadVar} to a specific value in {!t.read_vars} *)
val set_read : t -> int -> exp -> unit

(** Read memory from rodata *)
val read_from_rodata : t -> addr:exp -> size:Ast.Size.t -> exp option

(** Read the block designated by [addr] and [size] from the state and return an expression read.
    This will mutate the state to bind the read result to the newly created read variable.

    The [ctyp] parameter may give a type to the read variable.
    This type is fully trusted and not checked in any way.

    The expression could be either:
      - An actual expression if the read could be resolved.
      - Just the symbolic read variable if the read couldn't be resolved

    This function is for case with [provenance] information is known.*)
val read : provenance:Ctype.provenance -> ?ctyp:Ctype.t -> t -> addr:exp -> size:Ast.Size.t -> exp

(** A wrapper around {!read} for use when there is no provenance information.
    It may able to still perform the read under certain condition and otherwise will fail.*)
val read_noprov : ?ctyp:Ctype.t -> t -> addr:exp -> size:Ast.Size.t -> exp

(** Write the provided value in the block. Mutate the state.*)
val write : provenance:Ctype.provenance -> t -> addr:exp -> size:Ast.Size.t -> exp -> unit

(** A wrapper around {!write} for use when there is no provenance information.
    It may able to still perform the write under certain condition and otherwise will fail.*)
val write_noprov : t -> addr:exp -> size:Ast.Size.t -> exp -> unit

(** {1 State register accessors } *)

(** Reset the register to a symbolic value,
    and resets the type to the provided type (or no type if not provided)*)
val reset_reg : t -> ?ctyp:Ctype.t -> Reg.t -> unit

(** Sets the content of register *)
val set_reg : t -> Reg.t -> tval -> unit

(** Sets the type of the register, leaves the value unchanged *)
val set_reg_type : t -> Reg.t -> Ctype.t -> unit

(** Get the content of the register with it's type *)
val get_reg : t -> Reg.t -> tval

(** Get the content of the register without it's type *)
val get_reg_exp : t -> Reg.t -> exp

(** Apply a function to a register. Leave the type intact *)
val update_reg_exp : t -> Reg.t -> (exp -> exp) -> unit

(** {1 Pc manipulation } *)

(** Set the PC to a concrete value and keep its type appropriate *)
val set_pc : pc:Reg.t -> t -> int -> unit

val set_pc_sym : pc:Reg.t -> t -> Elf.Address.t -> unit

(** Bump a concrete PC by a concrete bump (generally the size of a non-branching instruction *)
val bump_pc : pc:Reg.t -> t -> int -> unit

(** Try to evaluate the PC if it is concrete *)
val concretize_pc : pc:Reg.t -> t -> unit

(** Set the [last_pc] of the state *)
val set_last_pc : t -> Elf.Address.t -> unit

(** {1 Pretty printing } *)

val pp : t -> Pp.document

(** Print only the mentioned regs and the memory and asserts since the base_state.
    Until a better solution is found, the fenv will be printed entirely all the time *)
val pp_partial : regs:Reg.t list -> t -> Pp.document
