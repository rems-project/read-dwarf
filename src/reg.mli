(** This module handle the register abstraction:
    - map string to identifiers
    - remember register characteristics (for now only machine types)
    - remember structure of register (like PSTATE)

    It also provide {!Reg.Map}, a map indexed by registers
*)

(** The type representing a register. *)
type t = private int

(** The type of a register
    (This is the machine type and has nothing to do with any C type inference)

    TODO: Support Sail vectors.
*)
type typ = Plain of Isla.ty | Struct of reg_struct

(** The type of register structure, i.s a map from field to sub-registers.
    This type is mutable *)
and reg_struct

(** Build a new register structure *)
val make_struct : unit -> reg_struct

(** Assert that a type is plain and return the underlying type.
    Throw [Failure] if the type is not plain *)
val assert_plain : typ -> Isla.ty

(** The global register index. Any top level register is a field of this structure *)
val index : reg_struct

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Accessors } *)

val field_to_string : reg_struct -> t -> string

val to_string : t -> string

val field_of_string : reg_struct -> string -> t

val of_string : string -> t

val mem : t -> bool

val mem_string : string -> bool

val num_field : reg_struct -> int

val num_reg : unit -> int

val field_type : reg_struct -> t -> typ

val reg_type : t -> typ

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Weak equality }

    Check that all the field match and have the same types.
    (The integer indexing may be different).

    The same type means the same properties recursively on sub structures.
*)

val struct_weak_eq : reg_struct -> reg_struct -> bool

val type_weak_eq : typ -> typ -> bool

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Adding elements } *)

(** Add a filed to a structure. *)
val add_field : reg_struct -> string -> typ -> t

(** Add a new register by name and throws Bimap.Exists if it already exists.
    Return the internal representation of the register*)
val add_reg : string -> typ -> t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Paths }

    A path is an ordered list of fields. that represent a precise field in a sub structure.

    A path of length one is just a plain register.
*)

(** The type of a path *)
type path = t list

(** A fancy name for the empty list *)
val empty_path : path

(** Convert a path in a structure to dot separated string *)
val partial_path_to_string : reg_struct -> path -> string

(** Convert a path to a dot separated string. Basically {!partial_path_to_string}{!index} *)
val path_to_string : path -> string

(** Convert a path to a list of field names *)
val path_to_string_list : path -> string list

(** Parse a dot separated string in the context of a given structure *)
val partial_path_of_string : reg_struct -> string -> path

(** Parse a dot separated string in the top level context *)
val path_of_string : string -> path

(** Build a path from a list of field names *)
val path_of_string_list : string list -> path

(** Get the type of a field in struture *)
val partial_path_type : reg_struct -> path -> typ

(** Get the type of a register field with a absolute path *)
val path_type : path -> typ

(** Pretty print a path *)
val pp_path : path -> PP.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Register indexed map } *)

(** This sub module defines a register map that associate a value to all
    architectural registers. This is a mutable data structure *)
module Map : sig
  (** The type of a map that bind one 'a to each register *)
  type 'a t

  (** Dummy value, UNSAFE : UB to use this value *)
  val dummy : unit -> 'a t

  (** Initialize by setting each value using the init function on the path *)
  val init : (path -> 'a) -> 'a t

  (** Get the value associated to a register *)
  val get : 'a t -> path -> 'a

  (** Set the value associated to a register *)
  val set : 'a t -> path -> 'a -> unit

  (** Map on the data structure, return a new instance *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Map on the data structure, mutating the given instance *)
  val map_mut : ('a -> 'a) -> 'a t -> unit

  (** Copy the data structure, return an new independent instance *)
  val copy : 'a t -> 'a t

  (** Run the function on each value stored in the map *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** Do a copy of the map but add value for all register that have been added
      since the creation of the previous register map. Values are initialized using init *)
  val copy_extend : init:(path -> 'a) -> 'a t -> 'a t

  (** Pretty print the map *)
  val pp : ('a -> PP.document) -> 'a t -> PP.document
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

val pp : t -> PP.document

val pp_field : reg_struct -> t -> PP.document

val pp_rstruct : reg_struct -> PP.document

val pp_rtype : typ -> PP.document
