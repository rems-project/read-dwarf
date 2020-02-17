type t = int

type typ = Plain of Isla.ty | Struct of reg_struct

and reg_struct

val make_struct : unit -> reg_struct

val assert_plain : typ -> Isla.ty

val index : reg_struct

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
(*        Equality                                                           *)
(*****************************************************************************)

val struct_weak_eq : reg_struct -> reg_struct -> bool

val type_weak_eq : typ -> typ -> bool

(*****************************************************************************)
(*        Adding                                                             *)
(*****************************************************************************)

val add_field : reg_struct -> string -> typ -> t

val add_reg : string -> typ -> t

(*****************************************************************************)
(*        Path manipulation                                                  *)
(*****************************************************************************)

type path = t list

val partial_path_to_string : reg_struct -> path -> string

val path_to_string : path -> string

val path_to_string_list : path -> string list

val partial_path_of_string : reg_struct -> string -> path

val path_of_string : string -> path

val path_of_string_list : string list -> path

val partial_path_type : reg_struct -> path -> typ

val path_type : path -> typ

(*****************************************************************************)
(*        Register indexed mapping                                           *)
(*****************************************************************************)

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
(*        Pretty Printing                                                    *)
(*****************************************************************************)

val pp : t -> PP.document

val pp_field : reg_struct -> t -> PP.document

val pp_rstruct : reg_struct -> PP.document

val pp_rtype : typ -> PP.document
