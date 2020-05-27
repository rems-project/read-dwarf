(** This module handle the register abstraction:

    A register is defined by a path (in {!Path}) i.e a string list.

    TODO Support sail vectors (A path will be of type (string + int) list)

    Those paths are indexed by integer through a global {!IdMap} named {!index}.
    This map also fix the types of registers.

    The module also provides {!Map} and {!PMap} a respectively full and partial maps
    over registers.
*)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Paths }

    Representation of register path. The string reprensentation is with dots.
    A name may not contain dots, but this is not checked.
*)

module Path : sig
  type t = string list

  val to_string : t -> string

  val of_string : string -> t

  val pp : t -> PP.document
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Registers }

    Global register properties and accessors
*)

(** The type representing a register. The module invariant is that this type always contains
    a value bound in the global index *)
type t = private int

(** The type of a register. This is isomorphic to {!Isla.ty}. Use {!IslaConv.ty} to convert *)
type ty = Ast.no Ast.ty

val mem : t -> bool

val mem_path : Path.t -> bool

val mem_string : string -> bool

val of_path : Path.t -> t

val to_path : t -> Path.t

val of_string : string -> t

val to_string : t -> string

(** Give the current number of registers *)
val num : unit -> int

val reg_type : t -> ty

val path_type : Path.t -> ty

(** Add a new register to the global {!index}. Return it's representation *)
val add : Path.t -> ty -> t

(** Ensure that a register with that path exists with that type,
    by adding it or checking it already exists with that type.
    Return the corresponding register *)
val ensure_add : Path.t -> ty -> t

(** Same as {!add} but returns unit *)
val adds : Path.t -> ty -> unit

(** Ensure that a register with that path exists with that type,
    by adding it or checking it already exists with that type. *)
val ensure_adds : Path.t -> ty -> unit

(** Run a function over all registers *)
val iter : (Path.t -> t -> ty -> unit) -> unit

(** Pretty prints the register (Just use {!to_string}) *)
val pp : t -> PP.document

val pp_ty : ty -> PP.document

(** Prints the register index *)
val pp_index : unit -> PP.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Register maps } *)

(** This is a port of {!HashVector} to use registers. Go to the documentation of {!HashVector}.*)
module PMap : sig
  type reg = t

  type 'a t

  val empty : unit -> 'a t

  exception Exists

  val add : 'a t -> reg -> 'a -> unit

  val mem : 'a t -> reg -> bool

  val set : 'a t -> reg -> 'a -> unit

  val get_opt : 'a t -> reg -> 'a option

  (** Throw [Invalid_argument] if the register is not bound *)
  val get : 'a t -> reg -> 'a

  val pp : ('a -> PP.document) -> 'a t -> PP.document
end

(** This is a port of {!FullVec} to use registers. Go to the documentation of {!FullVec}

    However, because the domain of register is finite, some extra function are available
    like {!iter} and {!iteri}.

    If register are added with {!add}, they are automatically implicitely added to the {!Map}
    and the generator must accept those new values. The generator will never be called
    on invalid register values (i.e. when the generator is called on a register,
    the former can get the latter's type and name)
*)
module Map : sig
  type reg = t

  type 'a t

  (** Dummy value that will fail as soon as it is used *)
  val dummy : unit -> 'a t

  (** Initialize the map with a generator *)
  val init : (reg -> 'a) -> 'a t

  (** Clear the map and restart with this generator *)
  val reinit : 'a t -> (reg -> 'a) -> unit

  (** Make a copy of the map *)
  val copy : 'a t -> 'a t

  (** Set the value of a register *)
  val set : 'a t -> reg -> 'a -> unit

  (** Get the value of a register *)
  val get : 'a t -> reg -> 'a

  (** Map the function all the registers (including future, not yet added ones) *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Map the function on all the register by mutation (including future ones) *)
  val map_mut : ('a -> 'a) -> 'a t -> unit

  (** Map the function on all current register. Future registers are unchanged *)
  val map_mut_current : ('a -> 'a) -> 'a t -> unit

  val iter : ('a -> unit) -> 'a t -> unit

  val iteri : (reg -> 'a -> unit) -> 'a t -> unit

  (** Give all the registers bindings *)
  val bindings : 'a t -> (reg * 'a) list

  (** Contrary to {!FullVector.pp}, this one will print the binding of all registers,
      and may call the generator to do that *)
  val pp : ('a -> PP.document) -> 'a t -> PP.document
end
