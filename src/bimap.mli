(** This module provide a structure to allow internalizing (with integers) of some type.
    It will create an integer identifier for each value and allow conversion both ways.
    {!Bimap.t} is mutable and has an imperative interface.
*)

(** The type of bimaps *)
type 'a t

(** Thrown by {!add_ident} when the binding already exists *)
exception Exists

(** Create a new bimap *)
val make : unit -> 'a t

(** Add an new binding to the table

    Returns the new integer identifier of the value

    Throws {!Exists} if the value is already bound
*)
val add_ident : 'a t -> 'a -> int

(** Convert a value in its integer identifier *)
val to_ident : 'a t -> 'a -> int

(** Convert an integer identifier to its value *)
val of_ident : 'a t -> int -> 'a

(** Check if a value is bound *)
val mem : 'a t -> 'a -> bool

(** Check if an identifier is bound *)
val mem_id : 'a t -> int -> bool

(** Run a function on all the pair value identifier *)
val iter : ('a -> int -> unit) -> 'a t -> unit

(** Return the underlying vector. Do not mutate it! *)
val vec : 'a t -> 'a Vector.t

(** Pretty print the bimap *)
val pp : ('a -> PP.document) -> 'a t -> PP.document
