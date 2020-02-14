(** An hash vector allow a vector to behave as hash map indexed by small integers.
    It is hash map with the identity hash function.

    If all goes well it has the same API as [(int, 'a) Hashtbl.t]
*)

(** The type of the hash vector *)
type 'a t

(** Check if an element is set *)
val mem : 'a t -> int -> bool

exception Exists

(** Set a value. Create a new binding if necessary *)
val set : 'a t -> int -> 'a -> unit

(** Create a new binding. Throws {!Exists} if a binding already exists *)
val add : 'a t -> int -> 'a -> unit

(** Retrieves the value and throw [Not_found] if that value in not bound *)
val get : 'a t -> int -> 'a

(** Create an empty hashVector.t *)
val empty : unit -> 'a t

(** Pretty print a hashVector *)
val pp : ('a -> PP.document) -> 'a t -> PP.document
