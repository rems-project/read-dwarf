(** An hash vector allow a vector to behave as hash map indexed by small integers.
    It is hash map with the identity hash function.

    If all goes well it has the same API as [(int, 'a) Hashtbl.t] for positive integers.
    Any attempt to use a negative integer will raise [Invalid_argument].
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

(** Return [Some v] if v is bound to the integer in the hash vector.
    Return [None] if nothing is bound *)
val get_opt : 'a t -> int -> 'a option

(** Return the value bound to the integer. Throw [Not_found] if the integer is not bound *)
val get : 'a t -> int -> 'a

(** Create an empty hashVector.t *)
val empty : unit -> 'a t

(** Returns a list of bindings in the hash vector *)
val bindings : 'a t -> (int * 'a) list

(** Pretty print a hashVector *)
val pp : ('a -> PP.document) -> 'a t -> PP.document
