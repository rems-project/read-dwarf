(** This module provide a simple way of associated key to value without keeping them alive
    The key is owned by the map but the value is weakly pointed to.
    When the value is deleted by the GC, the binding disappears.

    The value type must be heap allocated (basically not an integer or another weird type)
*)

(** The type of the weak map *)
type ('a, 'b) t

(** The initial map *)
val create : int -> ('a, 'b) t

exception Exists

(** Add a mapping to the map. raise Exists if a is already mapped *)
val add : ('a, 'b) t -> 'a -> 'b -> unit

(** Retrieves a value from the table. Throws Hashtbl.Not_found if the binding do not exists  *)
val get : ('a, 'b) t -> 'a -> 'b
