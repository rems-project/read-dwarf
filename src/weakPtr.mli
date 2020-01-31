(** This module provide a simple weak pointer that can be rendered invalid by the GC *)

(** The type of a weak pointer *)
type 'a t

(** Make a new empty pointer *)
val empty : unit -> 'a t

(** Make a new pointer and set it with the given value *)
val make : 'a -> 'a t

(** Set or clear the value inside the pointer depending of the option *)
val seto : 'a t -> 'a option -> unit

(** Set the value inside the pointer *)
val set : 'a t -> 'a -> unit

(** Clear the value inside the pointer *)
val reset : 'a t -> unit

(** Retrieves the value and return None if the pointer is now empty *)
val geto : 'a t -> 'a option

(** Raised when accessing and empty ptr *)
exception Deleted

(** Retrieves the value. Will raise Deleted if the value was deleted by the GC *)
val get : 'a t -> 'a

(** Check if the pointer is filled *)
val check : 'a t -> bool
