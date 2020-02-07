(** This module provide a structure to allow internalizing (with integers) of some type *)

type 'a t

exception Exists

val make : unit -> 'a t

val add_ident : 'a t -> 'a -> int

val to_ident : 'a t -> 'a -> int

val of_ident : 'a t -> int -> 'a

val mem : 'a t -> 'a -> bool

val mem_id : 'a t -> int -> bool

val iter : ('a -> int -> unit) -> 'a t -> unit

val vec : 'a t -> 'a Vector.t

val pp : ('a -> PP.document) -> 'a t -> PP.document
