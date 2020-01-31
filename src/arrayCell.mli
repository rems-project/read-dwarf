(** This module represent a simple array cell that can be mutated *)

type 'a t = { arr : 'a array; pos : int }

val make : 'a array -> int -> 'a t

val set : 'a t -> 'a -> unit

val get : 'a t -> 'a
