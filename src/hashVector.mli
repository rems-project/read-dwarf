type 'a t

val mem : 'a t -> int -> bool

exception Exists

val set : 'a t -> int -> 'a -> unit

val add : 'a t -> int -> 'a -> unit

val get : 'a t -> int -> 'a

val empty : unit -> 'a t

module PP : sig
  val hvector : ('a -> PP.document) -> 'a t -> PP.document
end
