(** This module contain random utility dealing with pairs *)

(** Map each function on one side of the pair *)
val map : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd

(** Iter each function on one side of the pair *)
val iter : ('a -> unit) -> ('b -> unit) -> 'a * 'b -> unit

(** Swap the element of a pair *)
val swap : 'a * 'b -> 'b * 'a

(** Compare a pair using provided comparison function. Both fst and std default to the
    polymorphic compare *)
val compare : ?fst:('a -> 'a -> int) -> ?snd:('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
