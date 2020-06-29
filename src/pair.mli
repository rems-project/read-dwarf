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

(** Test pair equality using provided equality function. Both fst and std default to the
    polymorphic equality *)
val equal : ?fst:('a -> 'a -> bool) -> ?snd:('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool

(** Just build the pair, not useful on it's own but
    [List.combine] is just [List.map2 Pair.make], and there are some other cases where
    it is handy *)
val make : 'a -> 'b -> 'a * 'b

(** Just build a pair with twice the element. Useful in high order programming *)
val split : 'a -> 'a * 'a

(** Check that both individual predicates hold *)
val for_all : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool

(** Check that at least one of the individual predicates holds *)
val exists : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool
