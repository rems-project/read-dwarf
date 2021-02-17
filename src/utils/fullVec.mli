(** A full vector is a vector in which all non-negative integer are bound.

    This consist in a normal vector and a function and all the bindings of value after
    the vector end are the result of the function call on that integer.

    It is guaranted that the function will be called lazily on integer when required
    in stricly increasing order (and thus never twice on the same integer).
    However an integer may be skipped (When manually set). That means the function can have
    side effect if necessary. However if the structure is copied ({!map} or {!copy}),
    The generator may be called multiple time on some integer in the different copies.

    Both set and get may generate calls to the generator if required.

    Any attempt to use a negative integer will raise [Invalid_argument].

    A negative integer will never be passed to the generator.
*)

type 'a t

(** Create a full vector from a generator *)
val make : (int -> 'a) -> 'a t

(** Make a copy that can be mutated separately *)
val copy : 'a t -> 'a t

(** Set the binding of that integer *)
val set : 'a t -> int -> 'a -> unit

(** Set the binding of all integer after this value by supplying a new generator.
    The former generator is discarded. The bindings before the value
    keep their value (if there were not generated, they will be before discarding
    the old generator *)
val set_after : 'a t -> int -> (int -> 'a) -> unit

(** Get the binding of that integer *)
val get : 'a t -> int -> 'a

(** Get the underlying vector *)
val get_vec : 'a t -> 'a Vec.t

(** Get a vector containing at least the elements until the specified value excluded *)
val get_vec_until : 'a t -> int -> 'a Vec.t

(** Map the function over the fullvec. Postcompose the map on the generator *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Map the function over the fullvector by mutation. Postcompose the map on the generator.contents
    Warning, a lot of {!map_mut} may make the generator big and slow.
    Maybe try to use {!set_after} to reset it when required.*)
val map_mut : ('a -> 'a) -> 'a t -> unit

(** Map the function over the fullvector until the limit. The rest is unchanged *)
val map_mut_until : limit:int -> ('a -> 'a) -> 'a t -> unit

(** Iterate until the specified value (excluded).
    (The FullVec is infinite so you can't iter on all of it) *)
val iter_until : limit:int -> ('a -> unit) -> 'a t -> unit

(** Same as {!iter_until} but with the index *)
val iteri_until : limit:int -> (int -> 'a -> unit) -> 'a t -> unit

(** Only prints the non-default values *)
val pp : ('a -> Pp.document) -> 'a t -> Pp.document
