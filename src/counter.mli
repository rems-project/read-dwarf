(** This module provide a small counter object which is just a int reference on which
    {!get} can be called to get an identifier and increment the reference *)

(** The type of a counter. *)
type t

(** Make a counter starting from the provided value. This means the first call
    to {!get} on that counter will be the input value:
    {[
    let c = make 42 in
    assert(get c = 42);
    ]}
*)
val make : int -> t

(** Get the next value of the counter and increment it *)
val get : t -> int

(** Get the current value of the counter *)
val read : t -> int

(** Skip a value of the counter equivalent to [ingore (get ...)] *)
val skip : t -> unit
