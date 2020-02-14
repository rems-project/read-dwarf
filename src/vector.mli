(** This module provide a resizable array. It is a rename of Res.array with added features *)

(** The vector type *)
type 'a t

val length : 'a t -> int

val empty : unit -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit

val to_list : 'a t -> 'a list

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Add a new element at the end of the vector *)
val add_one : 'a t -> 'a -> unit

(** Remove the last element of the vector *)
val remove_one : 'a t -> unit

(** Remove the last n elements of the vector *)
val remove_n : 'a t -> int -> unit

val to_array : 'a t -> 'a array

val of_array : 'a array -> 'a t

(** Ensure that the vector has size at least the int by resizing if needed
    The value provided will be use to set the newly created values.*)
val ensure : 'a t -> int -> 'a -> unit

(** Resize the vector to the specified size.
    The value provided will be use to set the newly created values if relevant *)
val resize : 'a t -> int -> 'a -> unit

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** Vector pretty printer *)
val pp : ('a -> PP.document) -> 'a t -> PP.document
