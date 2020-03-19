(** An IdMap is a map that associate an id to each key (and thus to each value).

    The value can be indexed with the key or with the id.

    The key can be retrieved from the id and vice versa.

    Values can be retreived from both keys and value (

    The id is an [int]
*)

(** The type of a idmap

    ['a] is type of keys that index the structure

    ['b] is the type of value that are indexed.
*)
type ('a, 'b) t

(** Gives the number of bindings in the idmap *)
val length : ('a, 'b) t -> int

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Creation and adding } *)

(** Create a new idmap from scratch *)
val make : unit -> ('a, 'b) t

val add : ('a, 'b) t -> 'a -> 'b -> int

(** Silent version of {!add} that ignore the result *)
val adds : ('a, 'b) t -> 'a -> 'b -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Keys} *)

(** Convert a key in an identifier. Throws if the key is not bound *)
val to_ident : ('a, 'b) t -> 'a -> int

(** Convert an identifier to its corresponding key. Throws if the id is not bound *)
val of_ident : ('a, 'b) t -> int -> 'a

(** Check if a key is bound *)
val mem : ('a, 'b) t -> 'a -> bool

(** Check if an id is bound *)
val mem_id : ('a, 'b) t -> int -> bool

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Accessors } *)

(** Get a value by key *)
val getk : ('a, 'b) t -> 'a -> 'b

(** Get a value by id *)
val geti : ('a, 'b) t -> int -> 'b

(** Get a value by id, unsafe *)
val unsafe_geti : ('a, 'b) t -> int -> 'b

(** Set a value by key *)
val setk : ('a, 'b) t -> 'a -> 'b -> unit

(** Set a value by id *)
val seti : ('a, 'b) t -> int -> 'b -> unit

(** Set a value by id, unsafe *)
val unsafe_seti : ('a, 'b) t -> int -> 'b -> unit

(** Bind the value to all the keys *)
val fill_all : ('a, 'b) t -> 'b -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Functional accessors } *)

(** Call the function on all the bindings of the idmap *)
val iter : ('a -> int -> 'b -> unit) -> ('a, 'b) t -> unit

(** Call the function on all the bindings of the idmap and return the list of results *)
val map_to_list : ('a -> int -> 'b -> 'c) -> ('a, 'b) t -> 'c list

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty print } *)

(** Pretty prints *)
val pp :
  ?name:string ->
  keys:('a -> PP.document) ->
  vals:('b -> PP.document) ->
  ('a, 'b) t ->
  PP.document
