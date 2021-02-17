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

(** Thrown when adding an existing value *)
exception Exists

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Creation and adding } *)

(** Create a new idmap from scratch *)
val make : unit -> ('a, 'b) t

(** Add a binding, and throw {!Exists} if the binding already exists *)
val add : ('a, 'b) t -> 'a -> 'b -> int

(** Silent version of {!add} that ignore the result *)
val adds : ('a, 'b) t -> 'a -> 'b -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Keys} *)

(** Convert a key in an identifier. Throws if the key is not bound *)
val to_ident : ('a, 'b) t -> 'a -> int

(** Convert a key in an identifier. None if the key is not bound *)
val to_ident_opt : ('a, 'b) t -> 'a -> int option

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

(** Get a value by key. Raise [Not_found] if the key is not bound. *)
val getk : ('a, 'b) t -> 'a -> 'b

(** Get a value by key. None if the key is not bound. *)
val getk_opt : ('a, 'b) t -> 'a -> 'b option

(** Get a value by id. Raise [Invalid_argument] if the index is not bound. *)
val geti : ('a, 'b) t -> int -> 'b

(** Get a value by id, unsafe. *)
val unsafe_geti : ('a, 'b) t -> int -> 'b

(** Set a value by key. Raise [Not_found] if the key is not bound.  *)
val setk : ('a, 'b) t -> 'a -> 'b -> unit

(** Set a value by id. Raise [Invalid_argument] if the index is not bound. *)
val seti : ('a, 'b) t -> int -> 'b -> unit

(** Set a value by id, unsafe. *)
val unsafe_seti : ('a, 'b) t -> int -> 'b -> unit

(** Bind the value to all the keys with the specified value. *)
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
  keys:('a -> Pp.document) ->
  vals:('b -> Pp.document) ->
  ('a, 'b) t ->
  Pp.document
