type t = int

type typ = Plain of Isla.ty | Struct of reg_struct

and reg_struct

val make_struct : unit -> reg_struct

val assert_plain : typ -> Isla.ty

val index : reg_struct

val field_to_string : reg_struct -> t -> string

val to_string : t -> string

val field_of_string : reg_struct -> string -> t

val of_string : string -> t

val mem : t -> bool

val mem_string : string -> bool

val num_field : reg_struct -> int

val num_reg : unit -> int

val field_type : reg_struct -> t -> typ

val reg_type : t -> typ

(*****************************************************************************)
(*        Equality                                                           *)
(*****************************************************************************)

val struct_weak_eq : reg_struct -> reg_struct -> bool

val type_weak_eq : typ -> typ -> bool

(*****************************************************************************)
(*        Adding                                                             *)
(*****************************************************************************)

val add_field : reg_struct -> string -> typ -> t

val add_reg : string -> typ -> t

(*****************************************************************************)
(*        Path manipulation                                                  *)
(*****************************************************************************)

type path = t list

val partial_path_to_string : reg_struct -> path -> string

val path_to_string : path -> string

val path_to_string_list : path -> string list

val partial_path_of_string : reg_struct -> string -> path

val path_of_string : string -> path

val path_of_string_list : string list -> path

val partial_path_type : reg_struct -> path -> typ

val path_type : path -> typ


(*****************************************************************************)
(*        Register indexed mapping                                           *)
(*****************************************************************************)

module Map : sig
  type 'a cell = MPlain of 'a | MStruct of 'a t

  and 'a t = 'a cell array

  val dummy : unit -> 'a t

  val init : (path -> 'a) -> 'a t

  val get_mut_cell : 'a t -> path -> 'a cell ArrayCell.t

  val get_cell : 'a t -> path -> 'a cell

  val get : 'a t -> path -> 'a

  val set : 'a t -> path -> 'a -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t

  val copy : 'a t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit

  module PP : sig
    val rmap : ('a -> PP.document) -> 'a t -> PP.document
  end
end

(*****************************************************************************)
(*        Pretty Printing                                                    *)
(*****************************************************************************)

module PP : sig
  val reg : t -> PP.document

  val field : reg_struct -> t -> PP.document

  val rstruct : reg_struct -> PP.document

  val rtype : typ -> PP.document
end
