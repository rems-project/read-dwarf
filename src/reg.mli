type t = int

type typ = Plain of int | Struct of reg_struct

and reg_struct

val make_struct : unit -> reg_struct

val index : reg_struct

val field_to_string : reg_struct -> t -> string

val to_string : t -> string

val field_from_string : reg_struct -> string -> t

val from_string : string -> t

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

val add_plain_field : reg_struct -> string -> int -> t

val add_reg : string -> typ -> t

val add_plain_reg : string -> int -> t

(*****************************************************************************)
(*        Pretty Printing                                                    *)
(*****************************************************************************)

module PP : sig
  val reg : t -> PP.document

  val field : reg_struct -> t -> PP.document

  val rstruct : reg_struct -> PP.document

  val rtype : typ -> PP.document
end
