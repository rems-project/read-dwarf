(** This module define a memory fragment to be used by C types *)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Fragments } *)

(** {!obj} is just {!Ctype.t}, but a bug in odoc hides that information.
    {!Fragment.obj} actually do not exists.

    The odoc PR is #349 on github, we just have to wait for it to be merged.*)
include
  RngMap.S with type obj := Ctype.t

val pp : t -> PP.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Environment } *)

module Env : sig
  type frag = t

  type t = { frags : frag Vector.t }

  val make : unit -> t

  val copy : t -> t

  (** Add the provided type at [addr] into a fragment indexed by [id] in the environment *)
  val add_typ : tenv:Ctype.env -> addr:int -> Ctype.t -> t -> id:int -> unit

  (** Add a new fragment to the environment. {!empty} by default. *)
  val adds_frag : ?frag:frag -> t -> unit

  (** Add a new fragment to the environment, returns the id of the new fragment.
    [frag] is {!empty} by default. *)
  val add_frag : ?frag:frag -> t -> int

  val pp : t -> PP.document
end

type env = Env.t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 At } *)

(** Dereference a pointer type with size of [size] and give the pointed to type.
    Requires a type environment [env] and the dynamic fragment environment [fenv] *)
val ptr_deref :
  env:Ctype.env -> fenv:env -> size:int -> Ctype.fragment -> Ctype.offset -> Ctype.t option

(** Dereference a pointer type with size of [size] and write a type to it *)
val ptr_write :
  env:Ctype.env -> fenv:env -> ctyp:Ctype.t -> Ctype.fragment -> Ctype.offset -> unit
