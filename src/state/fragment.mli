(** This module define a memory fragment to be used by C types *)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Fragments } *)

(** {!obj} is just {!Ctype.t}, but a bug in odoc hides that information.
    {!Fragment.obj} actually do not exists.

    The odoc PR is #349 on github, we just have to wait for it to be merged.*)
include RngMap.S with type obj := Ctype.t

val pp : t -> Pp.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Environment } *)

module Env : sig
  type frag = t

  type t = { frags : frag Vec.t }

  val make : unit -> t

  val copy : t -> t

  (** Add the provided type at [addr] into a fragment indexed by [id] in the environment *)
  val add_typ : addr:int -> Ctype.t -> t -> id:int -> unit

  (** Add a new fragment to the environment. {!empty} by default. *)
  val adds_frag : ?frag:frag -> t -> unit

  (** Add a new fragment to the environment, returns the id of the new fragment.
    [frag] is {!empty} by default. *)
  val add_frag : ?frag:frag -> t -> int

  val get : t -> int -> frag

  val set : t -> int -> frag -> unit

  val pp : t -> Pp.document
end

type env = Env.t
