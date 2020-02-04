(** Module to represent the SMT common machinery and interface *)

(** The interface any SMT solver module must implement *)
module type Smt = sig
  (** Simplify an expression *)
  val simplify : State.exp -> State.exp

  (** Check that a list of assertion is true. None means unknown *)
  val check : State.exp list -> bool option
end
