(** This module provide sum manipulation functionality on top of typed expression {!ExpTyped.t} *)

(** Split an expression as a list of terms. This function sees through +,- and extracts.

    TODO I need to sort according to an arbitrary order to be able to compare reliably.
    This will probably be part of a more general simplifier work.*)
val split : ('v, 'm) ExpTyped.t -> ('v, 'm) ExpTyped.t list

(** Merge a list of terms into a sum expression.
    The [size] is required if the empty list if given to put and appropriately sized [0] *)
val merge : size:int -> ('v, 'm) ExpTyped.t list -> ('v, 'm) ExpTyped.t

(** Add a [term] to a sum *)
val add_term : term:('v, 'm) ExpTyped.t -> ('v, 'm) ExpTyped.t -> ('v, 'm) ExpTyped.t

(** Remove a [term] from a sum.
    Return [Some res] if successful and [None] otherwise. *)
val remove_term :
  equal:(('v, 'm) ExpTyped.t -> ('v, 'm) ExpTyped.t -> bool) ->
  term:('v, 'm) ExpTyped.t ->
  ('v, 'm) ExpTyped.t ->
  ('v, 'm) ExpTyped.t option

(** Same as {!remove_term} but if the [term] is not found, add the opposite to the sum*)
val smart_substract :
  equal:(('v, 'm) ExpTyped.t -> ('v, 'm) ExpTyped.t -> bool) ->
  term:('v, 'm) ExpTyped.t ->
  ('v, 'm) ExpTyped.t ->
  ('v, 'm) ExpTyped.t

(** Split away the concrete terms of the sum and the symbolic part.
    The symbolic part can be [None] if the expression was concrete *)
val split_concrete : ('v, Ast.no) ExpTyped.t -> ('v, Ast.no) ExpTyped.t option * BitVec.t

(** Tells if an expression has a concrete term *)
val has_concrete_term : ('v, 'm) ExpTyped.t -> bool
