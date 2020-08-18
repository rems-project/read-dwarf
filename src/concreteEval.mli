(** This module provides a way of making concrete evaluation of an expression.
    The only required thing is a {!context}.*)

(** Thrown when trying to concretely evaluate a symbolic expression *)
exception Symbolic

(** A map from variables to concrete values.
    This map should throw {!Symbolic} when the variable should be treated as symbolic.*)
type 'v context = 'v -> Value.t

(** Evaluate concretely an expression and return a {!Value}.

    If the expression is not concrete, it will throw {!Symbolic}.

    The default [ctxt] consider all variables to be symbolic.

    [eval] may succeed even if {!is_concrete} is [false] in presence of
    conditionals. Indeed only the taken branch of the conditional is evaluated,
    so the other may be symbolic. For example:

    {[ eval ExpTyped.(ite ~cond:(bool true) (bits_smt #x2a) (var ...)) = ExpTyped.(bits_smt #x2a)]}

*)
val eval : ?ctxt:'v context -> ('a, 'v, Ast.no, Ast.no) Ast.exp -> Value.t

(** Tell if an expression is concrete, which means no variables of any kind *)
val is_concrete : _ Ast.exp -> bool

(** Evaluate an expression if it's concrete and returns [None] otherwise *)
val eval_if_concrete : ('a, 'v, Ast.no, Ast.no) Ast.exp -> Value.t option
