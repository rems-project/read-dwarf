(** This module provide some [Cmdliner] helper functions. *)

open Cmdliner

(** Return a unit [Term] that evaluates the input term and
    set the reference to the resulting value.

    Due to the nature of Cmdliner, this may be evaluated multiple times, so
    setting the reference to anything else is dangerous.
*)
let setter reference term =
  let set r t = r := t in
  Term.(const (set reference) $ term)

(** Add an unit term that need to be evaluated at the same time at the main term.

    The order of evaluation is unspecified, but the option will be evaluated
    before the resulting term is returned.
*)
let add_option opt term =
  let g a () = a in
  Term.(const g $ term $ opt)

(** Fold add_option on a list of option *)
let add_options olist term = List.fold_left (Fun.flip add_option) term olist

(** Replaces Term.const but allow a unit terms (like the one generated by {!setter}) to
    be evaluated before the function is called *)
let func_option opt func = add_option opt Term.(const func)

(** Same as {!func_option} but with a list of unit terms *)
let func_options olist func = add_options olist Term.(const func)
