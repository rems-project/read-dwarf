(** This module is about testing Z3 simplify against Z3 check. It's not really
    about testing Z3 itself but about testing our parsing of Z3 expression output
    (in particular let bindings unfolding) *)

open Common

let var_exp_gen_ty size ty =
  ExpGen.Gen.(
    exp_from_params
      { typ = ty; size; bv_atom_gen = bv_atom_with_var; bool_atom_gen = bool_atom_with_var })

let var_exp_gen size = Gen.(ExpGen.Gen.typ >>= var_exp_gen_ty size)

(** Shrink by trying all sub expressions *)
let const_exp_shrinker_top exp yield = Ast.Manip.direct_exp_iter_exp yield exp

(** Shrink by replacing a non-atomic expression by it's simplified version *)
let const_exp_shrinker_bot exp yield =
  if Ast.is_atomic exp then ()
  else
    try
      let simplified = ExpGen.Z3.simplify_full exp in
      yield simplified
    with _ -> (* If any exception happens, we don't shrink *)
              ()

(** Shrink by using both {!const_exp_shrinker_top} and {!const_exp_shrinker_bot} *)
let const_exp_shrinker exp yield =
  const_exp_shrinker_top exp yield;
  ExpGen.shrink_propagate const_exp_shrinker_bot exp yield

let var_exp = ExpGen.from_gen ~shrink:const_exp_shrinker Gen.(small_nat >>= var_exp_gen)

let simplify_check =
  QCT.make ~count:100 ~name:"Z3 simplify against Z3 check" var_exp (fun exp ->
      try
        let simplified = ExpGen.Z3.simplify_full exp in
        let asserteq = Exp.Typed.(exp = simplified) in
        match ExpGen.Z3.check_full asserteq with Some b -> b | None -> Q.assume_fail ()
        (* Sometime Z3 can't check it's own simplifcation.
           This is both rare and weird. I just ask to restart in that case *)
      with exn ->
        Z3.reset ();
        Raise.again exn)

let tests = [simplify_check]
