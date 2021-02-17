(** This module is for testing {!ConcreteEval} *)

open Common
module ConcreteEval = Exp.ConcreteEval
module Value = Exp.Value
module Typed = Exp.Typed

let const_exp_gen_ty size ty =
  ExpGen.Gen.(
    exp_from_params { typ = ty; size; bv_atom_gen = bv_consts; bool_atom_gen = bool_consts })

let const_exp_gen size = Gen.(ExpGen.Gen.typ >>= const_exp_gen_ty size)

(** Shrink by trying all sub expressions *)
let const_exp_shrinker_top exp yield = Ast.Manip.direct_exp_iter_exp yield exp

(** Shrink by replacing a non-atomic constant expression by it's constEval evaluation *)
let const_exp_shrinker_bot exp yield =
  if Ast.is_atomic exp then ()
  else
    let value = ConcreteEval.eval exp in
    let expv = Value.to_exp value in
    yield expv

(** Shrink by using both {!const_exp_shrinker_top} and {!const_exp_shrinker_bot} *)
let const_exp_shrinker exp yield =
  const_exp_shrinker_top exp yield;
  ExpGen.shrink_propagate const_exp_shrinker_bot exp yield

let const_exp = ExpGen.from_gen ~shrink:const_exp_shrinker Gen.(small_nat >>= const_exp_gen)

let concrete_eval =
  QCT.make ~count:1000 ~name:"ConcreteEval against Z3" const_exp (fun exp ->
      try
        let value = ConcreteEval.eval exp in
        (* PP.(eprintln @@ dprintf "exp:" ^^ ExpGen.ExpT.pp_smt exp); *)
        (* PP.eprintln (Value.pp value); *)
        let expv = Value.to_exp value in
        let asserteq = Typed.(exp = expv) in
        ExpGen.Z3.check_full asserteq = Some true
      with Division_by_zero -> Q.assume_fail ()
      (* We want to assume there is no division by 0 *))

let tests = [concrete_eval]
