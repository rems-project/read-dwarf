(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module intends to provider a convenience expression module by lifting
    function like equality or pretty printing from variable to expressions.

    It is restricted to typed expression as defined in {!Typed}

    For now it also do not support memory expression, but it will soon.
*)

module ConcreteEval = ConcreteEval
module PpExp = PpExp
module Sums = Sums
module Typed = Typed
module Value = Value

(** The type of memory-less expressions *)
type ty = Ast.no Ast.ty

(** This is the type signature for variable required by this module *)
module type Var = sig
  (** The type of variables *)
  type t

  (** Equality predicate that will be passed to expressions *)
  val equal : t -> t -> bool

  (** Pretty printer to be used, both for memory pretty printing and for sending memory to Z3 *)
  val pp : t -> Pp.document

  (** Get the type of the variable *)
  val ty : t -> ty
end

(** The signature of the output module of {!Make} *)
module type S = sig
  (** The type of variable provided in the functor *)
  type var

  (** The type of expression on which this module works *)
  type t = (var, Ast.no) Typed.t

  (** Test syntactic equality. [a + b] and [b + a] would test different under this predicate *)
  val equal : t -> t -> bool

  (** Pretty print the expression using {!PpExp} *)
  val pp : t -> Pp.document

  (** Pretty print the expression in SMTLIB language *)
  val pp_smt : t -> Pp.document

  (** Create an expression from a variable *)
  val of_var : var -> t

  (** Convert a similar but untyped expression to an expression of type {!t} *)
  val add_type : ('a, var, Ast.no, Ast.no) Ast.exp -> t
end

module Make (Var : Var) : S with type var = Var.t = struct
  open Ast

  type var = Var.t

  type t = (var, Ast.no) Typed.t

  let equal (e1 : t) (e2 : t) = equal_exp ~annot:( = ) ~var:Var.equal e1 e2

  let pp (exp : t) = PpExp.pp_exp (fun v -> Pp.(bar ^^ Var.pp v ^^ bar)) exp

  let pp_smt (exp : t) = pp_exp Var.pp (Ast.Manip.allow_lets @@ Ast.Manip.allow_mem exp)

  let of_var v = Typed.var ~typ:(Var.ty v) v

  let add_type e = Typed.add_type ~ty_of_var:(Fun.const Var.ty) e
end

module Pp = PpExp

