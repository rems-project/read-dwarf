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
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
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

(** This module extends the base OCaml API of
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Option.html}[Option]}.

    In particular, it adds:
    - Monadic bindings
    - Option merging
    - Option lifting of list and pairs.
*)

include module type of Stdlib.Option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Utility } *)

(** Take the value in the first argument if there is one, otherwise
    take the value in the second argument, otherwise [None] *)
val take_first : 'a option -> 'a option -> 'a option

(** {!take_first} as an operator:

    Behave like boolean or but keep the value of the first option that gave true.
    This is associative but obviously not commutative.*)
val ( ||| ) : 'a option -> 'a option -> 'a option

(** Take the value of the first [Some] in the list. returns [None] if all the option were [None] *)
val take_first_list : 'a option list -> 'a option

(** If both option have values, give [Some] of the pair, otherwise [None] *)
val take_all : 'a option -> 'b option -> ('a * 'b) option

(** Take_all as an operator:

    Behave like boolean and but keep all the value of the options
    This is not associative because at type level (a * b) * c is not a * (b * c).
    Using monadic bindings is recommended for more that 2 operands.*)
val ( &&& ) : 'a option -> 'b option -> ('a * 'b) option

(** Expect the option to contain a value and fails ([Failure]) otherwise.
    The format string specify the content of the failure *)
val value_fail : 'a option -> ('b, unit, string, 'a) format4 -> 'b

(** Like Stdlib.Option.value but the default is a called function,
    that can throw instead of giving a value *)
val value_fun : 'a option -> default:(unit -> 'a) -> 'a

(** Create an option from a bool, with the some value *)
val of_bool : some:'a -> bool -> 'a option

(** Create an option from a bool, with the some value as computed by the some function *)
val of_bool_fun : some:(unit -> 'a) -> bool -> 'a option

(** [for_all p o = fold ~none:true ~some:p]*)
val for_all : ('a -> bool) -> 'a option -> bool

(** [exists p o = fold ~none:false ~some:p]*)
val exists : ('a -> bool) -> 'a option -> bool

(** Return the second argument if the first is [true], otherwise [None] *)
val guard : bool -> 'a -> 'a option

(** Return the second argument if the first is [false], otherwise [None] *)
val guardn : bool -> 'a -> 'a option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Monadic bindings } *)

(** Applicative let.

    [let+ x = mx in e] is [Option.map (fun x -> e) mx] *)
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option

(** Applicative and.

    [let+ x = mx and+ y = my in e] give [Some e] if both [mx] and [my] were [Some]s. *)
val ( and+ ) : 'a option -> 'b option -> ('a * 'b) option

(** Iter applicative let.

    [let+! x = mx in e] runs [e] if [mx] contained a value i.e [Option.iter (fun x -> e) mx] *)
val ( let+! ) : 'a option -> ('a -> unit) -> unit

(** Monadic let: [let* x = mx in e] is [Option.bind mx (fun x -> e)] *)
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

(** Monadic and: [let* x = mx and* y = my in e] is [let* x = mx in let* y = my in e] *)
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Lists } *)

(** Commute the list and the option. If the list contains one [None] then the result is [None].
    If you want to keep all the [Some] value, use {!List.filter_map}.

    This can be condidered as a list-wide {!take_all}. A list-wide {!take_first} would be
    {!List.find_map} *)
val lift : 'a option list -> 'a list option

(** The same as a List.map and then a {!lift} *)
val map_lift : ('a -> 'b option) -> 'a list -> 'b list option

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pairs } *)

(** Lift a pair of options to an option of pair. It is the same as {!take_all}. *)
val lift_pair : 'a option * 'b option -> ('a * 'b) option

(** Unlift an option of pair to a pair of options *)
val unlift_pair : ('a * 'b) option -> 'a option * 'b option
