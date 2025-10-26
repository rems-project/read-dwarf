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

(** This module handle the register abstraction.

    A register is defined by a {!Path} and a type {!ty}. The path is a
    representation of dot separated list of identifiers.

    Registers are not part of the {!Arch} module because they are discovered
    dynamically. This module keeps a global index of all register of the current
    architecture (in a {!Utils.IdMap}). This map also fix the types of registers.

    This allow to represent registers as integer everywhere.

    The module also provides {!Map} and {!PMap} a respectively full and partial
    maps over registers. They need special support (especially the full map)
    because new registers may be added at any time after the creation of the map.

    TODO: Support sail vectors (A path will be of type (string + int) list) *)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Paths }

    Representation of register path. The string reprensentation is with dots.
    A name may not contain dots, but this is not checked.
*)

module Path : sig
  type t = string list

  (** Print the path as dotted list of identifier: [\["A";"B";"C"\] -> "A.B.C"] *)
  val to_string : t -> string

  (** Parse the path as dotted list of identifier:  ["A.B.C" -> \["A";"B";"C"\]] *)
  val of_string : string -> t

  (** Pretty print the path *)
  val pp : t -> Pp.document
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Registers }

    Global register properties and accessors
*)

(** The type representing a register. The module invariant is that this type
    always contains a value bound in the global index and so this is always a
    valid register id. *)
type t = private int

(** The type of a register. This is isomorphic to {!Isla.ty}.
    Use {!IslaConv.ty} to convert *)
type ty = Ast.no Ast.ty

(** Convert an integer into the corresponding register. *)
val of_int : int -> t option

(** Check if register is declared with that path *)
val mem_path : Path.t -> bool

(** Check if a register is declared with that name.
    Same as [Path.of_string |> mem_path] *)
val mem_string : string -> bool

(** Give the register corresponding to that path *)
val of_path : Path.t -> t

(** Give the path of a register *)
val to_path : t -> Path.t

(** Give the register corresponding to a register name *)
val of_string : string -> t

(** Give the name of a register *)
val to_string : t -> string

(** Give the current number of registers *)
val num : unit -> int

(** Give the type of a register *)
val reg_type : t -> ty

(** Give the type of register path.
    Throw [Not_found], it that path is not a declared register *)
val path_type : Path.t -> ty

(** Add a new register to the global {!index}. Return it's representation *)
val add : Path.t -> ty -> t

(** Ensure that a register with that path exists with that type,
    by adding it or checking it already exists with that type.
    Return the corresponding register *)
val ensure_add : Path.t -> ty -> t

(** Same as {!add} but returns unit *)
val adds : Path.t -> ty -> unit

(** Ensure that a register with that path exists with that type,
    by adding it or checking it already exists with that type. *)
val ensure_adds : Path.t -> ty -> unit

(** Run a function over all registers *)
val iter : (Path.t -> t -> ty -> unit) -> unit

(** Returns a sequence of all registers *)
val seq_all : unit -> t Seq.t

(** Equality predicate *)
val ( = ) : t -> t -> bool

(** Inequality predicate *)
val ( <> ) : t -> t -> bool

(** Compare according to an implementation-defined total order. *)
val compare : t -> t -> int

(** Pretty prints a register (Just use {!to_string}) *)
val pp : t -> Pp.document

(** Pretty prints a register type *)
val pp_ty : ty -> Pp.document

(** Prints the register index (mainly for debugging I suppose) *)
val pp_index : unit -> Pp.document

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Register map }

    To achieve a partial map on register, one may just used a plain [Hashtbl].
    However as register is a finite type one may want to have a map where all the
    register are bound and thus access to a bound value cannot fail. This is
    complicated by the fact that new registers can be added after the creation of
    the map. To handle all those subtleties, there is the {!Utils.Map} module.*)

(** This module provide a full map over register in the same way than {!Utils.FullVec}
    provide a map of integers. It still need a generator to generate the value
    bound to not-yet-added registers.

    Because the domain of registers is finite, some extra function are available
    like {!iter} and {!iteri} that are not possible in {!Utils.FullVec}.

    If a register is added with {!add}, it is automatically and implicitly added
    to the {!Utils.Map} and the generator must accept this new value. The generator
    will never be called on invalid register values (i.e. when the generator is
    called on a register, the former can get the latter's type and name with
    {!reg_type} and {!to_string}) *)
module Map : sig
  type reg = t

  (** The type of the complete map *)
  type 'a t

  (** Initialize the map with a generator *)
  val init : (reg -> 'a) -> 'a t

  (** Clear the map and restart with this generator *)
  val reinit : 'a t -> (reg -> 'a) -> unit

  (** Make a copy of the map *)
  val copy : 'a t -> 'a t

  (** Set the value of a register *)
  val set : 'a t -> reg -> 'a -> unit

  (** Get the value of a register *)
  val get : 'a t -> reg -> 'a

  (** Map the function all the registers (including future, not yet added ones) *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Same as {!map} but with the index *)
  val mapi : (reg -> 'a -> 'b) -> 'a t -> 'b t

  (** Map the function on all the register by mutation (including future ones) *)
  val map_mut : ('a -> 'a) -> 'a t -> unit

  (** Map the function on all current register. Future registers are unchanged *)
  val map_mut_current : ('a -> 'a) -> 'a t -> unit

  (** Iterate over all the value of all currently present registers *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** Same as {!iter} but also with the register index *)
  val iteri : (reg -> 'a -> unit) -> 'a t -> unit

  (** Give all the registers bindings *)
  val bindings : 'a t -> (reg * 'a) list

  (** Contrary to {!Utils.FullVector.pp}, this one will print the binding of all registers,
      and may call the generator to do that *)
  val pp : ('a -> Pp.document) -> 'a t -> Pp.document
end
