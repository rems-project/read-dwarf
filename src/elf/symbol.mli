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

(** This module represent an Elf symbol. One important difference with linksem
    symbols is that the symbols of this module always have the corresponding data
    (code or initial value). That's why function like {!of_linksem_with_data}
    exist.

    For now addresses are in ints and assume the top bit is sign extended. It
    may become Int64.t if required *)

(** The type of the ELF symbol *)
type typ = NOTYPE | OBJECT | FUNC | SECTION | FILE | UNKNOWN

type linksem_typ = Z.t

type data = {
  data: BytesSeq.t;
  relocations: Relocations.t
}

(** The ELF symbol. This type guarantee the data exists contrary to linksem symbols
    (it may be all zeros though) *)
type t = {
  name : string;
  other_names : string list;
  typ : typ;
  addr : Address.t;
  (* addr : int; *)
  size : int;
  writable : bool;
  data : data;
}

(** The type of an ELF symbol in linksem. See {!of_linksem}*)
type linksem_t = LinksemRelocatable.symbol

(** Add a name to the other names list *)
val push_name : string -> t -> t

(** Check if an address is in a symbol *)
(* val is_in : t -> int -> bool *)

(** For conformance with the {!Utils.RngMap.LenObject} module type *)
val len : t -> int

(** Convert the integer type into typ *)
val typ_of_linksem : linksem_typ -> typ

(** Get the type from the linksem symbol type *)
val linksem_typ : linksem_t -> linksem_typ

(** [LoadingError(name,addr)] means that symbol [name] at [addr] could not be loaded.*)
exception LoadingError of string * int

(** Convert a symbol from linksem to read-dwarf representation using the segment data

    May raise {!LoadingError} when the symbol has no data and the
    data cannot be found in the segments
*)
val of_linksem : linksem_t -> t

(** Tell if a symbol type is interesting for readDwarf purposes *)
val is_interesting : typ -> bool

(** Tell if a linksem symbol is interesting for readDwarf purposes *)
val is_interesting_linksem : linksem_t -> bool

(** Take the BytesSeq.t corresponding to the offset and length *)
val sub : t -> int -> int -> data

(** Starting address comparison *)
val compare : t -> t -> int

(** Pretty prints a symbol type *)
val pp_typ : typ -> Pp.document

(** Raw pretty printing of a symbol *)
val pp_raw : t -> Pp.document
