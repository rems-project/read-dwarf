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

(** The module provide a type to represent a symbol table.

     The interesting operations provided are fetching symbol by name
     and knowing which symbol owns a specific address

    {!of_position_string} provides a convenient way of describing a position
    in the ELF file from a human text input like the CLI.*)

type sym = Symbol.t

type linksem_sym = Symbol.linksem_t

(** The type of a symbol with offset *)
type sym_offset = sym * int

type linksem_t = LinksemRelocatable.global_symbol_init_info

(** The type of a symbol table. *)
type t

(** The empty symbol table *)
val empty : t

(** Return a new table with the symbol added
    If there already exists a symbol covering the same area, merge them with
    {!Symbol.other_names} *)
val add : t -> sym -> t

(** Get a symbol by name. Raise [Not_found] if no name matches *)
val of_name : t -> string -> sym

(** Get a symbol by name, None if no name matches *)
val of_name_opt : t -> string -> sym option

(** Get the symbol owning that address. Not_found is raised if no symbol own that address.data
      See {!of_addr_opt} *)
val of_addr : t -> Address.t -> sym

(** Get the symbol owning that address. None if no symbol own that address. See {!of_addr} *)
val of_addr_opt : t -> Address.t -> sym option

(** Get a symbol with the offset that correspond to that address *)
val of_addr_with_offset : t -> Address.t -> sym_offset

(** Get a symbol with the offset that correspond to that address *)
val of_addr_with_offset_opt : t -> Address.t -> sym_offset option

(** Get back the raw address from a symbol+offset value *)
val to_addr_offset : sym_offset -> Address.t

(** Transform a symbol + offset into the corresponding string *)
val string_of_sym_offset : sym_offset -> string

(** Transform a symbol + offset string into the actual symbol and the integer offset *)
val sym_offset_of_string : t -> string -> sym_offset

(** Convert a position string to a symbol + offset

   A position string is a string describing a position in an ELF file.
   Two format are accepted for now:
   - A raw address of the form "0x40cafe"
   - A symbol name with optional offset like "sym" or "sym+4" or "sym+0x4"
*)
val of_position_string : t -> string -> sym_offset

(** Extract the symbol from the linksem symbol representation.

    Need the segments for filling the missing symbol data *)
val of_linksem : linksem_t -> t

(** Pretty print the table as a raw ocaml value *)
val pp_raw : t -> Pp.document

(** Iterate through all the symbols in the table. *)
val iter : t -> (sym -> unit) -> unit

(** Fold over all the symbols in the table. *)
val fold : t -> 'a -> (sym -> 'a -> 'a) -> 'a
