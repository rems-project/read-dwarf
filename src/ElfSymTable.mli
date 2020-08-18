(** The module provide a type to represent a symbol table.

     The interesting operations provided are fetching symbol by name
     and knowing which symbol owns a specific address

    {!of_position_string} provides a convenient way of describing a position
    in the ELF file from a human text input like the CLI.*)

type sym = ElfSymbol.t

type linksem_sym = ElfSymbol.linksem_t

(** The type of a symbol with offset *)
type sym_offset = sym * int

type linksem_t = Elf_file.global_symbol_init_info

(** The type of a symbol table. *)
type t

(** The empty symbol table *)
val empty : t

(** Return a new table with the symbol added
    If there already exists a symbol covering the same area, merge them with
    {!ElfSymbol.other_names} *)
val add : t -> sym -> t

(** Get a symbol by name. Raise [Not_found] if no name matches *)
val of_name : t -> string -> sym

(** Get a symbol by name, None if no name matches *)
val of_name_opt : t -> string -> sym option

(** Get the symbol owning that address. Not_found is raised if no symbol own that address.data
      See {!of_addr_opt} *)
val of_addr : t -> int -> sym

(** Get the symbol owning that address. None if no symbol own that address. See {!of_addr} *)
val of_addr_opt : t -> int -> sym option

(** Get a symbol with the offset that correspond to that address *)
val of_addr_with_offset : t -> int -> sym_offset

(** Get a symbol with the offset that correspond to that address *)
val of_addr_with_offset_opt : t -> int -> sym_offset option

(** Get back the raw address from a symbol+offset value *)
val to_addr_offset : sym_offset -> int

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
val of_linksem : Segment.t list -> linksem_t -> t

(** Pretty print the table as a raw ocaml value *)
val pp_raw : t -> PP.document
