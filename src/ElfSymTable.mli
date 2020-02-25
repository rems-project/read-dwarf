(** Represent a symbol table.

     The interesting operation provided are fetching symbol by name
     and knowing which symbol own a specific address
 *)
type sym = ElfSymbol.t

type linksem_sym = ElfSymbol.linksem_t

(** The type of a symbol with offset *)
type sym_offset = sym * int

type linksem_t = Elf_file.global_symbol_init_info

type t

(** The empty symbol table *)
val empty : t

(** Return a new table with the symbol added *)
val add : t -> sym -> t

(** Get a symbol by name *)
val of_name : t -> string -> sym

(** Get the symbol owning that address. Not_found is raised if no symbol own that address.data
      See {!of_addr_opt} *)
val of_addr : t -> int -> sym

(** Get the symbol owning that address. None if no symbol own that address. See {!of_addr} *)
val of_addr_opt : t -> int -> sym option

(** Get a symbol with the offset that correspond to that address *)
val of_addr_with_offset : t -> int -> sym_offset

(** Transform a symbol + offset into the corresponding string *)
val string_of_sym_offset : sym_offset -> string

(** Transform a symbol + offset string into the actual symbol and the integer offset *)
val sym_offset_of_string : t -> string -> sym_offset

(** Extract the symbol from the linksem symbol representation.

    Need the segments for filling the missing symbol data *)
val of_linksem : Segment.t list -> linksem_t -> t

(** Pretty print the table as a raw ocaml value *)
val pp_raw : t -> PP.document
