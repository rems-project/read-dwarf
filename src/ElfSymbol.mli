(** This module represent an Elf symbol. One important difference with linksem symbols is that
    the symbols of this module always have the corresponding data (code or initial value).
    That's why function like {!of_linksem_with_data} exist.

    For now addresses are in ints and assume the top bit is 0.
    It may become Int64.t if required. (or I'll assume sign extension for the last bit)

*)

(** The type of the ELF symbol *)
type typ = NOTYPE | OBJECT | FUNC | SECTION | FILE | UNKNOWN

type linksem_typ = Z.t

(** The ELF symbol. This type guarantee the data exists contrary to linksem symbols
    (it may be all zeros though) *)
type t = { name : string; typ : typ; addr : int; size : int; data : BytesSeq.t }

(** The type of an ELF symbol in linksem. See {!of_linksem}*)
type linksem_t = string * (Z.t * Z.t * Z.t * BytesSeq.t option * Z.t)

(** Check if an address is in a symbol *)
val is_in : t -> int -> bool

(** Convert the integer type into typ *)
val typ_of_linksem : linksem_typ -> typ

(** Get the type from the linksem symbol type *)
val linksem_typ : linksem_t -> linksem_typ

(** [LoadingError(name,addr)] means that symbol [name] at [addr] could not be loaded.
*)
exception LoadingError of string * int

(** Convert a symbol from linksem to read-dwarf representation using the segment data

    May raise {!LoadingError} when the symbol has no data and the
    data cannot be found in the segments
*)
val of_linksem : Segment.t list -> linksem_t -> t

(** Tell if a symbol type is interesting for readDwarf purposes *)
val is_interesting : typ -> bool

(** Tell if a linksem symbol is interesting for readDwarf purposes *)
val is_interesting_linksem : linksem_t -> bool

(** Take the BytesSeq.t corresponding to the offset and length *)
val sub : t -> int -> int -> BytesSeq.t

(** Pretty prints a symbol type *)
val pp_typ : typ -> PP.document

(** Raw pretty printing of a symbol *)
val pp_raw : t -> PP.document
