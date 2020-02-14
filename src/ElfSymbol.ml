(** This module represent an Elf symbol *)

(** The type of the ELF symbol *)
type typ = NOTYPE | OBJECT | FUNC | SECTION | FILE | UNKNOWN

(** The ELF symbol. This type guarantee the data exists contrary to linksem symbols (it may be all zeros though) *)
type t = { name : string; typ : typ; addr : int; size : int; data : BytesSeq.t }

type sym = t

(** The type of a symbol with offset *)
type sym_offset = sym * int

(** Check if an address is in a symbol *)
let is_in t addr = t.addr <= addr && addr < t.addr + t.size

(** Convert the integer type into typ *)
let typ_of_linksem ltyp =
  match Z.to_int ltyp with
  | 0 -> NOTYPE
  | 1 -> OBJECT
  | 2 -> FUNC
  | 3 -> SECTION
  | 4 -> FILE
  | _ -> UNKNOWN

(** Get the data from the linksem symbol type *)
let linksem_data (name, (typ, size, addr, data, _)) = data

(** Create a symbol from a linksem symbol with the data specified *)
let of_linksem_with_data data (name, (typ, size, addr, _, _)) =
  { name; typ = typ_of_linksem typ; size = Z.to_int size; addr = Z.to_int addr; data }

(** Create a symbol from a linksem symbol *)
let of_linksem lsym =
  match linksem_data lsym with
  | Some data -> of_linksem_with_data data lsym
  | _ -> of_linksem_with_data BytesSeq.empty lsym

(** Tell if a symbol is interesting for readDwarf purposes *)
let is_interesting = function OBJECT | FUNC -> true | _ -> false

(** Pretty prints a symbol type *)
let pp_typ typ =
  PP.string
  @@
  match typ with
  | NOTYPE -> "NOTYPE"
  | FUNC -> "FUNC"
  | OBJECT -> "OBJECT"
  | SECTION -> "SECTION"
  | FILE -> "FILE"
  | UNKNOWN -> "UNKNOWN"

(** Raw pretty printing of a symbol *)
let pp_raw sym =
  PP.(
    !^"sym"
    ^^ OCaml.record "sym"
         [
           ("name", !^(sym.name));
           ("typ", pp_typ sym.typ);
           ("addr", PP.ptr sym.addr);
           ("size", PP.int sym.size);
           ("data", BytesSeq.pp32le sym.data);
         ])

(** Represent a symbol table
    The interesting operation provided are fetching symbol by name
    and knowing which symbol own a specific address
*)
module Table = struct
  module IMap = Map.Make (Int)
  module SMap = Map.Make (String)

  type t = { by_name : sym SMap.t; by_addr : sym IMap.t }

  (** The empty symbol table *)
  let empty = { by_name = SMap.empty; by_addr = IMap.empty }

  (** Return a new table with the symbol added *)
  let add t sym =
    { by_name = SMap.add sym.name sym t.by_name; by_addr = IMap.add sym.addr sym t.by_addr }

  (** Get a symbol by name *)
  let of_name t name = SMap.find name t.by_name

  (** Get the symbol owning that address. Not_found is raised if no symbol own that address *)
  let of_addr t addr =
    let candidate = IMap.find_last (fun a -> a <= addr) t.by_addr |> snd in
    if is_in candidate addr then candidate else raise Not_found

  (** Get the symbol owning that address. None if no symbol own that address *)
  let of_addr_opt t addr =
    let candidate = IMap.find_last (fun a -> a <= addr) t.by_addr |> snd in
    if is_in candidate addr then Some candidate else None

  (** Get a symbol with the offset that correspond to that address *)
  let of_addr_with_offset t addr =
    let sym = of_addr t addr in
    (sym, addr - sym.addr)

  (** Transform a symbol + offset into the corresponding string *)
  let string_of_sym_offset (sym, off) = sym.name ^ "+" ^ string_of_int off

  (** Transform a symbol + offset string into the actual symbol and the integer offset *)
  let sym_offset_of_string t s =
    match String.split_on_char '+' s with
    | [ssym] -> (of_name t (String.trim ssym), 0)
    | [ssym; soff] -> (of_name t (String.trim ssym), soff |> String.trim |> int_of_string)
    | _ -> failwith "sym_offset_to_string: wrong format"

  (** Pretty print the table as a raw ocaml value *)
  let pp_raw st = IMap.bindings st.by_addr |> List.map (Pair.map PP.ptr pp_raw) |> PP.mapping
end
