(* The documentation is in the mli file *)

type typ = NOTYPE | OBJECT | FUNC | SECTION | FILE | UNKNOWN

type linksem_typ = Z.t

type t = {
  name : string;
  other_names : string list;
  typ : typ;
  addr : int;
  size : int;
  writable : bool;
  data : BytesSeq.t;
}

type linksem_t = string * (Z.t * Z.t * Z.t * BytesSeq.t option * Z.t)

let push_name s t = { t with other_names = s :: t.other_names }

let is_in t addr = t.addr <= addr && addr < t.addr + t.size

let len t = t.size

let typ_of_linksem ltyp =
  match Z.to_int ltyp with
  | 0 -> NOTYPE
  | 1 -> OBJECT
  | 2 -> FUNC
  | 3 -> SECTION
  | 4 -> FILE
  | _ -> UNKNOWN

let linksem_typ (name, (typ, size, addr, data, _)) = typ

(** [LoadingError(name,addr)] means that symbol [name] at [addr] could not be loaded *)
exception LoadingError of string * int

let _ =
  Printexc.register_printer (function
    | LoadingError (name, addr) ->
        Some (Printf.sprintf "Symbol %s at 0x%x could not be loaded" name addr)
    | _ -> None)

let of_linksem segs (name, (typ, size, addr, data, _)) =
  let typ = typ_of_linksem typ in
  let size = Z.to_int size in
  let addr = Z.to_int addr in
  let segment =
    Opt.value_fail (Segment.get_containing segs addr) "No segment contains symbol %s" name
  in
  let writable = segment.write in
  let data =
    data
    |> Opt.value_fun ~default:(fun () -> Segment.get_addr (BytesSeq.sub_getter size) segment addr)
  in
  { name; other_names = []; typ; size; addr; data; writable }

let is_interesting = function OBJECT | FUNC -> true | _ -> false

let is_interesting_linksem lsym = lsym |> linksem_typ |> typ_of_linksem |> is_interesting

let sub sym off len = BytesSeq.sub sym.data off len

let compare s1 s2 = compare s1.addr s2.addr

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

let pp_raw sym =
  PP.(
    !^"sym"
    ^^ OCaml.record "sym"
         [
           ("name", !^(sym.name));
           ("other names", separate nbspace (List.map string sym.other_names));
           ("typ", pp_typ sym.typ);
           ("addr", ptr sym.addr);
           ("size", ptr sym.size);
           ("writable", bool sym.writable);
           ("data", BytesSeq.pp32le sym.data);
         ])
