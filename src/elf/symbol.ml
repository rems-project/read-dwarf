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

let linksem_typ (_name, (typ, _size, _addr, _data, _)) = typ

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
    Option.value_fail (Segment.get_containing segs addr) "No segment contains symbol %s" name
  in
  let writable = segment.write in
  let data =
    data
    |> Option.value_fun ~default:(fun () ->
           Segment.get_addr (BytesSeq.getbs ~len:size) segment addr)
  in
  { name; other_names = []; typ; size; addr; data; writable }

let is_interesting = function OBJECT | FUNC -> true | _ -> false

let is_interesting_linksem lsym = lsym |> linksem_typ |> typ_of_linksem |> is_interesting

let sub sym off len = BytesSeq.sub sym.data off len

let compare s1 s2 = compare s1.addr s2.addr

let pp_typ typ =
  Pp.string
  @@
  match typ with
  | NOTYPE -> "NOTYPE"
  | FUNC -> "FUNC"
  | OBJECT -> "OBJECT"
  | SECTION -> "SECTION"
  | FILE -> "FILE"
  | UNKNOWN -> "UNKNOWN"

let pp_raw sym =
  Pp.(
    !^"sym"
    ^^ OCaml.record "sym"
         [
           ("name", !^(sym.name));
           ("other names", separate nbspace (List.map string sym.other_names));
           ("typ", pp_typ sym.typ);
           ("addr", ptr sym.addr);
           ("size", ptr sym.size);
           ("writable", bool sym.writable);
           ("data", BytesSeq.ppby ~by:4 sym.data);
         ])
