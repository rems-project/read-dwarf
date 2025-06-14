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

(** The goal of this module is to represent a segment as loaded in memory.
    In particular, all information about file layout is intentionally lost
    I use basic ints for speed. It it fails for some reason, I'll move to int64s.

    This is basically a {!Utils.BytesSeq} with metadata.
*)

(** The type of a segment *)
type t = {
  data : BytesSeq.t * Relocations.t;
  addr : int;  (** The actual start address of the BytesSeq *)
  size : int;  (** redundant with {!Utils.BytesSeq.length} data *)
  read : bool;
  write : bool;
  execute : bool;
}

(** Loads a {!t} using a linksem interpreted segment. *)
let of_linksem (lseg : Elf_interpreted_segment.elf64_interpreted_segment) : t =
  let size = Z.to_int lseg.elf64_segment_memsz in
  let bytes = Bytes.create size in
  BytesSeq.blit lseg.elf64_segment_body 0 bytes 0 (Z.to_int lseg.elf64_segment_size);
  let (read, write, execute) = lseg.elf64_segment_flags in
  {
    data = BytesSeq.of_bytes bytes, Relocations.IMap.empty;
    addr = Z.to_int lseg.elf64_segment_base;
    size;
    read;
    write;
    execute;
  }

(** Check if an address is inside a segment *)
let is_in seg addr = seg.addr <= addr && addr < seg.addr + seg.size

(** Get a value at an address which is in this segment using the getter provided *)
let get_addr getter seg addr =
  let off = addr - seg.addr in
  getter seg.data off

(** Get a value at an address which is one of the segment of this list.
      It must be entirely in one of the segment *)
let get_addr_list_opt getter segs addr =
  List.fold_left
    (fun res seg -> if is_in seg addr then Some (get_addr getter seg addr) else res)
    None segs

(** Get the segment containing an address, among a list of them or [None] *)
let get_containing segs addr = List.find_opt (Fun.flip is_in addr) segs
