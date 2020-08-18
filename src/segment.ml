(** The goal of this module is to represent a segment as loaded in memory.
    In particular, all information about file layout is intentionally lost
    I use basic ints for speed. It it fails for some reason, I'll move to int64s.

    This is basically a {!BytesSeq} with metadata.
*)

(** The type of a segment *)
type t = {
  data : BytesSeq.t;
  addr : int;  (** The actual start address of the BytesSeq *)
  size : int;  (** redundant with {!BytesSeq.length} data *)
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
    data = BytesSeq.of_bytes bytes;
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
