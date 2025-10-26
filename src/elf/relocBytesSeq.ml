type t = {
  data: BytesSeq.t;
  relocations: Relocations.t
}

type linksem_t = BytesSeq.t * LinksemRelocatable.sym_data

let sub seq off len = {
  data = BytesSeq.sub seq.data off len;
  relocations = Relocations.sub seq.relocations off len;
}

let of_linksem (data, rel) = { data; relocations=Relocations.of_linksem rel }

let pp seq = Pp.(pair (BytesSeq.ppby ~by:4) Relocations.pp (seq.data, seq.relocations))

let of_bytes_seq data = { data; relocations=Relocations.empty }

(* Special case where only a single relocation is allowed *)
(* used by the executor *)
type opcode = BytesSeq.t * Relocations.rel

let as_opcode seq =
  (seq.data, Relocations.IMap.find_opt 0 seq.relocations)
  
let length seq = BytesSeq.length seq.data

let expect_bs_no_relocations {data; relocations} =
  if Relocations.IMap.is_empty relocations then
    data
  else
    failwith "Byte sequence has relocations"
