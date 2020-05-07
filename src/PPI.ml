(** This module provide intermediate pretty printing,
    i.e it add all custom data structure pretty printing with simple names.

    The other pretty printing convenience module are:
    - PP : Pretty Printing : StdLib and external pretty printing
    - PPA : Pretty Printing all : everything
*)

include PP

let vector conv v = Vec.pp conv v

let hvector conv hv = HashVector.pp conv hv

let reg = Reg.pp

let rmap conv map = Reg.Map.pp conv map

let rstruct = Reg.pp_rstruct

let sid = State.Id.pp

let byteseq = BytesSeq.pp

let byteseqc = BytesSeq.ppc

let byteseq32le = BytesSeq.pp32le
