(*****************************************************************************)
(**       look up address in DWARF frame info                                *)

(*****************************************************************************)

open AnalyseUtils
open AnalyseElfTypes
open AnalyseControlFlowTypes

let aof ((a : natural), (_cfa : string), (_regs : (string * string) list)) = a

let rec f (aof : 'b -> natural) (a : natural) (last : 'b option) (bs : 'b list) : 'b option =
  match (last, bs) with
  | (None, []) -> None
  | (Some b', []) -> if Nat_big_num.greater_equal a (aof b') then Some b' else None
  | (None, b'' :: bs') -> f aof a (Some b'') bs'
  | (Some b', b'' :: bs') ->
      if Nat_big_num.less a (aof b') then None
      else if Nat_big_num.greater_equal a (aof b') && Nat_big_num.less a (aof b'') then Some b'
      else f aof a (Some b'') bs'

let mk_frame_info test instructions :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array =
  Array.map (function i -> f aof i.i_addr None test.dwarf_semi_pp_frame_info) instructions

let pp_frame_info _m frame_info k : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match frame_info.(k) with
  | None -> "<no frame info for this address>\n"
  | Some ((a : natural), (cfa : string), (regs : (string * string) list)) ->
      pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " "
      ^ String.concat " " (List.map (function (rname, rinfo) -> rname ^ ":" ^ rinfo) regs)
      ^ "\n"
