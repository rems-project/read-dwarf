(** This module provide code to manipulate basic block and run them *)

open Fun

type trc = State.trc

type state = State.t

(** Type of a basic block.

    The main part is the traces of all the non-branching instruction

    TODO: The branch part are all the possible trace of the last, branching instruction.
    If the list is empty, there is no such last instruction
*)
type t = { main : trc array (* TODO: branch : trc list *) }

(** Take a binary block and call isla on all the instruction to get traces
    TODO Support variable length instructions
*)
let from_binary (code : BytesSeq.t) : t =
  let num = BytesSeq.length code / 4 in
  (* TODO fix fixed size instructions *)
  if BytesSeq.length code != num * 4 then
    failwith "BB.from_binary: The specified range cuts an instruction";
  let process (code : BytesSeq.t) : trc =
    let get_normal : Isla.rtrc list -> trc = function
      | [] -> failwith "BB.from_binary: no normal path"
      | [trc] -> trc |> IslaManip.isla_trace_conv_svar
      | _ -> failwith "BB.from_binary: Multiple path i.e. branching instruction"
    in
    code |> IslaCache.get_traces |> get_normal
  in
  let main = code |> BytesSeq.to_list32bs |> List.map process |> Array.of_list in
  { main }

(** Add to the register map all the register appearing in the basic block *)
let type_regs (bb : t) : unit = Array.iter (IslaType.type_trc %> ignore) bb.main

(** Run a linear basic block on a state by mutation *)
let run_mut state (bb : t) : unit = Array.iter (IslaTrace.run_trc_mut state) bb.main

(** Run a linear basic block on a trace and return a new state *)
let run start (bb : t) : state =
  let state = State.copy start in
  run_mut state bb;
  State.lock state;
  state

let pp (bb : t) = PP.(array State.pp_trc bb.main)
