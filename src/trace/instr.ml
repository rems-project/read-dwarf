(** This module provide the representation of an instruction.
    It only contain generic information about the opcode and not specific
    information about a place in the code.*)

module Reg = State.Reg

(** A simple trace with it's metadata *)
type trace_meta = { trace : Base.t; jump : bool; footprint : Reg.t list }

(** A full instruction representation *)
type t = { traces : trace_meta list; length : int;  (** Bytes length *) footprint : Reg.t list }

(** Helper to access the [footprint] field *)
let footprint t = t.footprint

(** Compute the metadata of trace *)
let trace_meta_of_trace trace =
  let pc = Arch.pc () in
  let footprint = ref [] in
  let jump = ref false in
  let process_reg reg = footprint := reg :: !footprint in
  let process_var = function Base.Var.Register reg -> process_reg reg | _ -> () in
  let process_exp : Base.exp -> unit = Ast.Manip.exp_iter_var process_var in
  let process_event : Base.event -> unit = function
    | WriteReg { reg; value } ->
        process_reg reg;
        process_exp value;
        if reg = pc then jump := true
    | ReadMem { addr; _ } -> process_exp addr
    | WriteMem { addr; value; _ } ->
        process_exp addr;
        process_exp value
    | Assert exp -> process_exp exp
  in
  List.iter process_event trace;
  { trace; jump = !jump; footprint = List.sort_uniq compare !footprint }

(** Generate an instruction data from a list of traces *)
let of_traces traces =
  let traces = List.map trace_meta_of_trace traces in
  let length = 4 (* TODO *) in
  let footprint =
    List.fold_left (fun l (tr : trace_meta) -> List.merge_uniq compare l tr.footprint) [] traces
  in
  { traces; length; footprint }

(** Pretty print the representation of an instruction *)
let pp instr =
  let open Pp in
  separate_mapi hardline
    (fun i trc -> prefix 4 1 (dprintf "Trace %d:" i) (Base.pp trc.trace))
    instr.traces
