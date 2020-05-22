(** This module provides a representation of a complex block of code.

    The block must stay inside a single symbol and the end of the block is decided by an
    arbitrary predicate on the pc

*)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Fun

(** [endpred pc_exp] gives when to stop *)
type t = {
  sym : Elf.Sym.t;
  start : int;
  endpred : State.exp -> bool;
  traces : Trace.t list array;
}

(* TODO support variable length instruction *)

(** Build a complex block starting from [start] in [sym] and ending when [endpred] says so.
    [endpred] is a predicate on the symbolic PC expression *)
let make ~(sym : Elf.Sym.t) ~start ~endpred =
  (* TODO fix fixed size instructions *)
  let process index (code : BytesSeq.t) : Trace.t list =
    try
      code |> IslaCache.get_traces |> List.map (tee (IslaType.type_trc %> ignore) %> Trace.of_isla)
    with Trace.OfIslaError ->
      err "Could not convert isla trace of instruction at 0x%x to Trace.t" (sym.addr + (4 * index));
      Raise.again Trace.OfIslaError
  in

  let traces = sym.data |> BytesSeq.to_list32bs |> Array.of_list_mapi process in
  { sym; start; endpred; traces }

(** Simplify the traces in the block *)
let simplify_mut (b : t) = Array.map_mut (List.map Trace.simplify) b.traces

(** Run the block an return a state tree indexed by the addresses of the branches *)
let run (b : t) ?dwarf (start : State.t) : int StateTree.t =
  assert (State.is_locked start);
  let rec run_from state =
    let pc_exp = State.get_reg state [Arch.pc ()] |> State.get_exp in
    if b.endpred pc_exp then begin
      info "Stopped at pc %t" (PP.top State.pp_exp pc_exp);
      State.map_mut_exp Z3.simplify state;
      State.lock state;
      StateTree.{ state; data = -1; rest = [] }
    end
    else begin
      info "Running pc %t" (PP.top State.pp_exp pc_exp);
      let pc = pc_exp |> Ast.expect_bits |> BitVec.to_int in
      if not @@ Elf.Sym.is_in b.sym pc then Raise.fail "out of function jump";
      let traces = b.traces.((pc - b.sym.addr) / 4) in
      match traces with
      | [] -> Raise.fail "reach a exceptional instruction"
      | [trc] ->
          TraceRun.trace_pc_mut ?dwarf ~next:4 state trc;
          run_from state
      | trcs ->
          State.map_mut_exp Z3.simplify state;
          State.lock state;
          let rest =
            List.map
              (fun trc ->
                let nstate = State.copy state in
                TraceRun.trace_pc_mut ?dwarf ~next:4 nstate trc;
                run_from nstate)
              trcs
          in
          StateTree.{ state; data = pc; rest }
    end
  in
  let state = State.copy start in
  State.set_reg state [Arch.pc ()]
    (State.make_tval (Ast.Op.bits_int ~size:64 (b.sym.addr + b.start)));
  let rest = [run_from state] in
  (* My attempt to write "start" in hexadecimal leet speak: *)
  StateTree.{ state = start; data = 0x57a27; rest }

let pp (b : t) =
  let open PP in
  let line = dprintf "Block in %s at %d. Intructions of symbol:" b.sym.name b.start in
  let instructions =
    concat_array_mapi
      (fun i trcs ->
        prefix 2 1
          (dprintf "Intruction 0x%x:" (i * 4))
          (separate_mapi hardline
             (fun i trc -> prefix 2 1 (dprintf "Trace %d:" i) (Trace.pp trc))
             trcs)
        ^^ hardline)
      b.traces
  in
  line ^^ hardline ^^ instructions
