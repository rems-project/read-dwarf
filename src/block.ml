(** This module provides a representation of a complex block of code.

    The block must stay inside a single symbol and the end of the block is decided by an
    arbitrary predicate on the pc

*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** [endpred pc_exp] gives when to stop *)
type t = { runner : Runner.t; start : int; endpred : State.exp -> bool }

(* TODO support variable length instruction *)

(** Build a complex block starting from [start] in [sym] and ending when [endpred] says so.
    [endpred] is a predicate on the symbolic PC expression *)
let make ~runner ~start ~endpred = { runner; start; endpred }

type label = Start | End | BranchAt of int

let label_to_string = function
  | Start -> "Start"
  | End -> "End"
  | BranchAt pc -> Printf.sprintf "Branch at 0x%x" pc

let pp_label label = label |> label_to_string |> PP.string

(** Run the block an return a state tree indexed by the addresses of the branches *)
let run (b : t) (start : State.t) : label StateTree.t =
  let pcreg = Arch.pc () in
  assert (State.is_locked start);
  let rec run_from state =
    let pc_exp = State.get_reg state pcreg |> State.get_exp in
    if b.endpred pc_exp then begin
      info "Stopped at pc %t" (PP.top State.pp_exp pc_exp);
      State.map_mut_exp Z3.simplify state;
      State.lock state;
      StateTree.{ state; data = End; rest = [] }
    end
    else begin
      info "Running pc %t" (PP.top State.pp_exp pc_exp);
      let prelock state = State.map_mut_exp Z3.simplify state in
      let states = Runner.run ~prelock b.runner state in
      match states with
      | [] -> Raise.fail "Reached a exceptional instruction"
      | [state] -> run_from state
      | states ->
          let rest = List.map run_from states in
          StateTree.{ state; data = BranchAt (pc_exp |> Ast.expect_bits |> BitVec.to_int); rest }
    end
  in
  let state = State.copy start in
  State.set_pc ~pc:pcreg state b.start;
  let rest = [run_from state] in
  StateTree.{ state = start; data = Start; rest }
