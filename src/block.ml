(** This module provides a representation of a complex block of code.

    The block must stay inside a single symbol and the end of the block is decided by an
    arbitrary predicate on the pc

*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** [endpred pc_exp] gives when to stop *)
type t = { runner : Runner.t; start : int; endpred : State.exp -> string option }

(* TODO support variable length instruction *)

(** Build a complex block starting from [start] in [sym] and ending when [endpred] says so.
    [endpred] is a predicate on the symbolic PC expression *)
let make ~runner ~start ~endpred = { runner; start; endpred }

type label = Start | End of string | BranchAt of int | NormalAt of int

let label_to_string = function
  | Start -> "Start"
  | End s -> Printf.sprintf "End (%s)" s
  | BranchAt pc -> Printf.sprintf "Branch at 0x%x" pc
  | NormalAt pc -> Printf.sprintf "Normal at 0x%x" pc

let pp_label label = label |> label_to_string |> PP.string

(** Run the block an return a state tree indexed by the addresses of the branches

    When [every_instruction] is true, It will make a snapshot of
*)
let run ?(every_instruction = false) (b : t) (start : State.t) : label StateTree.t =
  let pcreg = Arch.pc () in
  assert (State.is_locked start);
  let rec run_from state =
    let pc_exp = State.get_reg state pcreg |> State.get_exp in
    match b.endpred pc_exp with
    | Some endmsg ->
        info "Stopped at pc %t because %s" (PP.top State.pp_exp pc_exp) endmsg;
        State.map_mut_exp Z3.simplify state;
        State.lock state;
        StateTree.{ state; data = End endmsg; rest = [] }
    | None -> (
        info "Running pc %t" (PP.top State.pp_exp pc_exp);
        let prelock state = State.map_mut_exp Z3.simplify state in
        if every_instruction then begin
          prelock state;
          State.lock state
        end;
        let states = Runner.run ~prelock b.runner state in
        match states with
        | [] -> Raise.fail "Reached a exceptional instruction"
        | [state] when not every_instruction -> run_from state
        | [state] when every_instruction ->
            let rest = [run_from state] in
            { state; data = NormalAt (pc_exp |> Ast.expect_bits |> BitVec.to_int); rest }
        | states ->
            let rest = List.map run_from states in
            StateTree.
              { state; data = BranchAt (pc_exp |> Ast.expect_bits |> BitVec.to_int); rest }
      )
  in
  let state = State.copy start in
  State.set_pc ~pc:pcreg state b.start;
  let rest = [run_from state] in
  StateTree.{ state = start; data = Start; rest }

(** Generic end predicate. Will stop if:
    - pc below [min]
    - pc above [max]
    - pc is one of [brks]
    - pc has be seen more than [loop]
*)

let gen_endpred ?min ?max ?loop ?(brks = []) () : State.exp -> string option =
  let endnow fmt = Printf.ksprintf Opt.some fmt in
  let pchtbl = Hashtbl.create 10 in
  let loop_str =
    match loop with
    | Some x when x <= 0 -> Raise.fail "Block.gen_endpred: non-positive loop number"
    | Some 1 -> "once"
    | Some 2 -> "twice"
    | Some n -> Printf.sprintf "%d times" n
    | None -> ""
  in
  function
  | Ast.Bits (bv, _) -> (
      let pc = BitVec.to_int bv in
      debug "enpred: Evaluating PC 0x%x" pc;
      match (min, max, loop) with
      | (Some min, _, _) when pc < min -> endnow "PC 0x%x was below min 0x%x" pc min
      | (_, Some max, _) when pc >= max -> endnow "PC 0x%x was above max 0x%x" pc max
      | _ when List.exists (( = ) pc) brks -> endnow "PC 0x%x hit a breakpoint" pc
      | (_, _, Some loop) ->
          let current_num = Hashtbl.find_opt pchtbl pc |> Opt.value ~default:0 in
          if current_num >= loop then endnow "PC 0x%x had been seen more than %s" pc loop_str
          else begin
            Hashtbl.replace pchtbl pc (current_num + 1);
            None
          end
      | _ -> None
    )
  | exp -> endnow "PC %t is symbolic" PP.(tos State.pp_exp exp)
