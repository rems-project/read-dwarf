(** This module encodes the definition and checking of the simulation relation. *)

module Tree = State.Tree
module Tval = State.Tval
module Reg = State.Reg

type tree = Run.Block_lib.label Tree.t

let check_eq (st1 : State.t) (_ : State.t) =
  let _ = Z3.ensure_started_get () in
  State.Reg.Map.iteri (fun _ _ -> (??)) st1.regs

