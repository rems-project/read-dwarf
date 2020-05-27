(** This module provide the type for a context to run a trace

    Any information that should be required to run a trace but is not part of the state itself
    should be added here
*)

(** The context to run a trace *)
type t = {
  reg_writes : (Reg.t * State.tval) Vec.t;  (** Stores the delayed register writes *)
  mem_reads : State.tval HashVector.t;  (** Stores the result of register reads *)
  state : State.t;
  dwarf : Dw.t option;
}

(** Build a {!context} from a state *)
let make_context ?dwarf state =
  let reg_writes = Vec.empty () in
  let mem_reads = HashVector.empty () in
  { state; reg_writes; mem_reads; dwarf }

(** Expand a Trace variable to a State expression, using the context *)
let expand_var ~(ctxt : t) (v : Trace.Var.t) (a : Ast.lrng) : State.exp =
  match v with
  | Register reg -> State.get_reg ctxt.state reg |> State.get_exp
  | Read i -> (HashVector.get ctxt.mem_reads i).exp

(** Tell if typing should enabled with this context *)
let typing_enabled ~(ctxt : t) = ctxt.dwarf <> None
