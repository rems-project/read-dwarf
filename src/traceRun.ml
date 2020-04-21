open Logs.Logger (struct
  let str = "TraceRun"
end)

type context = {
  reg_writes : (Reg.path * State.tval) Vector.t;
  mem_reads : State.tval HashVector.t;
  state : State.t;
}

let make_context state =
  let reg_writes = Vector.empty () in
  let mem_reads = HashVector.empty () in
  { state; reg_writes; mem_reads }

let expand_var ~(ctxt : context) (v : Trace.Var.t) (a : Ast.lrng) =
  match v with
  | Register reg -> (Reg.Map.get ctxt.state.regs reg).exp
  | Read i -> (HashVector.get ctxt.mem_reads i).exp

let expand ~(ctxt : context) (exp : Trace.exp) : State.exp =
  AstManip.exp_var_subst (expand_var ~ctxt) (exp |> AstManip.allow_ptr)

(* This is where type-inference will happen (soon) *)
let event_mut ~(ctxt : context) (event : Trace.event) =
  debug "Running: %t" (PP.top Trace.pp_event event);
  match event with
  | WriteReg { reg; value } as e ->
      Vector.add_one ctxt.reg_writes (reg, State.make_tval (expand ~ctxt value))
  | ReadMem { addr; value; size } ->
      let mb : State.Mem.block = { addr = expand ~ctxt addr; size } in
      let tval = State.read mb ctxt.state in
      HashVector.set ctxt.mem_reads value tval
  | WriteMem { addr; value; size } ->
      let mb : State.Mem.block = { addr = expand ~ctxt addr; size } in
      let value = expand ~ctxt value in
      State.write mb value ctxt.state
  | Assert exp -> State.push_assert ctxt.state (expand ~ctxt exp)

let trace_mut (state : State.t) (events : Trace.t) : unit =
  let ctxt = make_context state in
  List.iter (event_mut ~ctxt) events;
  Vector.iter (fun (reg, tval) -> Reg.Map.set state.regs reg tval) ctxt.reg_writes

let trace (start : State.t) (events : Trace.t) : State.t =
  let state = State.copy start in
  trace_mut state events;
  State.lock state;
  state
