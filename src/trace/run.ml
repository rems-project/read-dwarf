(** This module is for running trace from {!Trace} like {!IslaRun} runs Isla traces.

    Due to the semantic of a register access being the register at the beginning of the trace,
    all register writes are not done immediately but delayed and stored in the {!context}.

    Typing is enabled if {!Context.typing_enabled} returns true for
    functions that take a context. For other functions, typing is enabled if the
    [dwarf] optional argument is passed
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module Ctxt = Context

type ctxt = Ctxt.t

(** Expand a {!Trace} expression to a {!State} expression, using the context *)
let expand ~(ctxt : ctxt) (exp : Base.exp) : State.exp =
  Ast.Manip.exp_var_subst (Ctxt.expand_var ~ctxt) exp

(** Expand a Trace expression to a typed State expression, using the context.

    If the context enables typing, the expression will actually be typed,
    otherwise the type will be [None] *)
let expand_tval ~(ctxt : ctxt) (exp : Base.exp) : State.tval =
  let sexp = expand ~ctxt exp in
  if Ctxt.typing_enabled ~ctxt then
    let ctyp = Typer.expr ~ctxt exp in
    { ctyp; exp = sexp }
  else { ctyp = None; exp = sexp }

(** Run the event.
    The modified state is the one inside [ctxt]. *)
let event_mut ~(ctxt : ctxt) (event : Base.event) =
  debug "Running: %t with typing %s" (Pp.top Base.pp_event event)
    (if Ctxt.typing_enabled ~ctxt then "on" else "off");
  match event with
  | WriteReg { reg; value } -> Vec.add_one ctxt.reg_writes (reg, expand_tval ~ctxt value)
  | ReadMem { addr; value; size } ->
      let naddr = expand ~ctxt addr in
      let tval =
        match ctxt.dwarf with
        | Some dwarf ->
            let ptrtype = Typer.expr ~ctxt addr in
            Typer.read ~dwarf ctxt.state ?ptrtype ~addr:naddr ~size
        | None -> State.read_noprov ctxt.state ~addr:naddr ~size |> State.Tval.of_exp
      in
      HashVector.set ctxt.mem_reads value tval
  | WriteMem { addr; value; size } -> (
      let naddr = expand ~ctxt addr in
      match ctxt.dwarf with
      | Some dwarf ->
          let ptrtype = Typer.expr ~ctxt addr in
          debug "Typed write mem with ptr:%t" (Pp.top (Pp.opt Ctype.pp) ptrtype);
          let value = expand_tval ~ctxt value in
          Typer.write ~dwarf ctxt.state ?ptrtype ~addr:naddr ~size value
      | None ->
          let value = expand ~ctxt value in
          State.write_noprov ctxt.state ~addr:naddr ~size value
    )
  | Assert exp -> State.push_assert ctxt.state (expand ~ctxt exp)

(** Run a trace on the provided state by mutation. Enable typing if [dwarf] is provided *)
let trace_mut ?dwarf (state : State.t) (events : Base.t) : unit =
  assert (not @@ State.is_locked state);
  info "Running trace with typing %s" (if dwarf <> None then "on" else "off");
  let ctxt = Context.make_context ?dwarf state in
  List.iter (event_mut ~ctxt) events;
  Vec.iter (fun (reg, tval) -> State.Reg.Map.set state.regs reg tval) ctxt.reg_writes

(** Run a trace on the provided state by returning an updated copy.*)
let trace ?dwarf (start : State.t) (events : Base.t) : State.t =
  let state = State.copy start in
  trace_mut ?dwarf state events;
  State.lock state;
  state

(** Run a trace by mutating the provided state including it's PC.
    If the trace modified the PC then nothing is done otherwise [next] is added to it.

    Thus this function automatically handle moving the PC for fall-through instruction
*)
let trace_pc_mut ?dwarf ~(next : int) (state : State.t) (events : Base.t) : unit =
  let pc = Arch.pc () in
  let rec is_touching_pc : Base.t -> bool = function
    | [] -> false
    | WriteReg { reg; _ } :: _ when reg = pc -> true
    | _ :: l -> is_touching_pc l
  in
  trace_mut ?dwarf state events;
  if is_touching_pc events then State.concretize_pc ~pc state else State.bump_pc ~pc state next
