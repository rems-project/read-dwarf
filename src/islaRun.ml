(** This module provide facility to run {!Isla} trace over state ({!State.t})

    The main functions are {!trc} for pure interface and {!trc_mut} for imperative interface.

    {!RunError} will be thrown when something goes wrong.

    It is for testing purpose only, otherwise use {!TraceRun}. Typing do not work,
    and some other expected behavior may not work either.

*)

open Isla

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Exception that represent an Isla runtime error which should not happen *)
exception RunError of lrng * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | RunError (l, s) -> Some PP.(sprint @@ prefix 2 1 (lrng l ^^ !^": ") (!^"RunError: " ^^ !^s))
    | _ -> None)

(** Throw a run error with the string part as formated by the format string *)
let run_error l fmt = Printf.ksprintf (fun s -> raise (RunError (l, s))) fmt

(** The contex of value that associate isla variable numbers to state expression *)
type value_context = State.exp HashVector.t

(** Get the expression associated to the free variable.
    Throw {!RunError}, if the variable is no bound.
    The {!lrng} is for error reporting. *)
let get_var l vc i =
  match HashVector.get_opt vc i with
  | Some exp -> exp
  | None -> run_error l "v%d is not bound in %t" i (PP.tos (PPI.hvector State.Exp.pp) vc)

(** Convert an {!Isla} expression to an {!Ast} by substituing
    all free variable with the bound expression in the {!value_context}.

    If a variable is not bound, throw {!RunError} *)
let exp_conv_subst (vc : value_context) (exp : Isla.rexp) : State.exp =
  let vconv i l = get_var l vc i in
  IslaConv.exp_var_subst vconv exp

(** Give the {!State.exp} that represents the input {!Isla.valu}.

    A symbolic variable i is represented by the expression bound to it
    in the provided {!value_context}.

    Newly created expression are located with the provided {!lrng}.

    If the value is not convertible to a state expression, throw a {!RunError} *)
let exp_of_valu l vc : Isla.valu -> State.exp = function
  | Val_Symbolic i -> get_var l vc i
  | Val_Bool b -> Bool (b, l)
  | Val_Bits bv -> Bits (BitVec.of_smt bv, l)
  | Val_I (int, size) -> Bits (BitVec.of_int ~size int, l)
  | Val_Enum (n, a) -> Enum ((n, a), l)
  | valu -> run_error l "Can't convert %t to a state expression" (PP.tos pp_valu valu)

(** This function write an expression to symbolic variable.
    The write is ignored if the variable was already set because
    isla guarantee that it would be the same value (Trusting Isla here) *)
let write_to_var l vc var exp = HashVector.set vc var exp

(** This function write an expression to an {!Isla.valu}.

    If the valu is a variable, it is added to the context, otherwise nothing happens. *)
let write_to_valu l vc valu exp =
  match valu with Val_Symbolic i -> write_to_var l vc i exp | _ -> ()

(** Run an event on {!State.t} and a {!value_context} by mutating both *)
let event_mut (vc : value_context) (state : State.t) : Isla.revent -> unit = function
  | Smt (DeclareConst (_, _), _) -> ()
  | Smt (DefineConst (i, e), l) -> (
      debug "Defining v%i with %t" i (PP.top pp_exp e);
      (* If the vc_subst_full fails, that means that a variable was not defined,
           Which means a non-determinism exists in the spec (no uni-valued type supported).
           As we don't support non-determinism, we just also won't define the current variable
           that depend on non determinism. If a non deterministic value was written to
           a register or memory, then the system would actually fail at that point *)
      try write_to_var l vc i (exp_conv_subst vc e) with RunError _ -> ()
    )
  | Smt (Assert e, l) -> State.push_assert state (exp_conv_subst vc e)
  | Smt (DefineEnum e, l) -> ()
  | ReadReg (name, al, valu, l) ->
      debug "Reading Reg %s at %t from %t" name PP.(top pp_accessor_list al) PP.(top pp_valu valu);
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      let e : State.exp = (Reg.Map.get state.regs reg).exp in
      write_to_valu l vc valu e
  | WriteReg (name, al, valu, l) ->
      debug "Writing Reg %s at %t from %t" name PP.(top pp_accessor_list al) PP.(top pp_valu valu);
      let string_path = IslaManip.string_of_accessor_list al in
      let valu = IslaManip.valu_get valu string_path in
      let reg = Reg.of_path (name :: string_path) in
      let exp : State.exp = exp_of_valu l vc valu in
      Reg.Map.set state.regs reg (State.Tval.make exp)
  | ReadMem (result, kind, addr, size, l) ->
      debug "Reading Mem";
      (* TODO stop ignoring kind *)
      let addr = exp_of_valu l vc addr |> Pointer.to_ptr_size in
      let size = State.Mem.Size.of_bytes size in
      write_to_valu l vc result (State.read_noprov state ~addr ~size)
  | WriteMem (success, kind, addr, data, size, l) ->
      debug "Writing Mem";
      (* TODO stop ignoring kind *)
      let addr = exp_of_valu l vc addr |> Pointer.to_ptr_size in
      let size = State.Mem.Size.of_bytes size in
      let data = exp_of_valu l vc data in
      State.write_noprov state ~addr ~size data
  | Cycle _ -> () (* Nothing happens here, this is just a marker *)
  | Branch _ -> () (* Nothing happens here, this is just a marker *)
  | BranchAddress _ -> () (* Nothing happens here, this is just a marker *)
  | WakeRequest _ -> ()
  | SleepRequest _ -> ()
  | MarkReg _ -> ()
  | Sleeping _ -> ()
  | Barrier _ -> ()
  | CacheOp _ -> ()
  | Instr _ -> ()

(* I don't need that information at that stage *)

(** This function run an isla trace on a state by mutation.
    If a [vc] is provided, then it is used and mutated according to the trace.

    Any encountered branch are ignored and their assertion are added to the state *)
let trc_mut ?(vc = HashVector.empty ()) (state : State.t) (trc : Isla.rtrc) =
  assert (not @@ State.is_locked state);
  let (Trace events) = trc in
  List.iter (event_mut vc state) events

(** This function run an isla trace on a state and return the end state as a new state

    It is just a wrapper of {!run_trc_mut} that remove the imperative interface
    The new state is fresh and locked.
*)
let trc start trc =
  let state = State.copy start in
  trc_mut state trc;
  State.lock state;
  state
