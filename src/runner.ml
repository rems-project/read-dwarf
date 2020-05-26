(** This module provide the program runner that caches all the information
    required to make a state transition

    Later this module will handle inlining in a transparent way, and
    may also compress basic block traces (We'll need to check this is okay with type inference)


    The final goal of this module is to encode all necessary information to perform a
    state transition. In particular, the {!run} function should never require more
    argument: If extra information is required, it should be part of the runner if
    it is dependent on the whole program or in the state if it is specific to the state.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Give the instruction descriptor at a given address *)
type slot =
  | Normal of Trace.t list * int  (** The traces and the size of the instruction *)
  | Special  (** Special instructions. Will be used to represent external events *)
  | Nocode
      (** The is no code at this address. Running it is UB.
          Also used if an address is in between instructions *)
  | IslaFail  (** This means Isla pipeline failed on that instruction *)

type t = {
  elf : Elf.File.t;
  dwarf : Dw.t option;
  instrs : (int, slot) Hashtbl.t;  (** Instruction cache *)
  pc : Reg.t;
}

let of_elf ?dwarf elf =
  let instrs = Hashtbl.create 100 in
  let pc = Arch.pc () in
  { elf; dwarf; instrs; pc }

let of_dwarf dwarf = of_elf ~dwarf dwarf.elf

(** Load a symbol into the runner. All instruction traces are fetched and cached.

    This may imply the discovery of some new register and thus require a state extension.*)
let load_sym runner (sym : Elf.Sym.t) =
  info "Loading symbol %s in %s" sym.name runner.elf.filename;
  let opcode_list = BytesSeq.to_list32bs sym.data in
  List.iteri
    (fun index code ->
      let addr = sym.addr + (4 * index) in
      try
        let traces = TraceCache.get_traces code in
        if traces = [] then begin
          debug "Instruction at 0x%x in %s is loaded as special" addr sym.name;
          Hashtbl.add runner.instrs addr Special
        end
        else begin
          debug "Instruction at 0x%x in %s is loaded as normal. Traces are:\n%t" addr sym.name
            PP.(topi Trace.pp_multiple traces);
          Hashtbl.add runner.instrs addr (Normal (traces, 4))
        end
      with exn ->
        warn "Could not convert isla trace of instruction at 0x%x in %s to Trace.t: %s" addr
          runner.elf.filename (Printexc.to_string exn);
        Hashtbl.add runner.instrs addr IslaFail)
    opcode_list

(** Fetch an instruction, and return corresponding slot. *)
let fetch (runner : t) (pc : int) : slot =
  debug "Fetching PC 0x%x" pc;
  match Hashtbl.find_opt runner.instrs pc with
  | Some v -> v
  | None -> (
      match Elf.SymTbl.of_addr_opt runner.elf.symbols pc with
      | Some sym when sym.typ = Elf.Sym.FUNC ->
          if Hashtbl.mem runner.instrs sym.addr then begin
            warn "Tried to fetch in middle of instructions in %s at 0x%x" runner.elf.filename pc;
            Hashtbl.add runner.instrs pc Nocode;
            Nocode
          end
          else begin
            load_sym runner sym;
            match Hashtbl.find_opt runner.instrs pc with
            | Some v -> v
            | None ->
                warn "Tried to fetch in middle of instructions in %s at 0x%x" runner.elf.filename
                  pc;
                Hashtbl.add runner.instrs pc Nocode;
                Nocode
          end
      | _ ->
          warn "Tried to fetch outside of normal code in %s at 0x%x" runner.elf.filename pc;
          Hashtbl.add runner.instrs pc Nocode;
          Nocode
    )

(** Run the traces on the state.

    If the state is unlocked and the instruction is single trace, then
    the function mutates the state and returns the same state.

    Otherwise the state is locked (if not) and new states are
    returned

    In any case the returned states are unlocked.
*)
let execute_normal ?(prelock = fun state -> ()) runner traces next state =
  let dwarf = runner.dwarf in
  let run_pure () =
    List.map
      (fun trc ->
        let nstate = State.copy state in
        TraceRun.trace_pc_mut ?dwarf ~next nstate trc;
        nstate)
      traces
  in
  if not (State.is_locked state) then begin
    match traces with
    | [trc] ->
        TraceRun.trace_pc_mut ?dwarf ~next state trc;
        [state]
    | trcs ->
        prelock state;
        State.lock state;
        run_pure ()
  end
  else run_pure ()

(** Do the whole fetch and execute.
    Take the PC from the state, and fetch it's {!Trace} and then run it.

    If the state is unlocked and the instruction is single trace, then
    the function mutates the state and returns the same state.

    Otherwise the state is locked (if not) and new states are
    returned

    In any case the returned states are unlocked.
*)
let run ?prelock runner state : State.t list =
  let pc_exp = State.get_reg state runner.pc |> State.get_exp in
  try
    let pc = pc_exp |> Ast.expect_bits |> BitVec.to_int in
    match fetch runner pc with
    | Normal (traces, next) -> execute_normal ?prelock runner traces next state
    | Special ->
        Raise.fail "Special instruction at 0x%x in %s. unsupported for now" pc runner.elf.filename
    | Nocode -> Raise.fail "Trying to run 0x%x in %s: no code there" pc runner.elf.filename
    | IslaFail ->
        Raise.fail "Trying to run 0x%x in %s: Isla pipeline failed on that instruction" pc
          runner.elf.filename
  with exn ->
    err "Trying to run instruction at %t in %s: too symbolic"
      PP.(top State.pp_exp pc_exp)
      runner.elf.filename;
    Raise.again exn

(** Pretty prints a instruction slot *)
let pp_slot =
  let open PP in
  function
  | Normal (traces, next) ->
      prefix 4 1 (dprintf "Normal instruction of size %d:" next) (Trace.pp_multiple traces)
  | Special -> !^"Special instruction"
  | Nocode -> !^"Not an instruction"
  | IslaFail -> !^"Isla failed at that PC: investigate"

(** Dump instruction table *)
let pp_instr (runner : t) =
  let open PP in
  hashtbl_sorted ~name:"Instructions" ~compare ptr pp_slot runner.instrs
