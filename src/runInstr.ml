(** This module add the [run-instr] sub command.

    This subcommand is about testing the various operations that can happen
    in an instruction processing. This is not [isla-test],
    the instructions will always be fetched from the cache and use {!Init}.
    If this fail at an earlier point, use [isla-test] to debug the Isla pipeline

    Additionally [run-instr] supports branching instructions.
*)

(* Possible options

   - Use {!Trace or not}
   - -no-run (just dump trace)
   - final State simplification
   - [Trace] simplification
   - Possible dumps :
     - Preprocessed Trace
     - Register types
     - Trace.t
     - Initial State after Init
     - Final State before Simp
     - Final State after Simp
*)

open Cmdliner
open CommonOpt

open Logs.Logger (struct
  let str = "RunInstr"
end)

type traces = IslaTraces of Isla.rtrc list | Traces of Trace.t list

let instr =
  let doc = "Instruction to run(either directly on in sym+offset way. See --elf" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTR_OR_LOC" ~doc)

let dump_trace =
  let doc = "Dump the trace of the instruction" in
  Arg.(value & flag & info ["d"; "dump-trace"] ~doc)

let dump_isla =
  let doc = "Dump the isla trace of the instruction" in
  Arg.(value & flag & info ["dump-isla"] ~doc)

let no_run =
  let doc = "Prevents the instruction from being run" in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let isla_run =
  let doc = "Use IslaRun instead of TraceRun" in
  Arg.(value & flag & info ["isla-run"] ~doc)

let simp_trace =
  let doc = "Simplify the trace before running" in
  Arg.(value & flag & info ["simp-trace"] ~doc)

let simp_state =
  let doc = "Simplify the final state" in
  Arg.(value & flag & info ["simp-state"] ~doc)

let simp =
  let doc = "Implies both simp-state and simp-trace" in
  Arg.(value & flag & info ["s"; "simp"] ~doc)

let reg_types =
  let doc = "Dump register types" in
  Arg.(value & flag & info ["t"; "types"] ~doc)

let init =
  let doc = "Dump the initial state" in
  Arg.(value & flag & info ["i"; "init"] ~doc)

let elf =
  let doc =
    "If specified, the instruction is interpreted as ELF location, otherwise it's a test \
     instruction"
  in
  Arg.(value & opt (some non_dir_file) None & info ["e"; "elf"] ~doc)

let get_instr instr elfopt : BytesSeq.t =
  let (elfname, symname) =
    match elfopt with
    | None ->
        Arch.ensure_loaded Arch.Type.AARCH64;
        (Arch.assemble_to_elf instr, "instr")
    | Some elfname -> (elfname, instr)
  in
  let elf = Elf.File.of_file elfname in
  Elf.File.load_arch elf;
  if elfopt = None then Sys.remove elfname;
  let (sym, off) =
    try Elf.SymTbl.sym_offset_of_string elf.symbols symname
    with Not_found -> fail "The symbol %s cannot found in %s" symname elfname
  in
  debug "Got symbol:\n%t\n" (PP.topi Elf.Sym.pp_raw sym);
  let len = 4 (* TODO proper Instruction length system *) in
  BytesSeq.sub sym.data off len

let instr_term = Term.(func_options comopts get_instr $ instr $ elf)

let simp_trace_term = Term.(const ( || ) $ simp_trace $ simp)

let simp_state_term = Term.(const ( || ) $ simp_state $ simp)

let get_traces arch instr isla_run dump_types : traces =
  IslaCache.start arch;
  Init.init ();
  let rtraces = IslaCache.get_traces instr in
  List.iter (fun t -> IslaType.type_trc t |> ignore) rtraces;
  if dump_types then base "Register types:\n%t\n" (PP.topi Reg.pp_rstruct Reg.index);
  if isla_run then IslaTraces rtraces else Traces (List.map Trace.of_isla rtraces)

let pre_traces_term = Term.(const get_traces $ arch $ instr_term $ isla_run $ reg_types)

let simp_traces simp_traces traces =
  if simp_traces then (
    match traces with
    | IslaTraces trcs as it -> it
    | Traces trcs ->
        Z3.ensure_started ();
        Traces (List.map Trace.simplify trcs)
  )
  else traces

let dump_traces dump_traces traces =
  begin
    if dump_traces then
      match traces with
      | IslaTraces trcs ->
          List.iteri (fun i trc -> base "Trace %d:\n%t\n" i (PP.topi Isla.pp_trc trc)) trcs
      | Traces trcs ->
          List.iteri (fun i trc -> base "Trace %d:\n%t\n" i (PP.topi Trace.pp trc)) trcs
  end;
  traces

let traces_term =
  Term.(const dump_traces $ dump_trace $ (const simp_traces $ simp_trace_term $ pre_traces_term))

let run_instr dump_init norun simp_state traces =
  if not norun then begin
    let init_state = Init.state () in
    if dump_init then base "Initial state:\n%t\n" (PP.topi State.pp init_state);
    let states =
      match traces with
      | IslaTraces trcs -> List.map (IslaRun.trc init_state) trcs
      | Traces trcs -> List.map (TraceRun.trace init_state) trcs
    in
    let states =
      if simp_state then begin
        Z3.ensure_started ();
        List.map
          (fun state ->
            State.unsafe_unlock state;
            State.map_mut_exp Z3.simplify state;
            State.lock state;
            state)
          states
      end
      else states
    in
    List.iteri (fun i state -> base "State %d:\n%t\n" i (PP.topi State.pp state)) states
  end;
  IslaCache.stop ();
  Z3.ensure_stopped ()

let term = Term.(const run_instr $ init $ no_run $ simp_state_term $ traces_term)

let info =
  let doc =
    "Run a single instruction. This is for instruction that works. If something fails early int \
     the Isla pipeline, use isla-test"
  in
  Term.(info "run-instr" ~doc ~exits)

let command = (term, info)
