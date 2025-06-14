(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module add the [run-instr] sub command.

    This subcommand is about testing the various operations that can happen
    in an instruction processing. This is not [isla-test],
    the instructions will always be fetched from the cache and use {!Init}.
    If this fail at an earlier point, use [isla-test] to debug the Isla pipeline

    Additionally [run-instr] supports branching instructions.
*)

(* Possible options

   - Use {!Trace} or not
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
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
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

let get_instr arch instr elfopt : BytesSeq.t =
  let (elfname, symname) =
    match elfopt with
    | None ->
        Arch.ensure_loaded arch;
        (Arch.assemble_to_elf instr, "instr")
    | Some elfname -> (elfname, instr)
  in
  let elf = Elf.File.of_file elfname in
  Arch.load_elf_arch elf;
  if elfopt = None then Sys.remove elfname;
  let (sym, off) =
    try Elf.SymTable.of_position_string elf.symbols symname
    with Not_found -> fail "The symbol %s cannot found in %s" symname elfname
  in
  debug "Got symbol:\n%t\n" (Pp.topi Elf.Symbol.pp_raw sym);
  let len = 4 (* TODO proper Instruction length system *) in
  BytesSeq.sub sym.data.data off len (*TODO relocations*)

let instr_term = Term.(CmdlinerHelper.func_options comopts get_instr $ arch $ instr $ elf)

let simp_trace_term = Term.(const ( || ) $ simp_trace $ simp)

let simp_state_term = Term.(const ( || ) $ simp_state $ simp)

let get_traces _instr _isla_run _dump_types : traces =
  Raise.todo()
  (* Isla.Cache.start @@ Arch.get_isla_config ();
  (* I call Init.init manually to print the register types *)
  Init.init () |> ignore;
  let rtraces = Isla.Cache.get_traces (instr, None) in (* TODO relocs *)
  List.iter (fun t -> Isla.Type.type_trc t |> ignore) rtraces;
  if dump_types then base "Register types:\n%t\n" (Pp.topi State.Reg.pp_index ());
  if isla_run then IslaTraces rtraces else Traces (List.map Trace.of_isla rtraces) *)

let pre_traces_term = Term.(const get_traces $ instr_term $ isla_run $ reg_types)

let simp_traces simp_traces traces =
  if simp_traces then (
    match traces with
    | IslaTraces _ -> traces
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
          List.iteri (fun i trc -> base "Trace %d:\n%t\n" i (Pp.topi Isla.pp_trc trc)) trcs
      | Traces trcs ->
          List.iteri (fun i trc -> base "Trace %d:\n%t\n" i (Pp.topi Trace.pp trc)) trcs
  end;
  traces

let traces_term =
  Term.(const dump_traces $ dump_trace $ (const simp_traces $ simp_trace_term $ pre_traces_term))

let run_instr dump_init norun simp_state traces =
  if not norun then begin
    let init_state = Init.state () in
    if dump_init then base "Initial state:\n%t\n" (Pp.topi State.pp init_state);
    let states =
      match traces with
      | IslaTraces trcs ->
          List.map ((Isla.Run.trc [@ocaml.warning "-3"] (* deprecated *)) init_state) trcs
      | Traces trcs -> List.map (Trace.Run.trace init_state) trcs
    in
    let states =
      if simp_state then begin
        Z3.ensure_started ();
        List.map
          (fun state ->
            (State.unsafe_unlock [@ocaml.warning "-3"] (* deprecated *)) state;
            State.Simplify.ctxfull state;
            State.lock state;
            state)
          states
      end
      else states
    in
    List.iteri (fun i state -> base "State %d:\n%t\n" i (Pp.topi State.pp state)) states
  end;
  Isla.Cache.stop ();
  Z3.ensure_stopped ()

let term = Term.(const run_instr $ init $ no_run $ simp_state_term $ traces_term)

let info =
  let doc =
    "Run a single instruction. This is for instruction that works. If something fails early int \
     the Isla pipeline, use isla-test"
  in
  Cmd.(info "run-instr" ~doc ~exits)

let command = (term, info)
