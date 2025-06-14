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

(** This module allow to do a test run of all the machinery for a single basic block *)

open Cmdliner
open Config.CommonOpt
module SMT = Z3

open Logs.Logger (struct
  let str = __MODULE__
end)

let dump =
  let doc = "Dump the trace representation of the basic block" in
  Arg.(value & flag & info ["d"; "dump"] ~doc)

let reg_types =
  let doc = "Print the register types used by the basic block (and base set)" in
  Arg.(value & flag & info ["t"; "types"] ~doc)

let no_run =
  let doc =
    "Do not run the basic block (useful to only get pre-run data like --dump-trace or --type)"
  in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let simp_trace =
  let doc = "Simplify the traces before running" in
  Arg.(value & flag & info ["simp-trace"] ~doc)

let simp_state =
  let doc = "Simplify the final state" in
  Arg.(value & flag & info ["simp-state"] ~doc)

let simp =
  let doc = "Implies both --simp-state and --simp-trace" in
  Arg.(value & flag & info ["s"; "simp"] ~doc)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let sym =
  let doc = "The symbol to analyse, \"test\" by default. An offset can be added like sym+8" in
  Arg.(value & opt string "test" & info ["sym"] ~doc)

let len =
  let doc =
    "The length in byte of the binary block to consider, "
    ^ "if unspecified, will run until the end of the symbol"
  in
  Arg.(value & opt (some int) None & info ["l"; "len"] ~doc)

let get_code elfname symname len : BytesSeq.t =
  let elf = Elf.File.of_file elfname in
  Arch.load_elf_arch elf;
  let (sym, off) =
    try Elf.SymTable.of_position_string elf.symbols symname
    with Not_found -> fail "The symbol %s cannot found in %s" symname elfname
  in
  let len = match len with Some i -> i | None -> sym.size - off in
  (Elf.Symbol.sub sym off len).data (*TODO relocations*)

let code_term = Term.(CmdlinerHelper.func_options comopts get_code $ elf $ sym $ len)

let simp_trace_term = Term.(const ( || ) $ simp_trace $ simp)

let simp_state_term = Term.(const ( || ) $ simp_state $ simp)

let get_bb dump reg_types simp_trace code : Bb_lib.t =
  Isla.Cache.start @@ Arch.get_isla_config ();
  (* I call Init.init manually to print the register types *)
  Init.init () |> ignore;
  let bb = Bb_lib.from_binary code in
  if reg_types then base "Register types:\n%t\n" (Pp.topi State.Reg.pp_index ());
  if simp_trace then begin
    Z3.ensure_started ();
    Bb_lib.simplify_mut bb
  end;
  if dump then base "Basic block:\n%t\n" (Pp.topi Bb_lib.pp bb);
  bb

let bb_term = Term.(const get_bb $ dump $ reg_types $ simp_trace_term $ code_term)

let run_bb norun simp_state bb =
  if not norun then begin
    let init_state = Init.state () in
    let state = Bb_lib.run init_state bb in
    if simp_state then begin
      Z3.ensure_started ();
      (State.unsafe_unlock [@ocaml.warning "-3"] (* deprecated *)) state;
      State.Simplify.ctxfull state;
      State.lock state
    end;
    base "Final state:\n%t\n" (Pp.topi State.pp state)
  end;
  Isla.Cache.stop ();
  Z3.ensure_stopped ()

let term = Term.(const run_bb $ no_run $ simp_state_term $ bb_term)

let info =
  let doc =
    "Run a basic block. This will run instructions in order, without updating the PC: Any jump \
     will be ignored."
  in
  Cmd.(info "run-bb" ~doc ~exits)

let command = (term, info)
