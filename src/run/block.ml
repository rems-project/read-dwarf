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

(** This module add the [run-block] sub command.

    This subcommand is about running a complex block of execution
    It start at a specific offset in a block and can terminate on various condition.
    If not enough condition are met, it may crash by reaching an invalid instruction *)

open Cmdliner
open Config.CommonOpt
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

(* let dump =
 *   let doc = "Dump the traces of all the instructions in the symbol" in
 *   Arg.(value & flag & info ["d"; "dump"] ~doc) *)

let no_run =
  let doc = "Prevents the block from being run (to get the traces or reg type)" in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let reg_types =
  let doc = "Dump register types" in
  Arg.(value & flag & info ["t"; "types"] ~doc)

let len =
  let doc = "Stop condition: Stop as soon as the pc out of range [start, start + len) " in
  Arg.(value & opt (some int) None & info ["len"] ~docv:"BYTES" ~doc)

let breakpoints =
  let doc =
    "Stop condition: Stop as soon as the pc reach the position (symbol + offset or raw)"
  in
  Arg.(value & opt_all string [] & info ["b"; "break"] ~docv:"POSITION" ~doc)

let ensure_linear =
  let doc = "Ensure block has only one valid execution path" in
  Arg.(value & flag & info ["linear"] ~docv:"LINEAR" ~doc)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let start =
  let doc = "Position to start at (either raw hexadecimal or symbol + offset" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"START_POS" ~doc)

let get_elf_start elfname startname =
  let elf = Elf.File.of_file elfname in
  Arch.load_elf_arch elf;
  let start = Elf.SymTable.of_position_string elf.symbols startname in
  (elf, start)

let elf_term = Term.(CmdlinerHelper.func_options comopts get_elf_start $ elf $ start)

let gen_block ((elf : Elf.File.t), (symoffset : Elf.SymTable.sym_offset)) len breakpoints =
  let brks =
    List.map
      (Elf.SymTable.of_position_string elf.symbols %> Elf.SymTable.to_addr_offset)
      breakpoints
  in
  let start = Elf.SymTable.to_addr_offset symoffset in
  let (min, max) =
    let open Option in
    unlift_pair
    @@ let+ l = len in
       (start, Elf.Address.(start + l))
  in
  let endpred = Block_lib.gen_endpred ?min ?max ~brks () in
  Trace.Cache.start @@ Arch.get_isla_config ();
  let runner = Runner.of_elf elf in
  (elf, Block_lib.make ~runner ~start ~endpred)

let elfblock_term = Term.(const gen_block $ elf_term $ len $ breakpoints)

let rec prune_paths p (tree : _ State.Tree.t) =
  match tree.rest with
  | [] -> if p tree.state then None else Some tree
  | _ :: _ -> (
      match List.filter_map (prune_paths p) tree.rest with
      | [] -> None
      | _ :: _ as rest -> Some { tree with rest }
    )

let has_assert_false (state : State.t) =
  match state.asserts with [Bool (false, _)] -> true | _ -> false

let run_block (elf, block) no_run reg_types ensure_linear =
  if reg_types then base "Register types:\n%t\n" (Pp.topi State.Reg.pp_index ());
  if not no_run then begin
    let init_state = Init.state () |> State.copy ~elf in
    State.lock init_state;
    let tree = Block_lib.run block init_state in
    let print tree = Pp.println @@ State.Tree.pp_all Block_lib.pp_label tree in
    if ensure_linear then
      match prune_paths has_assert_false tree with
      | None -> fail "Block execution is not linear"
      | Some tree -> print tree
    else print tree
  end

let term = Term.(const run_block $ elfblock_term $ no_run $ reg_types $ ensure_linear)

let info =
  let doc =
    "Run complex block of instructions and print the tree of possible control flows. An end \
     condition must be specified using --len, -s/--stop-sym or -b/--break. If the end condition \
     is not specified enough and --stop-sym is not specified, then the run will fail."
  in
  Cmd.(info "run-block" ~doc ~exits)

let command = (term, info)
