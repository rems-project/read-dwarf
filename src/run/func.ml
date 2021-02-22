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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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

(** This module is designed to run symbolically (and later, maybe not) an ELF function *)

open Cmdliner
open Config.CommonOpt
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

let run_func elfname name dump no_run entry len breakpoints loop =
  base "Running %s in %s" name elfname;
  base "Loading %s" elfname;
  let dwarf = Dw.of_file elfname in
  let elf = dwarf.elf in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Option.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  let api = Dw.Func.get_api func in
  base "API %t" (Pp.top Arch.pp_api api);
  base "Loading ABI";
  let abi = Arch.get_abi api in
  Trace.Cache.start @@ Arch.get_isla_config ();
  base "Computing entry state";
  let start = Init.state () |> State.copy ~elf |> abi.init in
  if entry then base "Entry state:\n%t" (Pp.topi State.pp start);
  begin
    if (not no_run) || dump then
      match func.sym with
      | None -> fail "Function %s exists in DWARF data but does not have any code" name
      | Some sym ->
          let brks =
            List.map
              (Elf.SymTable.of_position_string elf.symbols %> Elf.SymTable.to_addr_offset)
              breakpoints
          in
          let (min, max) =
            let open Option in
            unlift_pair
            @@ let+ l = len in
               (sym.addr, sym.addr + l)
          in
          let endpred = Block_lib.gen_endpred ?min ?max ?loop ~brks () in
          let runner = Runner.of_dwarf dwarf in
          let block = Block_lib.make ~runner ~start:sym.addr ~endpred in
          if dump then begin
            base "Preloading Instructions for dump";
            Runner.load_sym runner sym;
            base "Instructions:\n%t\n" (Pp.topi Runner.pp_instr runner)
          end;
          if not no_run then base "Start running";
          let tree = Block_lib.run block start in
          base "Run tree:\n%t" (Pp.top (State.Tree.pp_all Block_lib.pp_label) tree)
  end;
  Isla.Cache.stop ()

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FUNCTION" ~doc)

let dump =
  let doc = "Dump the traces of all the instructions in the function" in
  Arg.(value & flag & info ["d"; "dump"] ~doc)

let no_run =
  let doc = "Prevents the function from being run" in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let entry =
  let doc = "Still dump the entry state if -n/--no-run is selected" in
  Arg.(value & flag & info ["e"; "entry"] ~doc)

let loop =
  let doc = "Number of times to run loop, i.e number of time going over a PC is allowed" in
  Arg.(value & opt (some int) None & info ["l"; "loop"] ~doc)

let len =
  let doc = "Stop condition: Stop as soon as the pc out of range [start, start + len) " in
  Arg.(value & opt (some int) None & info ["len"] ~docv:"BYTES" ~doc)

let breakpoints =
  let doc =
    "Stop condition: Stop as soon as the pc reach the position ((symbol + offset) or raw)"
  in
  Arg.(value & opt_all string [] & info ["b"; "break"] ~docv:"POSITION" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run_func
    $ elf $ func $ dump $ no_run $ entry $ len $ breakpoints $ loop)

let info =
  let doc =
    "Run symbolically (or not) a single function. From the start until some end condition is \
     reached. If no other end condition if specified it will run until some symbolic PC is \
     encountered which generally means a return (The return address is symbolic). If any loop is \
     present termination is not ensured unless option -l/--loop is used"
  in
  Term.(info "run-func" ~doc ~exits)

let command = (term, info)
