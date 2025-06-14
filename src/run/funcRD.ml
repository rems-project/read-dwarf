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

(** This module is a merge of [rd] sub command and [run-func --loop=1]. It will
    run the equivalent of [run-func --loop=1], and print the instruction like
    [rd] but will print the state in a light form ({!State.pp_partial}) between
    instructions.*)

open Cmdliner
open Config.CommonOpt
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

let run_func_rd elfname name objdump_d branchtables breakpoints =
  match Analyse.Utils.read_file_lines "src/analyse/html-preamble-insts.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines
  ;
  base "Running with rd %s in %s" name elfname;
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
  let start = Init.state () |> State.copy ~elf |> abi.init |> State.init_sections ~sp:Arch.sp ~addr_size:Arch.address_size in
  base "Loading %s for Analyse" elfname;
  let analyse_test = Analyse.Elf.parse_elf_file elfname in
  base "Analysing %s for Analyse" elfname;
  let analyse_analysis = Analyse.Collected.mk_analysis analyse_test objdump_d branchtables in
  let print_analyse_instruction pc =
    let pc = Elf.Address.to_sym pc in
    let index = analyse_analysis.index_of_address pc in
    let instr = analyse_analysis.instructions.(index) in
    Analyse.Pp.pp_instruction Analyse.Types.Html (*Ascii*) analyse_test analyse_analysis 0 index
      instr
  in
  base "Entry state:\n%t" Pp.(topi State.pp start);
  match func.sym with
  | None -> fail "Function %s exists in DWARF data but do not have any code" name
  | Some sym ->
      let brks =
        List.map
          (Elf.SymTable.of_position_string elf.symbols %> Elf.SymTable.to_addr_offset)
          breakpoints
      in
      let endpred = Block_lib.gen_endpred ~loop:1 ~brks () in
      let runner = Runner.of_dwarf dwarf in
      let block = Block_lib.make ~runner ~start:sym.addr ~endpred in
      base "Start running";
      let tree = Block_lib.run ~every_instruction:true block start in
      base "Ended running, start pretty printing";
      (* This table will contain the state diff to print at each pc with a message *)
      let instr_data : (Elf.Address.t, string * State.t * State.Reg.t list) Hashtbl.t =
        Hashtbl.create 100
      in
      let get_footprint pc =
        Runner.get_normal_opt runner pc |> Option.fold ~none:[] ~some:Trace.Instr.footprint
      in
      State.Tree.iter
        (fun a st ->
          match a with
          | Block_lib.Start -> ()
          | Block_lib.BranchAt pc ->
              let cur_instr_f = get_footprint pc in
              let last_pc = st.last_pc in
              let last_instr_f = get_footprint last_pc in
              let s =
                if Elf.Address.(last_pc + 4 <> pc) then Printf.sprintf "Coming from %t: " Pp.(tos Elf.Address.pp last_pc) else ""
              in
              let regs = List.merge_uniq Stdlib.compare cur_instr_f last_instr_f in
              Hashtbl.add instr_data pc (Printf.sprintf "%sBefore branch" s, st, regs)
          | Block_lib.NormalAt pc ->
              let cur_instr_f = get_footprint pc in
              let last_pc = st.last_pc in
              let last_instr_f = get_footprint last_pc in
              let s =
                if Elf.Address.(last_pc + 4 <> pc) then Printf.sprintf "Coming from %t: " Pp.(tos Elf.Address.pp last_pc) else ""
              in
              let regs = List.merge_uniq Stdlib.compare cur_instr_f last_instr_f in
              Hashtbl.add instr_data pc (Printf.sprintf "%sNormal instruction" s, st, regs)
          | Block_lib.End s ->
              let last_pc = st.last_pc in
              let last_instr = Runner.expect_normal runner last_pc in
              Hashtbl.add instr_data Elf.Address.(st.last_pc + 4)
                (Printf.sprintf "End because: %s" s, st, Trace.Instr.footprint last_instr))
        tree;
      Vec.iter
        (fun funcaddr ->
          let sym = Elf.SymTable.of_addr elf.symbols funcaddr in
          Analyse.Pp.pp_instruction_init ();
          Seq.iota_step_up ~start:0 ~step:4 ~endi:sym.size
          |> Seq.iter (fun pc_diff ->
                let pc = Elf.Address.(funcaddr + pc_diff) in
                 Hashtbl.find_all instr_data pc
                 |> List.iter (fun (msg, st, regs) ->
                        base "At %t, %s:\n%t" Pp.(top Elf.Address.pp pc) msg Pp.(topi (State.pp_partial ~regs) st));
                 print_string (print_analyse_instruction pc)))
        runner.funcs; 
  match Analyse.Utils.read_file_lines "src/analyse/html-postamble.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FUNCTION" ~doc)

let objdump_d =
  let doc = "File containing result of objdump -d" in
  Arg.(required & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_table =
  let doc = "File containing branch table base addresses and sizes" in
  Arg.(
    (* required *)
    value & opt (some non_dir_file) None & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let breakpoints =
  let doc =
    "Stop condition: Stop as soon as the pc reach the position ((symbol + offset) or raw)"
  in
  Arg.(value & opt_all string [] & info ["b"; "break"] ~docv:"POSITION" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run_func_rd
    $ elf $ func $ objdump_d $ branch_table $ breakpoints)

let info =
  let doc =
    "Run a symbolically (or not) a single function like run-func with option --loop=1. Instead \
     of printing a tree of possibility. The output will be printed with the rd format with state \
     diffs between instructions"
  in
  Cmd.(info "run-func-rd" ~doc ~exits)

let command = (term, info)
