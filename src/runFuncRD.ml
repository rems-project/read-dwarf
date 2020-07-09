(** This module is a merge of [rd] sub command and [run-func --loop=1].
    It will run the equivalent of [run-func --loop=1], and print the instruction like
    [rd] but with a {!StateDiff} at relevant points.
*)

open Cmdliner
open CommonOpt
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

let run_func_rd elfname name objdump_d branchtables breakpoints =
  base "Running with rd %s in %s" name elfname;
  base "Loading %s" elfname;
  let dwarf = Dw.of_file elfname in
  let elf = dwarf.elf in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Opt.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  let api = Dw.Func.get_api func in
  base "API %t" (PP.top Arch.pp_api api);
  base "Loading ABI";
  let abi = Arch.get_abi api in
  TraceCache.start @@ Arch.get_isla_config ();
  Init.init ();
  base "Computing entry state";
  let start = Init.state () |> State.copy ~elf |> abi.init in
  base "Loading %s for Analyse" elfname;
  let analyse_test = Analyse.parse_elf_file elfname in
  base "Analysing %s for Analyse" elfname;
  let analyse_analysis = Analyse.mk_analysis analyse_test objdump_d branchtables in
  let print_analyse_instruction pc =
    let pc = Z.of_int pc in
    let index = analyse_analysis.index_of_address pc in
    let instr = analyse_analysis.instructions.(index) in
    Analyse.pp_instruction Types.Ascii analyse_test analyse_analysis index instr
  in
  base "Entry state:\n%t" PP.(topi State.pp start);
  match func.sym with
  | None -> fail "Function %s exists in DWARF data but do not have any code" name
  | Some sym ->
      let brks =
        List.map
          (Elf.SymTbl.of_position_string elf.symbols %> Elf.SymTbl.to_addr_offset)
          breakpoints
      in
      let endpred = Block.gen_endpred ~loop:1 ~brks () in
      let runner = Runner.of_dwarf dwarf in
      let block = Block.make ~runner ~start:sym.addr ~endpred in
      base "Start running";
      let tree = Block.run ~every_instruction:true block start in
      base "Ended running, start pretty printing";
      (* This table will contain the state diff to print at each pc with a message *)
      let instr_data : (int, string * State.t * Reg.t list) Hashtbl.t = Hashtbl.create 100 in
      let get_footprint pc =
        Runner.get_normal_opt runner pc |> Opt.fold ~none:[] ~some:Instr.footprint
      in
      StateTree.iter
        (fun a st ->
          match a with
          | Block.Start -> ()
          | Block.BranchAt pc ->
              let cur_instr_f = get_footprint pc in
              let last_pc = st.last_pc in
              let last_instr_f = get_footprint last_pc in
              let s =
                if last_pc <> pc - 4 then Printf.sprintf "Coming from 0x%x: " last_pc else ""
              in
              let regs = List.merge_uniq Stdlib.compare cur_instr_f last_instr_f in
              Hashtbl.add instr_data pc (Printf.sprintf "%sBefore branch" s, st, regs)
          | Block.NormalAt pc ->
              let cur_instr_f = get_footprint pc in
              let last_pc = st.last_pc in
              let last_instr_f = get_footprint last_pc in
              let s =
                if last_pc <> pc - 4 then Printf.sprintf "Coming from 0x%x: " last_pc else ""
              in
              let regs = List.merge_uniq Stdlib.compare cur_instr_f last_instr_f in
              Hashtbl.add instr_data pc (Printf.sprintf "%sNormal instruction" s, st, regs)
          | Block.End s ->
              let last_pc = st.last_pc in
              let last_instr = Runner.expect_normal runner last_pc in
              Hashtbl.add instr_data (st.last_pc + 4)
                (Printf.sprintf "End because: %s" s, st, last_instr.footprint))
        tree;
      Vec.iter
        (fun funcaddr ->
          let sym = Elf.SymTbl.of_addr elf.symbols funcaddr in
          Analyse.pp_instruction_init ();
          Seq.iota_step_up ~start:funcaddr ~step:4 ~endi:(funcaddr + sym.size)
          |> Seq.iter (fun pc ->
                 Hashtbl.find_all instr_data pc
                 |> List.iter (fun (msg, st, regs) ->
                        base "At 0x%x, %s:\n%t" pc msg PP.(topi (State.pp_partial ~regs) st));
                 print_string (print_analyse_instruction pc)))
        runner.funcs

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
    required
    & opt (some non_dir_file) None
    & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let breakpoints =
  let doc =
    "Stop condition: Stop as soon as the pc reach the position ((symbol + offset) or raw)"
  in
  Arg.(value & opt_all string [] & info ["b"; "break"] ~docv:"POSITION" ~doc)

let term =
  Term.(func_options comopts run_func_rd $ elf $ func $ objdump_d $ branch_table $ breakpoints)

let info =
  let doc =
    "Run a symbolically (or not) a single function like run-func with option --loop=1. Instead \
     of printing a tree of possibility. The output will be printed with the rd format with state \
     diffs between instructions"
  in
  Term.(info "run-func-rd" ~doc ~exits)

let command = (term, info)
