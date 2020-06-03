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

let run_func_rd elfname name objdump_d branchtables every =
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
  match func.sym with
  | None -> fail "Function %s exists in DWARF data but do not have any code" name
  | Some sym ->
      let open Opt in
      let endpred = Block.gen_endpred ~loop:1 ~brks:[] () in
      let runner = Runner.of_dwarf dwarf in
      let block = Block.make ~runner ~start:sym.addr ~endpred in
      base "Start running";
      let tree = Block.run block start in
      base "Ended running, start pretty printing";
      (* This table will contain the state diff to print at each pc with a message *)
      let instr_data : (int, string * StateDiff.t) Hashtbl.t = Hashtbl.create 100 in
      let pc_reg = Arch.pc () in
      StateTree.iter
        (fun a st ->
          match State.previous st with
          | None -> ()
          | Some prev -> (
              let diff = StateDiff.diff prev st in
              match a with
              | Block.Start -> ()
              | Block.BranchAt pc -> Hashtbl.add instr_data pc ("Before branch", diff)
              | Block.End s ->
                  Hashtbl.add instr_data (st.last_pc + 4)
                    (Printf.sprintf "End because: %s" s, diff)
            ))
        tree;
      Vec.iter
        (fun funcaddr ->
          let sym = Elf.SymTbl.of_addr elf.symbols funcaddr in
          Analyse.pp_instruction_init ();
          Seq.iota_step_up ~start:funcaddr ~step:4 ~endi:(funcaddr + sym.size)
          |> Seq.iter (fun pc ->
                 begin
                   match Hashtbl.find_opt instr_data pc with
                   | Some (msg, stdiff) ->
                       base "StateDiff at 0x%x, %s:\n%t" pc msg PP.(topi StateDiff.pp stdiff)
                   | None -> ()
                 end;
                 print_string (print_analyse_instruction pc)))
        runner.funcs

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FUNCTION" ~doc)

let every =
  let doc =
    "Whether to dump state diff at every instruction (by default, only at branch points)\n\
    \      (unimplemented for now, will be ingnored)"
  in
  Arg.(value & flag & info ["e"; "every-instruction"] ~doc)

let objdump_d =
  let doc = "File containing result of objdump -d" in
  Arg.(required & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_table =
  let doc = "File containing branch table base addresses and sizes" in
  Arg.(
    required
    & opt (some non_dir_file) None
    & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let term = Term.(func_options comopts run_func_rd $ elf $ func $ objdump_d $ branch_table $ every)

let info =
  let doc =
    "Run a symbolically (or not) a single function like run-func with option --loop=1. Instead \
     of printing a tree of possibility. The output will be printed with the rd format with state \
     diffs between instructions"
  in
  Term.(info "run-func-rd" ~doc ~exits)

let command = (term, info)
