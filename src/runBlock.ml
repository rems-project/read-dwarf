(** This module add the [run-block] sub command.

    This subcommand is about running a complex block of execution
    It start at a specific offset in a block and can terminate on various condition.
    If not enough condition are met, it may crash by reaching an invalid instruction


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
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

let dump =
  let doc = "Dump the traces of all the instructions in the symbol" in
  Arg.(value & flag & info ["d"; "dump"] ~doc)

let no_run =
  let doc = "Prevents the block from being run (To get the traces or reg type" in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let reg_types =
  let doc = "Dump register types" in
  Arg.(value & flag & info ["t"; "types"] ~doc)

let len =
  let doc = "Stop condition: Stop as soon as the pc out of range [start, start + len) " in
  Arg.(value & opt (some int) None & info ["len"] ~docv:"BYTES" ~doc)

let stop_sym =
  let doc = "Stop condition: Stop as soon as the pc is symbolic" in
  Arg.(value & flag & info ["s"; "stop-sym"] ~docv:"BYTES" ~doc)

let breakpoints =
  let doc =
    "Stop condition: Stop as soon as the pc reach the position (symbol + offset or raw)"
  in
  Arg.(value & opt_all string [] & info ["b"; "break"] ~docv:"POSITION" ~doc)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let start =
  let doc = "Position to start at (either raw hexadecimal or symbol + offset" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"START_POS" ~doc)

let get_elf_start elfname startname =
  let elf = Elf.File.of_file elfname in
  Elf.File.load_arch elf;
  let start = Elf.SymTbl.of_position_string elf.symbols startname in
  (elf, start)

let elf_term = Term.(func_options comopts get_elf_start $ elf $ start)

let gen_block arch ((elf : Elf.File.t), (symoffset : Elf.SymTbl.sym_offset)) len stop_sym
    breakpoints =
  let brks =
    List.map (Elf.SymTbl.of_position_string elf.symbols %> Elf.SymTbl.to_addr_offset) breakpoints
  in
  let endpred (exp : State.exp) =
    match exp with
    | Bits (bv, _) -> (
        let addr = BitVec.to_int bv in
        let start = Elf.SymTbl.to_addr_offset symoffset in
        match len with
        | Some l when start > addr || addr >= start + l -> true
        | _ -> List.exists (( = ) addr) brks
      )
    | _ -> stop_sym
  in
  IslaCache.start arch;
  let (sym, start) = symoffset in
  Block.make ~sym ~start ~endpred

let block_term = Term.(const gen_block $ arch $ elf_term $ len $ stop_sym $ breakpoints)

let run_block block no_run dump reg_types =
  Block.simplify_mut block;
  if reg_types then base "Register types:\n%t\n" (PP.topi Reg.pp_rstruct Reg.index);
  if dump then base "Block:\n%t\n" (PP.topi Block.pp block);
  if not no_run then begin
    Init.init ();
    let init_state = Init.state () in
    let tree = Block.run block init_state in
    PP.println @@ StateTree.pp_all PP.shex tree
  end

let term = Term.(const run_block $ block_term $ no_run $ dump $ reg_types)

let info =
  let doc =
    "Run complex block of instructions and print the tree of possible control flows. An end \
     condition must be specified using --len, -s/--stop-sym or -b/--break. If the end condition \
     is not specified enough and --stop-sym is not specified, then the run will fail."
  in
  Term.(info "run-block" ~doc ~exits)

let command = (term, info)
