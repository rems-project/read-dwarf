(** This module add the [run-block] sub command.

    This subcommand is about running a complex block of execution
    It start at a specific offset in a block and can terminate on various condition.
    If not enough condition are met, it may crash by reaching an invalid instruction *)

open Cmdliner
open CommonOpt
open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

(* let dump =
 *   let doc = "Dump the traces of all the instructions in the symbol" in
 *   Arg.(value & flag & info ["d"; "dump"] ~doc) *)

let no_run =
  let doc = "Prevents the block from being run (To get the traces or reg type" in
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

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let start =
  let doc = "Position to start at (either raw hexadecimal or symbol + offset" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"START_POS" ~doc)

let get_elf_start elfname startname =
  let elf = Elf.File.of_file elfname in
  Arch.load_elf_arch elf;
  let start = Elf.SymTbl.of_position_string elf.symbols startname in
  (elf, start)

let elf_term = Term.(func_options comopts get_elf_start $ elf $ start)

let gen_block ((elf : Elf.File.t), (symoffset : Elf.SymTbl.sym_offset)) len breakpoints =
  let brks =
    List.map (Elf.SymTbl.of_position_string elf.symbols %> Elf.SymTbl.to_addr_offset) breakpoints
  in
  let start = Elf.SymTbl.to_addr_offset symoffset in
  let (min, max) =
    let open Opt in
    unlift_pair
    @@ let+ l = len in
       (start, start + l)
  in
  let endpred = Block.gen_endpred ?min ?max ~brks () in
  TraceCache.start @@ Arch.get_isla_config ();
  let runner = Runner.of_elf elf in
  (elf, Block.make ~runner ~start ~endpred)

let elfblock_term = Term.(const gen_block $ elf_term $ len $ breakpoints)

let run_block (elf, block) no_run reg_types =
  if reg_types then base "Register types:\n%t\n" (PP.topi Reg.pp_index ());
  if not no_run then begin
    let init_state = Init.state () |> State.copy ~elf in
    State.lock init_state;
    let tree = Block.run block init_state in
    PP.println @@ StateTree.pp_all Block.pp_label tree
  end

let term = Term.(const run_block $ elfblock_term $ no_run $ reg_types)

let info =
  let doc =
    "Run complex block of instructions and print the tree of possible control flows. An end \
     condition must be specified using --len, -s/--stop-sym or -b/--break. If the end condition \
     is not specified enough and --stop-sym is not specified, then the run will fail."
  in
  Term.(info "run-block" ~doc ~exits)

let command = (term, info)
