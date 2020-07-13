(** This module is designed to run symbolically (and later, maybe not) an ELF function
*)

open Cmdliner
open CommonOpt
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
  if entry then base "Entry state:\n%t" (PP.topi State.pp start);
  begin
    if (not no_run) || dump then
      match func.sym with
      | None -> fail "Function %s exists in DWARF data but do not have any code" name
      | Some sym ->
          let brks =
            List.map
              (Elf.SymTbl.of_position_string elf.symbols %> Elf.SymTbl.to_addr_offset)
              breakpoints
          in
          let (min, max) =
            let open Opt in
            unlift_pair
            @@ let+ l = len in
               (sym.addr, sym.addr + l)
          in
          let endpred = Block.gen_endpred ?min ?max ?loop ~brks () in
          let runner = Runner.of_dwarf dwarf in
          let block = Block.make ~runner ~start:sym.addr ~endpred in
          if dump then begin
            base "Preloading Instructions for dump";
            Runner.load_sym runner sym;
            base "Instructions:\n%t\n" (PP.topi Runner.pp_instr runner)
          end;
          if not no_run then base "Start running";
          let tree = Block.run block start in
          base "Run tree:\n%t" (PP.top (StateTree.pp_all Block.pp_label) tree)
  end;
  IslaCache.stop ()

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
    func_options comopts run_func $ elf $ func $ dump $ no_run $ entry $ len $ breakpoints $ loop)

let info =
  let doc =
    "Run symbolically (or not) a single function. From the start until some end condition is \
     reached. If no other end condition if specified it will run until some symbolic PC is \
     encountered which generally means a return (The return address is symbolic). If any loop is \
     present termination is not ensured unless option -l/--loop is used"
  in
  Term.(info "run-func" ~doc ~exits)

let command = (term, info)
