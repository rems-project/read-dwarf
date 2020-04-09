(** This module is designed to run symbolically (and later, maybe not) an ELF function
*)

open Cmdliner
open CommonOpt

open Logs.Logger (struct
  let str = "RunFunc"
end)

let run_func arch elfname name =
  base "Running %s in %s" name elfname;
  warn "Only ABI start for now";
  base "Loading %s" elfname;
  let dwarf = Dw.of_file elfname in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Opt.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  let api = Dw.Func.get_api func in
  base "API %t" (PP.top Arch.pp_api api);
  base "Loading ABI";
  let abi = Arch.get_abi api in
  Random.self_init ();
  IslaCache.start arch;
  Init.init ();
  base "Loading ABI";
  base "Init state:\n%t" (PP.topi State.pp (Init.state ()));
  let start = abi.init @@ Init.state () in
  base "Entry state:\n%t" (PP.topi State.pp start);
  IslaCache.stop ()

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FUNCTION" ~doc)

let term = Term.(func_options comopts run_func $ arch $ elf $ func)

let info =
  let doc = "Run a symbolically (or not) a single function" in
  Term.(info "run-func" ~doc ~exits)

let command = (term, info)
