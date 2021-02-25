(** This is the main test module that handle the external test suite *)

open Cmdliner

let tests = List.concat Tests.[BytesSeqT.tests; ConcreteEvalT.tests; SimplifyCheck.tests]

let config = Config.CommonOpt.config

let () = Config.File.ensure_loaded "../config/config.toml"

let () = Printexc.record_backtrace Config.enable_backtrace

let seed =
  let doc = "The seed to use to run the tests. Useful to run again with same input" in
  Arg.(value & opt (some int) None & info ["s"; "seed"] ~doc ~docv:"SEED")

let long =
  let doc = "Also run slow tests" in
  Arg.(value & flag & info ["l"; "long"] ~doc)

let verbose_test =
  let doc = "Use a verbose test output listing all the tests" in
  Arg.(value & flag & info ["verbose-test"] ~doc)

let debug_shrink =
  let doc = "Print all shrink steps details to the specified file" in
  Arg.(value & opt (some string) None & info ["debug_shrink"] ~doc ~docv:"FILE")

let debug_shrink_list =
  let doc = "All the tests about which debug shrinking info will be printed" in
  Arg.(value & opt_all string [] & info ["debug_shrink_list"] ~doc ~docv:"TEST_NAME")

let run_test verbose seed long debug_shrink_name debug_shrink_list =
  let seed =
    match seed with
    | None ->
        Random.self_init ();
        Random.int (1 lsl 29)
    | Some seed -> seed
  in
  Printf.printf "%sRunning read-dwarf main test suite with seed: %d\n\n%!"
    QCheck_base_runner.Color.reset_line seed;
  let rand = Random.State.make [|seed|] in
  let debug_shrink = Option.map Stdlib.open_out debug_shrink_name in
  QCheck_base_runner.run_tests ~verbose ~rand ~long ~debug_shrink ~debug_shrink_list tests

let term =
  Term.(
    CmdlinerHelper.func_options [config; Logs.term] run_test
    $ verbose_test $ seed $ long $ debug_shrink $ debug_shrink_list)

let info =
  let doc =
    "Run the main read-dwarf test suite. The command `make test` run this, the inline test suite \
     and also does some direct read-dwarf call and checks they do not fail"
  in
  let exits = Config.CommonOpt.exits in
  Term.(info "rd-tester" ~doc ~exits)

let command = (term, info)

let _ = Term.exit @@ Term.eval command

(* let () = QCheck_base_runner.run_tests_main tests *)
