(** This is the main test module that handle the external test suite *)

open Cmdliner
open CommonOpt

let tests = List.concat [BytesSeqT.tests; ConcreteEvalT.tests; SimplifyCheck.tests]

let _ = Cmdliner.Term.eval_peek_opts CommonOpt.config

let () = Printexc.record_backtrace Config.enable_backtrace

(* let _ = Cmdliner.Term.eval_peek_opts CommonOpt.logs_term *)

let seed =
  let doc = "The seed to use to run the tests. Useful to run again with same input" in
  Arg.(value & opt (some int) None & info ["s"; "seed"] ~doc ~docv:"SEED")

let long =
  let doc = "Also run slow tests" in
  Arg.(value & flag & info ["l"; "long"] ~doc)

let verbose =
  let doc = "Use a verbose test output listing all the tests" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let debug_shrink =
  let doc = "Print all shrink steps details to the specified file" in
  Arg.(value & opt (some string) None & info ["debug_shrink"] ~doc ~docv:"FILE")

let debug_shrink_list =
  let doc = "All the tests about which debug shrinking info will be printed" in
  Arg.(value & opt_all string [] & info ["debug_shrink_list"] ~doc ~docv:"TEST_NAME")

let verbose_log =
  let doc = "Log more stuff. When set twice, output all debugging logs" in
  Arg.(value & flag_all & info ["verbose-logs"] ~doc)

let logs_term =
  Term.(const process_logs_opts $ quiet $ verbose_log $ infoopt $ debug $ stdout_level)

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
    func_options [config; logs_term] run_test
    $ verbose $ seed $ long $ debug_shrink $ debug_shrink_list)

let info =
  let doc =
    "Run the main read-dwarf test suite. The command `make test` run this, the inline test suite \
     and also does some direct read-dwarf call and checks they do not fail"
  in
  Term.(info "rd-tester" ~doc ~exits)

let command = (term, info)

let _ = Term.exit @@ Term.eval command

(* let () = QCheck_base_runner.run_tests_main tests *)
