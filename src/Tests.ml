(** This module define a unit-test system
    you can then invoke the subcommand unit with a test name to run a test or

*)

open Cmdliner
open CommonOpt

let arch_ref = ref "aarch64.ir"

let enable_tests = true

(** This type of function can be registered to be called between tests *)
type reset = unit -> unit

let resets : (string, reset) Hashtbl.t = Hashtbl.create 10

let add_reset name f = if enable_tests then Hashtbl.add resets name f

let reset () =
  Hashtbl.iter
    (fun name f ->
      try f ()
      with e -> Warn.fatal2 "Reset function %s has thrown: %s\n" name (Printexc.to_string e))
    resets

(** Test failure is either when it returns false or throws *)
type test = unit -> bool

(** Global test table *)
let tests : (string, test) Hashtbl.t = Hashtbl.create 10

let add_test name f = if enable_tests then Hashtbl.add tests name f

let success = ref true

let run_test name (f : test) =
  print_string ("Running test " ^ name ^ ": ");
  flush stdout;
  reset ();
  try
    if f () then print_endline "Success"
    else begin
      print_endline "Failure";
      success := false
    end
  with e ->
    Printf.printf "Thrown: %s\n" (Printexc.to_string e);
    flush stdout;
    success := false

let run_test_name name =
  try run_test name (Hashtbl.find tests name)
  with Not_found -> Warn.fatal "Test %s doesn't exist\n" name

let run_tests testl =
  if testl = [] then Hashtbl.iter run_test tests else List.iter run_test_name testl

let tests_arg =
  let doc = "Test names to be run. if no test is supplied, run all the tests" in
  Arg.(value & pos_all string [] & info [] ~docv:"TESTS" ~doc)

let test_cmd tests =
  run_tests tests;
  if !success then begin
    print_endline "All tests passed";
    Ok ()
  end
  else Error (`Msg "Some test failed")

let test_term = Term.(func_options (setter arch_ref arch :: comopts) test_cmd $ tests_arg)

let term = Term.(term_result test_term)

let info =
  let doc = "Run the unit tests" in
  Term.(info "test" ~doc ~exits)

let command = (term, info)

let true_test () = true

let _ = add_test "Tests.true_test" true_test
