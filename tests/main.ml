open OUnit2

(* module Q = QCheck;;let ( ==> ) = Q.( ==> );; *)

let dummy_test ctxt = assert_equal ~ctxt 0 0

let dummy_test_fail ctxt = assert_equal ~ctxt ~printer:string_of_int 0 1

let suite =
  "Dummy tests"
  >::: [
         "Dummy test" >:: dummy_test;
         (* "Dummy test fail" >:: dummy_test_fail; *)
         "Dummy test 2" >:: dummy_test;
       ]

let () = print_endline "\nRunning main test suite"

let () = run_test_tt_main suite
