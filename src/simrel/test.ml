(** Temporary exe to test simrel code. *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Simrel.Base

let o0_elf = "src/simrel/O0.elf"

let o2_elf = "src/simrel/O2.elf"

let tree_pair name : tree pair =
  {
    o0 = Run.Func.get_state_tree ~elf:o0_elf ~name ();
    o2 = Run.Func.get_state_tree ~elf:o2_elf ~name ();
  }

let main () =
  let test2 = tree_pair "hyp_get_page_tv_test2" in
  let test2_matched = Test2.infer_rels test2 in
  base "test2: %s" (if check_rel test2_matched then "SAME" else "DIFF");
  let test3 = tree_pair "hyp_get_page_tv_test3" in
  let test3_matched = Test3.infer_rels test3 in
  base "test3: %s" (if check_rel test3_matched then "SAME" else "DIFF")

let () =
  Config.CommonOpt.quick_exe ~name:"simrel-test"
    ~doc:"A small harness for running simulation relation checking code on examples."
    main
