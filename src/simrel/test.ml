(** Temporary exe to test simrel code. *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Simrel.Base

let o0_elf = "vm-stuff/pkvm-O0/el2.elf"

let o2_elf = "vm-stuff/pkvm-O2/el2.elf"

let tree_pair name : tree pair =
  {
    o0 = Run.Func.get_state_tree ~elf:o0_elf ~name ();
    o2 = Run.Func.get_state_tree ~elf:o2_elf ~name ();
  }

let main () =
  let test2 = tree_pair "hyp_get_page_tv_test2" in
  let test2_matched = match_up test2 in
  base "%s" (if Test2.related test2_matched then "SAME" else "DIFF");
  ignore test2_matched

let () =
  Config.CommonOpt.quick_exe ~name:"simrel-test"
    ~doc:"A small harness for running simulation relation checking code on examples."
    main
