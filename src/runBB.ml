(** This module allow to do a test run of all the machinery for a single basic block
    For now, without branch at the end
*)

open Cmdliner
open CommonOpt
module SMT = Z3

open Logs.Logger (struct
  let str = "RunBB"
end)

type state = State.t

let dump =
  let doc = "Dump the trace representation of the basic block" in
  Arg.(value & flag & info ["d"; "dump"] ~doc)

let reg_types =
  let doc = "Print the register types used by the basic block (and base set)" in
  Arg.(value & flag & info ["t"; "types"] ~doc)

let no_run =
  let doc =
    "Do not run the basic block (useful to only get pre-run data like --dump-trace or --type)"
  in
  Arg.(value & flag & info ["n"; "no-run"] ~doc)

let simp_trace =
  let doc = "Simplify the traces before running" in
  Arg.(value & flag & info ["simp-trace"] ~doc)

let simp_state =
  let doc = "Simplify the final state" in
  Arg.(value & flag & info ["simp-state"] ~doc)

let simp =
  let doc = "Implies both --simp-state and --simp-trace" in
  Arg.(value & flag & info ["s"; "simp"] ~doc)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let sym =
  let doc = "The symbol to analyse, \"test\" by default. An offset can be added like sym+8" in
  Arg.(value & opt string "test" & info ["sym"] ~doc)

let len =
  let doc =
    "The length in byte of the binary block to consider, "
    ^ "if unspecified, will run until the end of the symbol"
  in
  Arg.(value & opt (some int) None & info ["l"; "len"] ~doc)

let get_code elfname symname len : BytesSeq.t =
  let elf = Elf.File.of_file elfname in
  Elf.File.load_arch elf;
  let (sym, off) =
    try Elf.SymTbl.of_position_string elf.symbols symname
    with Not_found -> fail "The symbol %s cannot found in %s" symname elfname
  in
  let len = match len with Some i -> i | None -> sym.size - off in
  Elf.Sym.sub sym off len

let code_term = Term.(func_options comopts get_code $ elf $ sym $ len)

let simp_trace_term = Term.(const ( || ) $ simp_trace $ simp)

let simp_state_term = Term.(const ( || ) $ simp_state $ simp)

let get_bb arch dump reg_types simp_trace code : BB.t =
  IslaCache.start arch;
  Init.init ();
  let bb = BB.from_binary code in
  if reg_types then base "Register types:\n%t\n" (PP.topi Reg.pp_rstruct Reg.index);
  if simp_trace then begin
    Z3.ensure_started ();
    BB.simplify_mut bb
  end;
  if dump then base "Basic block:\n%t\n" (PP.topi BB.pp bb);
  bb

let bb_term = Term.(const get_bb $ arch $ dump $ reg_types $ simp_trace_term $ code_term)

let run_bb norun simp_state bb =
  if not norun then begin
    let init_state = Init.state () in
    let state = BB.run init_state bb in
    if simp_state then begin
      Z3.ensure_started ();
      State.unsafe_unlock state;
      State.map_mut_exp Z3.simplify state;
      State.lock state
    end;
    base "Final state:\n%t\n" (PP.topi State.pp state)
  end;
  IslaCache.stop ();
  Z3.ensure_stopped ()

let term = Term.(const run_bb $ no_run $ simp_state_term $ bb_term)

let info =
  let doc = "Test operations on a single basic block" in
  Term.(info "run-bb" ~doc ~exits)

let command = (term, info)
