(** This module allow to do a test run of all the machinery for a single basic block
    For now, without branch at the end
*)

open Cmdliner
open CommonOpt

module SMT : Smt.Smt = Z3

type state = State.t

(* TODO control flow with lazyness *)
let run_code trcs typ (run : bool) simp (code : BytesSeq.t) : unit =
  if trcs || typ || run || simp then begin
    let bb = BB.from_binary code in
    (if trcs then PP.(println $ BB.pp bb));
    if typ || run || simp then begin
      BB.type_regs bb;
      (if typ then PP.(println $ Reg.pp_rstruct Reg.index));
      if run || simp then begin
        let istate = Init.state () in
        let state = State.copy istate in
        Printf.printf "Starting with state %d\n" istate.id;
        BB.run_mut state bb;
        (if run then PP.(println $ State.pp state));
        if simp then State.map_mut_exp SMT.simplify state;
        PP.(println $ State.pp state)
      end
    end
  end

let run_bb arch trcs typ run simp elfname sym len =
  let elf = Elf.File.of_file elfname in
  try
    let (sym, off) = Elf.SymTbl.sym_offset_of_string elf.symbols sym in
    let len = match len with Some i -> i | None -> sym.size - off in
    let code = Elf.Sym.sub sym off len in
    Random.self_init ();
    IslaServer.start arch;
    Init.init ();
    run_code trcs typ run simp code;
    flush stdout;
    IslaServer.stop ()
  with Not_found -> Warn.fatal2 "The symbol %s was not found in %s\n" sym elfname

let trcs =
  let doc = "Print the isla traces of the basic block" in
  Arg.(value & flag & info ["i"; "isla-trc"] ~doc)

let typ =
  let doc = "Print the register types after the basic block" in
  Arg.(value & flag & info ["t"; "type"] ~doc)

let run =
  let doc = "Run the basic block and print the end state" in
  Arg.(value & flag & info ["r"; "run"] ~doc)

let simp =
  let doc = "Print a simplified end state" in
  Arg.(value & flag & info ["s"; "simplify"] ~doc)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let sym =
  let doc = "The symbol to analyse, \"test\" by default. an offset can be added like sym+8" in
  Arg.(value & opt string "test" & info ["sym"] ~doc)

let len =
  let doc =
    "The length in byte of the binary block to consider, "
    ^ "if unspecified, will run until the end of the symbol"
  in
  Arg.(value & opt (some int) None & info ["l"; "len"] ~doc)

let term = Term.(func_options comopts run_bb $ arch $ trcs $ typ $ run $ simp $ elf $ sym $ len)

let info =
  let doc = "Test operations on a single basic block" in
  Term.(info "run-bb" ~doc ~exits)

let command = (term, info)
