(* This file about testing interaction with Isla *)

open Isla
open Files

(** the type of test we want to run *)
type mode = TEXTASM | ISLAOUTPUT

(** Test parsing an isla output *)
let test_parse (s : string) =
  let t = Isla_lang_parser.term_start Isla_lang_lexer.token @@ Lexing.from_string s in
  print_endline "Term parsed";
  PP.(println @@ Isla_lang_parser_pp.pp_term t)

(** main isla-test command *)
let isla_test mode arch instr =
  match mode with
  | TEXTASM -> isla_cmd [|""; "-a"; arch; "-i"; instr; "-t"; "1"|]
  | ISLAOUTPUT -> test_parse instr

open Cmdliner

let arch =
  let doc = "Overrides the default architecture to use in isla" in
  let env = Arg.env_var "ISLA_ARCH" ~doc in
  let doc = "Architecture to be analysed" in
  Arg.(value & opt non_dir_file "aarch64.ir" & info ["a"; "arch"] ~env ~docv:"ARCH_IR" ~doc)

let instr =
  let doc = "Instruction to be analysed (or other things depending on options)" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTR" ~doc)

let direct =
  let doc = "Input direct isla output syntax instead of an instruction" in
  Arg.(value & flag & info ["d"; "direct"] ~doc)

let file =
  let doc = "Add to interpret main argument as a file instead of raw text" in
  Arg.(value & flag & info ["f"; "file"] ~doc)

let optional_file isfile text =
  if isfile then try `Ok (read_file text) with e -> `Error (false, Printexc.to_string e)
  else `Ok text

let instr_term = Term.(ret (const optional_file $ file $ instr))

let flag_to_mode direct = match direct with true -> ISLAOUTPUT | false -> TEXTASM

let info =
  let doc = "Test the isla interaction" in
  Term.(info "isla-test" ~doc ~exits:default_exits)

let term = Term.(const isla_test $ (const flag_to_mode $ direct) $ arch $ instr_term)

let command = (term, info)
