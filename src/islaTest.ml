(** This file about testing interaction with Isla mostly for single instructions *)

open Isla
open Files

(* possible options :

  - call isla or not
  - parse or not
  - type or not
  - interpret or not

   type need parse
   interpret need type

   mode will be DUMP, PARSE, TYPE, INTERPRET,
   default is PARSE

   possible inputs methods are :
     RAW
     CMD ISLA from command line
     PIPE isla from pipe request

*)

(* IslaTest use Z3 by default *)

module SMT : Smt.Smt = Z3

(** The type of processing requested  *)
type pmode =
  | DUMP  (** Dump isla output on standard output *)
  | PARSE  (** parse isla output and prettyprint it *)
  | TYPE  (** Type isla output and dump var types. Also dump the deduced register file*)
  | RUN  (** Run the the isla output on a test state and print all branches and states *)
  | SIMP  (** Run the the isla output on a test state and also print a simplified version *)

(** The way isla is called *)
type isla_mode =
  | RAW  (** Do not call isla and take the input test as if it was isla output *)
  | CMD  (** Call isla by setting the request on the command line *)
  | INT  (** Call isla in an interactive session and send the request *)

(** The way input is taken *)
type imode =
  | CMD  (** Read the input as the main command line argument *)
  | FILE  (** Read the input in a file *)

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

let inter =
  let doc = "Call isla via it's interactive piped interface and not via the cli" in
  Arg.(value & flag & info ["i"; "interactive"] ~doc)

let noparse =
  let doc = "Do not try to parse isla output. Just dump it" in
  Arg.(value & flag & info ["n"; "no-parse"] ~doc)

let typer =
  let doc = "Type the isla output, dump the types and the register file" in
  Arg.(value & flag & info ["t"; "type"] ~doc)

let run =
  let doc = "Run the isla output on some state and print the result" in
  Arg.(value & flag & info ["r"; "run"] ~doc)

let simp =
  let doc = "Run the isla output on some state and simplify the result" in
  Arg.(value & flag & info ["s"; "simplify"] ~doc)

let file =
  let doc = "Add to interpret main argument as a file instead of raw text" in
  Arg.(value & flag & info ["f"; "file"] ~doc)

(** Input flag to mode conversion *)
let input_f2m file : imode = if file then FILE else CMD

let imode_term = Term.(const input_f2m $ file)

(** Input takes the imode and the main argument and returns the filename and input string *)
let input imode (arg : string) : (string * string) Term.ret =
  match imode with
  | CMD -> `Ok ("CLI input", arg)
  | FILE -> (
      try `Ok (arg, read_file arg) with e -> `Error (false, Printexc.to_string e)
    )

let input_term = Term.(ret (const input $ imode_term $ instr))

(** Convert various flag describe the mode of operation into the mode of operation *)
let isla_f2m direct inter : isla_mode Term.ret =
  match (direct, inter) with
  | (false, false) -> `Ok CMD
  | (true, false) -> `Ok RAW
  | (false, true) -> `Ok INT
  | _ -> `Error (false, "You cannot use -d/--direct and --i/--inter at the same time")

let isla_mode_term = Term.(ret (const isla_f2m $ direct $ inter))

let isla_run isla_mode arch (filename, input) : string * string =
  match isla_mode with
  | RAW -> (filename, input)
  | CMD ->
      (filename ^ " through isla", isla_cmd [|""; "-a"; arch; "-i"; input; "-t"; "1"|] read_all)
  | INT -> raise @@ Failure "Interactive isla interaction is not yet implemented"

let isla_term = Term.(const isla_run $ isla_mode_term $ arch $ input_term)

let processing_f2m noparse typer run simp =
  if simp then SIMP
  else if run then RUN
  else if typer then TYPE
  else if noparse then DUMP
  else PARSE

let pmode_term = Term.(const processing_f2m $ noparse $ typer $ run $ simp)

let processing pmode (filename, input) : unit =
  let parse input =
    let t = Isla.parse_term_string filename input in
    PPA.(println @@ pp_term string t);
    t
  in
  let typer ast =
    match ast with
    | Traces [t] ->
        let c = IslaType.type_regs t in
        PPA.(println @@ tcontext c);
        PPA.(println @@ rstruct Reg.index);
        t
    | _ -> Warn.fatal0 "To use -t option, the trace must be linear (for now)"
  in
  let run trace =
    let istate = State.make () in
    PPA.(println @@ state istate);
    let estate = IslaTrace.run_lin_trace istate trace in
    PPA.(println @@ state estate);
    estate
  in
  match pmode with
  | DUMP -> input |> print_endline
  | PARSE -> input |> parse |> ignore
  | TYPE -> input |> parse |> typer |> ignore
  | RUN ->
      input |> parse |> typer
      |> IslaManip.trace_conv_var (fun _ -> failwith "hey")
      |> run |> ignore
  | SIMP ->
      input |> parse |> typer
      |> IslaManip.trace_conv_var (fun _ -> failwith "hey")
      |> run |> State.map_exp SMT.simplify |> PPA.state |> PPA.println

let term = Term.(const processing $ pmode_term $ isla_term)

let info =
  let doc = "Test the isla interaction" in
  Term.(info "isla-test" ~doc ~exits:default_exits)

let command = (term, info)
