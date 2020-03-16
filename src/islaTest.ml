(** This file about testing interaction with Isla mostly for single instructions *)

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

module SMT = Z3

(** The type of processing requested  *)
type pmode =
  | DUMP  (** Dump isla output on standard output *)
  | PARSE  (** parse isla output and prettyprint it *)
  | TYPE  (** Type isla output and dump var types. Also dump the deduced register file*)
  | RUN  (** Run the the isla output on a test state and print all branches and states *)
  | SIMP  (** Run the the isla output on a test state and also print a simplified version *)

(** The input syntax *)
type isla_mode =
  | RAW  (** Do not call isla and take the input test as if it was isla output *)
  | ASM  (** Call isla with text assembly *)
  | HEX  (** Call isla with text hexadecimal as binary encoding of instruction *)
  | BIN  (** Call isla with actually binary (for now 4 bytes) *)

(** The way input is taken *)
type imode =
  | CMD  (** Read the input as the main command line argument *)
  | FILE  (** Read the input in a file *)
  | ELF of string  (** Read the input from an elf symbol + offset. implies BIN for isla_mode *)

open Cmdliner
open CommonOpt

let arg =
  let doc = "May argument, may be a text instruction, an isla dump or a file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTR_OR_FILE" ~doc)

let direct =
  let doc = "Input direct isla output syntax and bypass isla" in
  Arg.(value & flag & info ["d"; "direct"] ~doc)

let bin =
  let doc = "Supply a instruction in raw binary form (should probably get it from file or elf)" in
  Arg.(value & flag & info ["b"; "bin"] ~doc)

let hex =
  let doc = "Supply a instruction in hexadecimal. Do not put Ox in front" in
  Arg.(value & flag & info ["h"; "hex"] ~doc)

let noparse =
  let doc = "Do not try to parse isla output. Just dump it" in
  Arg.(value & flag & info ["n"; "no-parse"] ~doc)

let preprocess =
  let doc = "Preprocess the trace before doing the rest" in
  Arg.(value & flag & info ["p"; "preprocess"] ~doc)

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

let sym =
  let doc = "The main argument would be an elf file and you should supply a symbol like main+4" in
  Arg.(value & opt (some string) None & info ["sym"] ~doc)

(** Input flag to mode conversion *)
let input_f2m file sym : imode Term.ret =
  match (file, sym) with
  | (true, None) -> `Ok FILE
  | (false, Some s) -> `Ok (ELF s)
  | (false, None) -> `Ok CMD
  | _ -> `Error (false, "You can't supply -f/--file and --sym at the same time")

let imode_term = Term.(ret (const input_f2m $ file $ sym))

(** Input takes the imode and the main argument and returns the filename and input string *)
let input imode (arg : string) : (string * string) Term.ret =
  match imode with
  | CMD -> `Ok ("CLI input", arg)
  | FILE -> (
      try `Ok (arg, read_string arg) with e -> `Error (false, Printexc.to_string e)
    )
  | ELF s ->
      let filename = s ^ " in " ^ arg in
      let elf = Elf.File.of_file arg in
      let (sym, off) = Elf.SymTbl.sym_offset_of_string elf.symbols s in
      `Ok (filename, BytesSeq.to_string (BytesSeq.sub sym.data off 4))

let input_term = Term.(ret (const input $ imode_term $ arg))

(** Convert various flag describe the mode of operation into the mode of operation
    If sym is activated, then the default mode is BIN and not ASM *)
let isla_f2m direct hex bin sym : isla_mode Term.ret =
  match (direct, hex, bin) with
  | (false, false, false) -> if sym = None then `Ok ASM else `Ok BIN
  | (true, false, false) -> `Ok RAW
  | (false, true, false) -> `Ok HEX
  | (false, false, true) -> `Ok BIN
  | _ -> `Error (false, "You cannot use -d/--direct, -b/--bin or -h/--hex at the same time")

let isla_mode_term = Term.(ret (func_option logs_term isla_f2m $ direct $ hex $ bin $ sym))

let isla_mode_to_request imode input =
  match imode with
  | ASM -> IslaServer.TEXT_ASM input
  | HEX -> IslaServer.ASM (BytesSeq.of_hex input)
  | BIN -> IslaServer.ASM (BytesSeq.of_string input)
  | _ -> assert false

(** Run isla and return a text trace with a filename
    (if mode is RAW than just return the trace and filename without isla)

    If isla return multiple traces, just silently pick the first non-exceptional one *)
let isla_run isla_mode arch (filename, input) : string * string =
  match isla_mode with
  | RAW -> (filename, input)
  | _ ->
      IslaServer.(
        Random.self_init ();
        start arch;
        let msg : string =
          match request (isla_mode_to_request isla_mode input) with
          | Traces l -> List.assoc true l
          | _ -> failwith "isla did not send back traces"
        in
        stop ();
        (filename ^ " through isla-client", msg))

let isla_term = Term.(func_option isla_client isla_run $ isla_mode_term $ arch $ input_term)

(** How far into the processing pipeline we go. We just pick the deepest option chosen *)
let processing_f2m noparse typer run simp =
  if simp then SIMP
  else if run then RUN
  else if typer then TYPE
  else if noparse then DUMP
  else PARSE

let pmode_term = Term.(const processing_f2m $ noparse $ typer $ run $ simp)

(** Does the actual processing of the trace *)
let processing preprocessing pmode (filename, input) : unit =
  let parse input =
    let t = Isla.parse_trc_string ~filename input in
    let t = IslaManip.remove_ignored t in
    if preprocessing then begin
      let pre = IslaPreprocess.simplify_trc t in
      PPA.(println @@ pp_trc string pre);
      pre
    end
    else begin
      PPA.(println @@ pp_trc string t);
      t
    end
  in
  let typer t =
    let c = IslaType.type_trc t in
    PPA.(println @@ tcontext c);
    PPA.(println @@ rstruct Reg.index);
    t
  in
  let run trace =
    let init_state = State.make () in
    State.lock init_state;
    PPA.(println @@ state init_state);
    let end_state = IslaTrace.run_trc init_state trace in
    PPA.(println @@ state end_state);
    end_state
  in
  let simp state =
    Z3.start ();
    State.unsafe_unlock state;
    State.map_mut_exp SMT.simplify state;
    State.lock state;
    Z3.stop ();
    state
  in
  match pmode with
  | DUMP -> input |> print_endline
  | PARSE -> input |> parse |> ignore
  | TYPE -> input |> parse |> typer |> ignore
  | RUN -> input |> parse |> typer |> IslaManip.isla_trace_conv_svar |> run |> ignore
  | SIMP ->
      input |> parse |> typer |> IslaManip.isla_trace_conv_svar |> run |> simp |> PPA.state
      |> PPA.println

let term = Term.(func_option z3 processing $ preprocess $ pmode_term $ isla_term)

let info =
  let doc = "Test the isla interaction" in
  Term.(info "isla-test" ~doc ~exits)

let command = (term, info)
