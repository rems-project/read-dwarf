(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This file about testing interaction with Isla for single instructions *)

open Files

open Logs.Logger (struct
  let str = __MODULE__
end)

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

(* Test use Z3 by default *)

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
open Config.CommonOpt

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
      let (sym, off) =
        try Elf.SymTable.of_position_string elf.symbols s
        with Not_found -> fail "The position %s could not be found in %s" s arg
      in
      `Ok (filename, BytesSeq.to_string (BytesSeq.sub sym.data.data off 4)) (* TODO relocations *)

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

let isla_mode_term =
  Term.(ret (CmdlinerHelper.func_option Logs.term isla_f2m $ direct $ hex $ bin $ sym))

let isla_mode_to_request imode input =
  match imode with
  | ASM -> Server.TEXT_ASM input
  | HEX -> Server.ASM (BytesSeq.of_hex input, None) (* TODO? *)
  | BIN -> Server.ASM (BytesSeq.of_string input, None)
  | _ -> assert false

(** Run isla and return a text trace with a filename
    (if mode is RAW than just return the trace and filename without isla)

    If isla return multiple traces, just silently pick the first non-exceptional one *)
let isla_run isla_mode arch (filename, input) : string * string * Server.config =
  match isla_mode with
  | RAW -> (filename, input, Config.File.get_isla_config arch)
  | _ ->
      Server.(
        Random.self_init ();
        let config = ConfigFile.get_isla_config arch in
        start config;
        let msg : string =
          match request (isla_mode_to_request isla_mode input) with
          | Traces (_, l) -> List.assoc true l (* TODO segments *)
          | _ -> failwith "isla did not send back traces"
        in
        stop ();
        (filename ^ " through isla-client", msg, config))

let isla_term =
  Term.(CmdlinerHelper.func_option isla_client isla_run $ isla_mode_term $ arch $ input_term)

(** How far into the processing pipeline we go. We just pick the deepest option chosen *)
let processing_f2m noparse typer run simp =
  if simp then SIMP
  else if run then RUN
  else if typer then TYPE
  else if noparse then DUMP
  else PARSE

let pmode_term = Term.(const processing_f2m $ noparse $ typer $ run $ simp)

(** Does the actual processing of the trace *)
let processing preprocessing pmode (filename, input, (config : Server.config)) : unit =
  let parse input =
    let t = Base.parse_trc_string ~filename input in
    let t = Manip.remove_ignored config.ignored_regs t in
    if preprocessing then begin
      let pre = Preprocess.simplify_trc t in
      base "Preprocessed trace:\n%t\n" (Pp.topi Base.pp_trc pre);
      pre
    end
    else begin
      base "Trace:\n%t\n" (Pp.topi Base.pp_trc t);
      t
    end
  in
  let typer t =
    let c = Type.type_trc t in
    base "Isla vars typing context:\n%t\n" (Pp.topi Type.pp_tcontext c);
    base "Register types:\n%t\n" (Pp.topi State.Reg.pp_index ());
    t
  in
  let run trace =
    let init_state = State.make () in
    State.lock init_state;
    base "Initial state:\n%t\n" (Pp.topi State.pp init_state);
    let end_state = (Run.trc [@ocaml.warning "-3"] (* deprecated *)) init_state trace in
    base "Final state:\n%t\n" (Pp.topi State.pp end_state);
    end_state
  in
  let simp state =
    Z3.start ();
    (State.unsafe_unlock [@ocaml.warning "-3"] (* deprecated *)) state;
    State.Simplify.ctxfull state;
    State.lock state;
    base "Simplified state:\n%t\n" (Pp.topi State.pp state);
    Z3.stop ();
    state
  in
  match pmode with
  | DUMP -> input |> print_endline
  | PARSE -> input |> parse |> ignore
  | TYPE -> input |> parse |> typer |> ignore
  | RUN -> input |> parse |> typer |> run |> ignore
  | SIMP -> input |> parse |> typer |> run |> simp |> ignore

let term = Term.(CmdlinerHelper.func_option z3 processing $ preprocess $ pmode_term $ isla_term)

let info =
  let doc =
    "Test the isla pipeline. Allow to test all the elements of the pipeline individually. In \
     particular it allows to parse an isla trace text from disk and other similar low-level \
     testing operation. It completely ignores the Cache."
  in
  Cmd.(info "isla-test" ~doc ~exits)

let command = (term, info)
