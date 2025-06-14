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

(** This module is about launching isla as a background server and using it

    Important remark: even if I call isla a server because in a logical sense
    read-dwarf is making request to isla and isla is serving them, in Unix
    sense read-dwarf is the server because it listen to the socket and isla
    connect to it.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module ConfigFile = Config.File
module CommonOpt = Config.CommonOpt
module Config = Config.File.ArchConf.Isla
module Server = Cmd.SocketServer

(** The configuration record type *)
type config = Config.t

(** The raw output of the server for an instruction.

    It is a list of traces, each with a flag telling if they are normal traces (no
    processor exception/fault) or not *)
type trcs = Base.instruction_segments option * (bool * Base.rtrc) list

type opcode = BytesSeq.t * Relocation.t option


(** Bump when updating isla.
    TODO: move the version checking to allow a range of version.
    Also, right now the cache invalidation is based on
    this and not on the actual isla version, which may be dangerous.*)
let required_version = "v0.1.0-113-g711f7b5"

let req_num = ref (-1)

(** This instance of socket server for isla *)
let server = ref None

(** Assume the server is started and get it out of the reference *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Isla server was not started"

(** Compute the isla-client command line from the isla configuration *)
let cmd_of_config (config : config) socket =
  let cmd =
    Vec.of_array
      [|
        !CommonOpt.isla_client_ref;
        "--config";
        config.arch_toml;
        "--socket";
        socket;
        "--arch";
        config.arch_file;
      |]
  in
  List.iter
    (fun s ->
      Vec.add_one cmd "-L";
      Vec.add_one cmd s)
    config.linearize;
  List.iter (fun s -> Vec.add_one cmd s) config.other_opts;
  let cmd = Vec.to_array cmd in
  debug "Isla command line: \n%t" Pp.(topi (array string) cmd);
  cmd

(** Start the server with the specified architecture, do not attempt any checks *)
let raw_start config : unit =
  if !server != None then failwith "Isla server starting when there is already a server online";
  server := Some (Server.start ~name:"isla" (cmd_of_config config))

(** Stop the server by cutting the connection. *)
let raw_stop () =
  match !server with
  | Some serv ->
      Server.stop serv;
      server := None
  | None -> ()

(** This should match exactly with the Answer type in isla-client code *)
type basic_answer = Error | Version of string | StartTraces | Trace of bool * string | EndTraces | Segments of string

(** Read an answer from isla-client.
    This must match exactly [write_answer] in [client.rs] in [isla] *)
let read_basic_answer () =
  let serv = get_server () in
  match Server.read_byte serv with
  | 0 -> Error
  | 1 -> Version (Server.read_string serv)
  | 2 -> StartTraces
  | 3 ->
      let b = Server.read_byte serv = 1 in
      let s = Server.read_string serv in
      Trace (b, s)
  | 4 -> EndTraces
  | 5 -> Segments (Server.read_string serv)
  | _ -> failwith "Unknown isla anwser"

(** The interpreted answer. If the protocol is followed,
    then one request lead to exactly one answer of that type *)
type answer = Version of string | Traces of (string option * (bool * string) list)

(** Expect a version answer and fails if it is not the case *)
let expect_version = function Version s -> s | _ -> failwith "expected version number from isla"

(** Expect isla traces and fails if it is not the case *)
let expect_traces = function Traces tl -> tl | _ -> failwith "expected traces from isla"

(** Expect isla traces and fails if it is not the case, additionally parse them *)
let expect_parsed_traces a : trcs =
  let rsegs, rtrcs = expect_traces a in
  let filename = Printf.sprintf "Isla call %d" !req_num in
  let trcs = List.mapi (fun i (b, t) ->
         ( b,
           let filename = filename ^ Printf.sprintf ", trace %d" i in
           Base.parse_trc_string ~filename t )) rtrcs
  in
  let segs = Option.map (Base.parse_segments_string ~filename) rsegs in
  segs, trcs

(** When isla encounter a non fatal error with that specific request.
    This error is recoverable and the sever can accept other requests *)
exception IslaError

(** Read the answer from isla, block until full answer *)
let read_answer () : answer =
  let rec traces_seq () =
    match read_basic_answer () with
    | EndTraces -> Seq.Nil
    | Trace (bool, s) -> Seq.Cons ((bool, s), traces_seq)
    | Error -> raise IslaError
    | _ -> failwith "isla protocol error: no EndTraces"
  in
  match read_basic_answer () with
  | Error -> raise IslaError
  | Version s -> Version s
  | Segments s -> (
      match read_basic_answer () with
      | StartTraces ->  Traces (Some s, List.of_seq traces_seq)
      | _ -> failwith "segments not followed by traces"
    )
  | StartTraces ->
      Traces (None, List.of_seq traces_seq)
  | _ -> failwith "isla protocol error: Traces element before StartTraces"

(** Answer pretty printer *)
let pp_answer = function
  | Version s -> Pp.(prefix 2 1 !^"isla-client version:" !^s)
  | Traces (s, l) ->
      Pp.(
        optional string s
        ^^ hardline 
        ^^ hardline 
        ^^ (l
            |> List.map (fun (b, t) ->
                let bdoc = if b then !^"norm:" else !^"ex:" in
                prefix 2 1 bdoc (string t))
            |> separate (hardline ^^ hardline)))

(** The type of a request to isla *)
type request = TEXT_ASM of string | ASM of opcode | VERSION | STOP

(** Convert a request into the string message expected by isla-client
    This should match the protocol *)
let string_of_request = function
  | TEXT_ASM s -> Printf.sprintf "execute_asm %s" s
  | ASM b -> Pp.(sprintc @@ !^"execute " ^^ Relocation.pp_opcode_with_segments b)
  | VERSION -> "version"
  | STOP -> "stop"

(** Send a string request to the server, and do not wait for any answer *)
let send_string_request (req : string) : unit =
  let serv = get_server () in
  incr req_num;
  debug "Sending request %s" req;
  Server.write_string serv req

(** Same as {!request} but takes the request directly as a string *)
let string_request (req : string) : answer =
  send_string_request req;
  read_answer ()

(** Send a request and wait for answer *)
let request (req : request) : answer = req |> string_of_request |> string_request

(** Request the traces of a binary instruction and parse the result.

    This is the main entry point of this module.
*)
let request_bin_parsed (opcode : opcode) : trcs = ASM opcode |> request |> expect_parsed_traces

(** Send a request without expecting any answer *)
let send_request req = req |> string_of_request |> send_string_request

(** Stop isla client by sending a stop request *)
let stop () =
  send_request STOP;
  raw_stop ();
  info "Isla stopped"

(** Start isla and check version *)
let start config =
  raw_start config;
  let version = request VERSION |> expect_version in
  if version = required_version then info "Isla started with version %s" version
  else
    warn
      "Isla started with API version %s\n\
      \        but version %s was required.\n\
      \        This may crash the communication protocol in unknown ways" version required_version

(** Test that isla can start and keep a valid version *)

(*$T start
  ConfigFile.(ensure_loaded "../config/config.toml"; get_arch_name () |> get_isla_config |> start; stop (); true)
*)

(** This module provide a CLI subcommand to test isla directly.
    All isla output is reported as raw text *)
module Cmd = struct
  open Cmdliner
  open CmdlinerHelper
  open CommonOpt

  let server_test arch =
    base "Starting";
    Random.self_init ();
    let config = ConfigFile.get_isla_config arch in
    raw_start config;
    try
      while true do
        print_string "> ";
        flush stdout;
        let c = input_line stdin in
        try
          let answer = string_request c in
          Pp.(println @@ pp_answer answer)
        with IslaError -> print_string "Isla sent back an error\n"
      done
    with
    | End_of_file ->
        print_newline ();
        stop ()
    | e ->
        stop ();
        raise e

  let term = Term.(func_options [isla_client; Logs.term; config] server_test $ arch)

  let info =
    let doc =
      "Test the raw isla server. Allow to do manual call to the isla server. The input is \
       un-parsed and transmitted as raw text to isla, however the result is parsed and printed \
       again as the protocol is partially a binary protocol."
    in
    Cmd.(info "isla-server" ~doc ~exits)

  let command = (term, info)
end
