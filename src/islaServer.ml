(** This module is about launching isla as a background server and using it

    Important remark: even if I call isla a server because in a logical sense
    read-dwarf is making request to isla and isla is serving them, in Unix
    sense read-dwarf is the server because it listen to the socket and isla
    connect to it.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module Config = ConfigFile.ArchConf.Isla
module Server = Cmd.SocketServer

(** The configuration record type *)
type config = Config.t

(** The raw output of the server for an instruction.

    It is a list of traces, each with a flag telling if they are normal traces (no
    processor exception/fault) or not *)
type trcs = (bool * Isla.rtrc) list

(** Bump when updating isla. As some point the version checking should move to
    allow a range of version. Also, right know the cache invalidation is base on
    this and not on the actual isla version, which may be dangerous.*)
let required_version = "dev-e6d5ea336532a38c3d196eccac6aa5d7c454f6a1"

let req_num = ref (-1)

(** This instance of socket server for isla *)
let server = ref None

(** Assume the server is started and get it out of the reference *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Isla server was not started"

(** Compute the isla-client command line from the isla configuration *)
let cmd_of_config (config : config) socket =
  let cmd =
    Vec.of_array [|!CommonOpt.isla_client_ref; "--socket"; socket; "--arch"; config.arch_file|]
  in
  List.iter
    (fun s ->
      Vec.add_one cmd "-L";
      Vec.add_one cmd s)
    config.linearize;
  List.iter
    (fun (s, b) ->
      Vec.add_one cmd "-r";
      Vec.add_one cmd (Printf.sprintf "%s=%b" s b))
    config.config_registers;
  List.iter (fun s -> Vec.add_one cmd s) config.other_opts;
  let cmd = Vec.to_array cmd in
  debug "Isla command line: \n%t" PP.(topi (array string) cmd);
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
type basic_answer = Error | Version of string | StartTraces | Trace of bool * string | EndTraces

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
  | _ -> failwith "Unknown isla anwser"

(** The interpreted answer. If the protocol is followed,
    then one request lead to exactly one answer of that type *)
type answer = Version of string | Traces of (bool * string) list

(** Expect a version answer and fails if it is not the case *)
let expect_version = function Version s -> s | _ -> failwith "expected version number from isla"

(** Expect isla traces and fails if it is not the case *)
let expect_traces = function Traces tl -> tl | _ -> failwith "expected traces from isla"

(** Expect isla traces and fails if it is not the case, additionally parse them *)
let expect_parsed_traces a : trcs =
  a |> expect_traces
  |> List.mapi (fun i (b, t) ->
         ( b,
           let filename = Printf.sprintf "Isla call %d, trace %d" !req_num i in
           Isla.parse_trc_string ~filename t ))

(** When isla encounter a non fatal error with that specific request.
    This error is recoverable and the sever can accept other requests *)
exception IslaError

(** Read the answer from isla, block until full answer *)
let read_answer () : answer =
  match read_basic_answer () with
  | Error -> raise IslaError
  | Version s -> Version s
  | StartTraces ->
      let rec seq () =
        match read_basic_answer () with
        | EndTraces -> Seq.Nil
        | Trace (bool, s) -> Seq.Cons ((bool, s), seq)
        | Error -> raise IslaError
        | _ -> failwith "isla protocol error: no EndTraces"
      in
      Traces (List.of_seq seq)
  | _ -> failwith "isla protocol error: Traces element before StartTraces"

(** Answer pretty printer *)
let pp_answer = function
  | Version s -> PP.(prefix 2 1 !^"isla-client version:" !^s)
  | Traces l ->
      l
      |> List.map (fun (b, t) ->
             PP.(
               let bdoc = if b then !^"norm:" else !^"ex:" in
               prefix 2 1 bdoc (string t)))
      |> PP.(separate (hardline ^^ hardline))

(** The type of a request to isla *)
type request = TEXT_ASM of string | ASM of BytesSeq.t | VERSION | STOP

(** Convert a request into the string message expected by isla-client
    This should match the protocol *)
let string_of_request = function
  | TEXT_ASM s -> Printf.sprintf "execute_asm %s" s
  | ASM b -> PP.(sprintc @@ !^"execute " ^^ BytesSeq.ppint b)
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
let request_bin_parsed (bin : BytesSeq.t) : trcs = ASM bin |> request |> expect_parsed_traces

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
let _ =
  Tests.add_test "IslaServer.version" (fun () ->
      start (ConfigFile.get_isla_config (ConfigFile.get_arch_name ()));
      stop ();
      true)

(** This module provide a CLI subcommand to test isla directly.
    All isla output is reported as raw text *)
module Cmd = struct
  open Cmdliner
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
          PP.(println @@ pp_answer answer)
        with IslaError -> print_string "Isla sent back an error\nx "
      done
    with
    | End_of_file ->
        print_newline ();
        stop ()
    | e ->
        stop ();
        raise e

  let term = Term.(func_options [isla_client; logs_term; config] server_test $ arch)

  let info =
    let doc =
      "Test the raw isla server. Allow to do manual call to the isla server. The input is \
       un-parsed and transmitted as raw text to isla, however the result is parsed and printed \
       again as the protocol is partially a binary protocol."
    in
    Term.(info "isla-server" ~doc ~exits)

  let command = (term, info)
end
