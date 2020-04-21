(** This module is about launching isla as a background server and using it

    Important remark: even if I call isla a server because in a logical sense
    read-dwarf is making request to isla and isla is serving them, in Unix
    sense read-dwarf is the server because it listen to the socket and isla
    connect to it.
*)

open Logs.Logger (struct
  let str = "IslaServer"
end)

(** Bump when updating isla *)
let required_version = "dev-9f2194634a96f4046ebbae8ae1c8b30c8c41ab35"

let req_num = ref (-1)

let server = ref None

(** Assume the server is started and get it out of the reference *)
let get_server () =
  match !server with Some serv -> serv | None -> failwith "Isla server was not started"

(** Start the server with the specified architecture, do not attempt any checks *)
let raw_start arch : unit =
  if !server != None then failwith "Isla server starting when there is already a server online";
  server :=
    Some
      (Cmd.Server.start "isla" (fun socket ->
           [|!CommonOpt.isla_client_ref; "--socket"; socket; "--arch"; arch|]))

(** Stop the server by cutting the connection. *)
let raw_stop () =
  match !server with
  | Some serv ->
      Cmd.Server.stop serv;
      server := None
  | None -> ()

(** This should match exactly with the Answer type in isla-client code *)
type basic_answer = Error | Version of string | StartTraces | Trace of bool * string | EndTraces

(** Read an answer from isla-client. must match exactly write_answer in client.rs *)
let read_basic_answer () =
  let serv = get_server () in
  match Cmd.Server.read_byte serv with
  | 0 -> Error
  | 1 -> Version (Cmd.Server.read_string serv)
  | 2 -> StartTraces
  | 3 ->
      let b = Cmd.Server.read_byte serv = 1 in
      let s = Cmd.Server.read_string serv in
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
let expect_parsed_traces a =
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
  | ASM b ->
      assert (BytesSeq.length b = 4);
      PPI.(sprintc @@ !^"execute " ^^ byteseq32le b)
  | VERSION -> "version"
  | STOP -> "stop"

let send_string_request (req : string) : unit =
  let serv = get_server () in
  req_num := !req_num + 1;
  Cmd.Server.write_string serv req

let string_request (req : string) : answer =
  send_string_request req;
  read_answer ()

(** Send a request and wait for answer *)
let request (req : request) : answer = req |> string_of_request |> string_request

(** Request the traces of a binary instruction and parse the result.

    This is the main entry point of this module.
*)
let request_bin_parsed (bin : BytesSeq.t) : (bool * Isla.rtrc) list =
  ASM bin |> request |> expect_parsed_traces

let send_request req = req |> string_of_request |> send_string_request

(** Stop isla client by sending a stop request *)
let stop () =
  send_request STOP;
  raw_stop ();
  info "Isla stopped"

(** Start isla and check version *)
let start arch =
  raw_start arch;
  let version = request VERSION |> expect_version in
  if version = required_version then info "Isla started with version %s" version
  else begin
    stop ();
    fatal "Isla started with API version %s but version %s was required" version required_version
  end

(** Test that isla can start and keep a valid version *)
let _ =
  Tests.add_test "IslaServer.version" (fun () ->
      start "aarch64.ir";
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
    raw_start arch;
    try
      while true do
        print_string "> ";
        flush stdout;
        let c = input_line stdin in
        try
          let answer = string_request c in
          PP.(print $ pp_answer answer ^^ hardline)
        with IslaError -> print_string "Isla send back an error\nx "
      done
    with
    | End_of_file ->
        print_newline ();
        stop ()
    | e ->
        stop ();
        raise e

  let term = Term.(func_options [isla_client; logs_term] server_test $ arch)

  let info =
    let doc = "Raw isla server interaction." in
    Term.(info "isla-server" ~doc ~exits)

  let command = (term, info)
end
