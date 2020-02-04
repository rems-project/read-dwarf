(** This module is about calling external processes, and helper functions for doing that *)

open Protect
open Unix

type cmd = string array

(** Thrown when a call to cmd resulted in non-zero exit status *)
exception Crash of cmd * process_status

let _ =
  Printexc.register_printer (function
    | Crash (cmd, code) ->
        Some PP.(sprint @@ array string cmd ^^ !^" exited with " ^^ statusi code)
    | _ -> None)

(** Check the return status and throw Chrash if input is noe WEXITED 0 *)
let check_status cmd = function
  | WEXITED code when code = 0 -> ()
  | WSIGNALED code when code < 0 -> Printf.eprintf "Signal %d should not exist: ????\n" code
  | code -> raise (Crash (cmd, code))

(** Close input channel for channel openned with open_process_in.
    Throw Crash if the process did not return 0 *)
let closing_in channel cmd () = check_status cmd @@ close_process_in channel

(** Close input channel for channel openned with open_process_in.
    Throw Crash if the process did not return 0 *)
let closing channels cmd () = check_status cmd @@ close_process channels

(** Call the `which` shell command to get a executable position from its name.
    May throw Crash if the command fails*)
let which arg =
  let channel = open_process_in @@ "which " ^ arg in
  protect (fun () -> input_line channel) @@ closing_in channel [|"which"; arg|]

(** Expand an executable path using which if it's not already a full path *)
let get_full_path (s : string) : string =
  match s.[0] with
  | '/' | '.' -> s
  | _ -> (
      try (* TODO add warning here *)
          which s with _ -> s
    )

(*****************************************************************************)
(*        Pipe input                                                         *)
(*****************************************************************************)

(** Call cmd and then call cont on the output pipe *)
let input (cmd : cmd) (cont : in_channel -> 'a) : 'a =
  cmd.(0) <- get_full_path cmd.(0);
  let output = open_process_args_in cmd.(0) cmd in
  protect (fun () -> cont output) @@ closing_in output cmd

let input_string (cmd : string array) : string = input cmd Files.read_all

let input_exec exec cmd cont =
  cmd.(0) <- exec;
  input cmd cont

(*****************************************************************************)
(*        Pipe input-output                                                  *)
(*****************************************************************************)

let io (cmd : cmd) (out_cont : out_channel -> unit) (in_cont : in_channel -> 'a) : 'a =
  cmd.(0) <- get_full_path cmd.(0);
  (* PP.(println @@ array string cmd); *)
  let output, input = open_process_args cmd.(0) cmd in
  let process () =
    out_cont input;
    flush input;
    in_cont output
  in
  protect process @@ closing (output, input) cmd

let io_test_cat () =
  let output oc = Printf.fprintf oc "test string\n" in
  let input ic = input_line ic in
  let a = io [|"cat"|] output input in
  a = "test string"

let _ = Tests.add_test "Cmd.io.cat" io_test_cat
