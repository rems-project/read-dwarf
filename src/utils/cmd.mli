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

(**
   This module provides high-level interaction with external processes.

   This provide a functionality similar to
   {{:https://erratique.ch/software/bos/doc/Bos.html}[Bos])}, but this lib is still unstable.

   There are two main mode of communication provided:
    - Interaction with pipes: capture the stdin or stdout of the command and use them
    - Interaction with socket: Create a socket on which the child program can connect to.

   Programs can be launched in two modes
    - Calling: They are called like a function and we wait for them to give a result
      before contnuing
    - Server: The are launch as a background process that stay there and can be called.

   Calling programs can be done with [call*] function like {!call}, {!call_read},
   {!call_send} and {!call_send_read} and only support pipe interaction

   Server like setups can be done in pipe mode with {!IOServer} and in socket mode with
   {!SocketServer}.



*)

(** The type of a command to be sent. The program to call must be the item 0 of the array *)
type cmd = string array

(** If a program do not return with a 0 exit code, we throw that exception giving
    the command that failed and the invalid status it returned *)
exception Crash of cmd * Unix.process_status

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pipe calling } *)

(** Call the command without redirecting anything. Wait for completion before returning.

    May throw {!Crash} on error.*)
val call : cmd -> unit

(** Call the command and then call [sender] to send it some data on it's [stdin].
    Then wait for completion.

    May throw {!Crash} on error.*)
val call_send : cmd -> sender:(out_channel -> unit) -> unit

(** Call the command, send it the string on it's standard input and wait for completion.

    May throw {!Crash} on error.*)
val call_send_string : cmd -> string -> unit

(** Call the command and then call [reader] to parse what the command outputs on it's [stdout].
    Then wait for completion and return the parsed value

    May throw {!Crash} on error.*)
val call_read : cmd -> reader:(in_channel -> 'a) -> 'a

(** Call the command, wait for completion, and return it's [stdout] in a string *)
val call_read_string : cmd -> string

(** Call the command and then call [sender] to send the input data on it's [stdin].
    Then call [reader] to parse an answer from [stdout]
    Then wait for completion and return the parsed value

    May throw {!Crash} on error.*)
val call_send_read : cmd -> sender:(out_channel -> unit) -> reader:(in_channel -> 'a) -> 'a

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pipe server } *)

(** This module provide functionality to run command in the background and communicate
    with it via redirection on it's standard input and output.

    An example of use is in {!Z3} *)
module IOServer : sig
  (** The type of pipe IO server *)
  type t = {
    cmd : cmd;
    input : in_channel;  (** The output of the command from which answer can be read *)
    output : out_channel;  (** The input of the server on which request can be sent *)
  }

  (** Start the server using the specified command *)
  val start : cmd -> t

  (** Stop the server.

      May throw {!Crash} on error.*)
  val stop : t -> unit
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Socket server } *)

(** This module provide functionality for a socket server with which one
    can communicate on a sockets.

    An example of use is in {!Isla.Server} *)
module SocketServer : sig
  (** The abstract type of a socket server *)
  type t

  (** Start the server with provided name and wait for it to connect to the socket.
      Then build the {!Server.t} object.
      The function argument must take a socket name and give a valid command line to call
      the server process and make it connect to the socket.*)
  val start : name:string -> (string -> cmd) -> t

  (** Stop the server and cut the connection, wait for the subprocess to die
      and then delete the socket

      May throw {!Crash} on error.*)
  val stop : t -> unit

  (** Read a single byte from the server *)
  val read_byte : t -> int

  (** Read a string with the following format:

      [| header : 4 bytes | data : header bytes |]

      In other words, read a 4 bytes number, then read that number of bytes into a string *)
  val read_string : t -> string

  (** Write a string in the same binary format as {!read_string} *)
  val write_string : t -> string -> unit
end
