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

(** This module provide a logging system for each module.

    The logging system is intended to print on [stderr] or [stdout] (and maybe in file later)
    messages of the form:

    {v [Module][Level] Message here v}

    For example:

    {v [Z3][Info] Z3 server started with version 4.8.7 v}

    If you just want to use it to log things in your own module, just insert
    at the start of the module the following code:

    {[
    open Logs.Logger (struct
      let str = __MODULE__
    end)
    ]}

    You can then use all the function in {!Logger} to print logging messages in your module.

    The rest of this module is about manipulation log {!level} in the different modules.
    You can dynamically interact with the logs level from the command line
    by using {!section:CommonOpt.logs} *)

(** The type of normal logging function *)
type 'a printf = ('a, out_channel, unit) format -> 'a

(** The type of a failing logging function, that will exit after printing it's message *)
type ('a, 'b) printf_fatal = ('a, out_channel, unit, 'b) format4 -> 'a

(** The type of log level *)
type level =
  | Base
      (** The actual output. The only thing printed in quiet mode.
          Should only appear in {!CLI} modules *)
  | Err  (** An error message *)
  | Warn  (** An warning *)
  | Info
      (** Details on what happens that should be understandable if the person
          do not know the corresponding module *)
  | Debug  (** Everything happening in detail *)

(** Pretty prints a level *)
val pp_level : level -> Pp.document

(** Convert level to string *)
val level_to_string : level -> string

(** Convert level to string header like ["\[Error\]"]. {!Base} is the empty string *)
val level_to_header : level -> string

(** Set a default level to all the modules. Erase local customized levels *)
val set_default_level : level -> unit

(** Set level of a module by name *)
val set_level : string -> level -> unit

(** Parser for log level on command line *)
val level_conv : level Cmdliner.Arg.conv

(** Set level below which the output goes to stdout *)
val set_stdout_level : level -> unit

module type String = sig
  val str : string
end

(** This declare a logger instance. The string parameter is the logger name.
    It should be the OCaml module name. The normal way of instantiating this
    module is:

    {[
    open Logs.Logger (struct
      let str = __MODULE__
    end)
    ]}

    This module provide a lot of logging function, They only provide a
    format string interface but with {!Pp.top} on may print efficiently but
    lazily arbitrary {!Pp.document} to the logs.

    Some examples:

    {[ warn "This weird thing happened, I choose that default behavior for case %s" case ]}

    {[ debug "Function ... received value %t" Pp.(top printer object) ]} *)
module Logger (S : String) : sig
  (** Override the level of this logger. It may still be overridden by the command line *)
  val set_level : level -> unit

  (** Get the current level *)
  val get_level : unit -> level

  (** Log a specific level using a format string *)
  val log : level -> 'a printf

  (** Log a fatal problem with format string then shutdown with [code] *)
  val log_fatal : code:int -> level -> ('a, 'b) printf_fatal

  (** Base command line output asked for by the CLI. Level is {!Base} *)
  val base : 'a printf

  (** Failure due to external circumstances, generally wrong user input, then exit with code 1.
      Level is {!Base} *)
  val fail : ('a, 'b) printf_fatal

  (** Declare a non-fatal internal error. Level is {!Err} *)
  val err : 'a printf

  (** Declare a fatal internal error then exit with code 2. Level is {!Err} *)
  val fatal : ('a, 'b) printf_fatal

  (** Raise a warning. Level is {!Warn} *)
  val warn : 'a printf

  (** Print general information about what's happening. Level is {!Info} *)
  val info : 'a printf

  (** Print debugging information about what's happening. Level is {!Debug} *)
  val debug : 'a printf

  (** Tell if debug logging is enabled *)
  val has_debug : unit -> bool
end

val process_opts : bool -> bool list -> string list -> string list -> level -> unit

val term : unit Cmdliner.Term.t
