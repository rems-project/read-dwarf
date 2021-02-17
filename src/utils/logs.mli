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
