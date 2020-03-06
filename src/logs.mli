(** This module provide a logging system. To create a logging module use functor {!Logger} *)

type ldoc = unit -> PP.document

type 'a printf = ('a, out_channel, unit) format -> 'a

type ('a, 'b) printf_fatal = ('a, out_channel, unit, 'b) format4 -> 'a

type level =
  | Base
      (** The actual output. The only thing printed in quiet mode.
          Should only appear in CLI modules *)
  | Err  (** An error message *)
  | Warn  (** An warning *)
  | Info  (** Detailed on what happening *)
  | Debug  (** Everything happening *)

val pp_level : level -> PP.document

(** Convert level to string (Base is the empty string) *)
val level_to_string : level -> string

(** Set a default level to all the module. Erase local customized levels *)
val set_default_level : level -> unit

(** Set level of a module by name *)
val set_level : string -> level -> unit

(** Functors can't take string as parameters but they can take a module of this type *)
module type String = sig
  val str : string
end

(** This declare a logger. The string parameter is the logger name.
    It should be the OCaml module name.

    This module provide a lot of logging function.
    The function with d suffix take a {!ldoc} instead of a format string
    to log complex data structures.
*)
module Logger (S : String) : sig
  (** Override the level of this logger. It may still be overridden by the command line *)
  val set_level : level -> unit

  (** Log a specific level using a format string *)
  val log : level -> 'a printf

  (** Log a fatal problem with format string then shutdown with code 1 *)
  val log_fatal : code:int -> level -> ('a, 'b) printf_fatal

  (** Base command line output asked for by the CLI *)
  val base : 'a printf

  (** Failure due to external circumstances, exit with code 1 *)
  val fail : ('a, 'b) printf_fatal

  (** Declare a non-fatal internal error *)
  val err : 'a printf

  (** Declare a fatal internal error then exit with code 2 *)
  val fatal : ('a, 'b) printf_fatal

  (** Raise a warning *)
  val warn : 'a printf

  (** Print an information about what's happening *)
  val info : 'a printf

  (** Print debugging information about what's happening *)
  val debug : 'a printf

  (*****************************************************************************)
  (*        PP.document version                                                *)
  (*****************************************************************************)

  (** Log a specific level using a lazy PP.document *)
  val logd : level -> ldoc -> unit

  (** Log a fatal problem with a l then shutdown with code 1 *)
  val logd_fatal : code:int -> level -> ldoc -> 'a

  (** Base command line output asked for by the CLI *)
  val based : ldoc -> unit

  (** Failure due to external circumstances, exit with code 1 *)
  val faild : ldoc -> 'a

  (** Declare a non-fatal internal error *)
  val errd : ldoc -> unit

  (** Declare a fatal internal error then exit with code 2 *)
  val fatald : ldoc -> 'a

  (** Raise a warning *)
  val warnd : ldoc -> unit

  (** Print an information about what's happening *)
  val infod : ldoc -> unit

  (** Print debugging information about what's happening *)
  val debugd : ldoc -> unit
end
