(** This is the user configuration module.
    It is pulled from the root config.ml file or the default_config.ml file

    All configuration option are specified here.
*)

(* Please ensure the documentation of this file matches the documentation in default_config.ml *)

(** Default config file *)
val config_file : string

(** Default Z3 command. (Can be overidden with Z3_PATH and --z3) *)
val z3 : string

(** Default Isla command. (Can be overidden with ISLA_CLIENT and --isla) *)
val isla_client : string

(** The default architecture to pick when none is specified *)
val default_arch : string

(** The Architecture module. They are in src/archs. *)
module Arch : ArchSig.S

(** Whether to enable backtraces or not *)
val enable_backtrace : bool

(** Whether to enable internal unit test and the test test sub-command *)
val enable_tests : bool
