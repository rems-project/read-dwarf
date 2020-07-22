(** This is the compile-time configuration module.
    It is pulled from the root [config.ml] file or will fallback to the [default_config.ml] file

    All compile-time configuration options are specified here. For runtime configuration,
    look at {!ConfigFile}.

    Since all the module depending on {!Arch} depend on this,
    There is also a {!ConfigPre} module which is the same but without the [module Arch] definition.
*)

(* Please ensure the documentation of this file matches the documentation in default_config.ml *)

(** Default config file *)
val config_file : string

(** Default Z3 command. (Can be overidden with Z3_PATH and [--z3]) *)
val z3 : string

(** Default Isla command. (Can be overidden with ISLA_CLIENT and [--isla]) *)
val isla_client : string

(** The default architecture to pick when none is specified *)
val default_arch : string

(** The Architecture module. They are in [src/archs]. *)
module Arch : ArchSig.S

(** Whether to enable backtraces or not *)
val enable_backtrace : bool

(** Whether to enable internal unit test and the test test sub-command *)
val enable_tests : bool
