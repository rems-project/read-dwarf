(* -*- tuareg -*- *)

(* The default configuration file. Copy this file to "config.ml" and edit it to change the build
   configuration. Only edit this file to change the default configuration on git checkout. *)

(** Default config file *)
let config_file = "config.toml"

(** Default Z3 command. (Can be overidden with Z3_PATH and [--z3]) *)
let z3 = "z3"

(** Default Isla command. (Can be overidden with ISLA_CLIENT and [--isla]) *)
let isla_client = "isla-client"

(** The default architecture to pick when none is specified *)
let default_arch = "aarch64"

(** The Architecture module. They are in [src/archs]. *)
module Arch = AArch64

(** Whether to enable backtraces or not *)
let enable_backtrace = true

(** Whether to enable internal unit test and the test test sub-command *)
let enable_tests = true
