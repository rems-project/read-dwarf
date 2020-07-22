(** This module is to handle the configuration file of the program.

    For now I expect a TOML format and use the [toml] library from opam. The data structure
    themselves are format agnostic so if we change the TOML library of change of format, only this
    file must be modified. For example to JSON or YAML.

    The top level usage is to first set the {!file} reference which will be done by
    {!CommonOpt.config} using {!ensure_loaded}.
    Then the config can be accessed with the various {!section:acc}
*)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Configuration structure }

    All field of records in this section have a matching line in the checked-in
    [config.toml]. If something is optional here, and should be disabled in the
    default [config.toml], put a commented line there, so that the
    correspondence can be immediately seen.*)

module ArchConf : sig
  (** This module provides the current isla configuration.
       *)
  module Isla : sig
    (** Isla configuration option.
        Everything in here is salient for cache coherency which means that if any option
        if changed here, the whole {!IslaCache} is invalidated.
        This is checked with {!digest}.
    *)
    type t = {
      ignored_regs : string list;  (** The list of register to be ignored *)
      arch_file : string;
          (** The name of the architecture.ir file to use.
              This is the file that was compiled with [isla-sail] from
              the sail source. Alternatively this file can be found in the
              {{:https://github.com/rems-project/isla-snapshots}[isla-snapshots]}
              respository. *)
      arch_file_digest : string;
          (** The digest of the file in {!field:arch_file}.
              This will be used instead of {!field:arch_file} by {!digest}. *)
      linearize : string list;
          (** List of sail function to be linearized.
              That means that if there is a control-flow branching in that
              function, instead of generating multiple traces, isla will generate
              a single trace with it-then-else expression *)
      config_registers : (string * bool) list;
          (** List of boolean sail configuration register and the value that
              they should be set too.*)
      other_opts : string list;
          (** List of other option to pass to isla. Ideally if an option is to
              be easily used, a new field of {!t} should be created, but for
              quick and dirty testing, this field can be used. *)
    }

    (** Produces a digest of the Isla configuration for invalidating
        the {!IslaCache} when some configuration parameter changes *)
    val digest : t -> string
  end

  (** The list of all architecture specific configuration options *)
  type t = {
    arch : ArchType.t;  (** The architecture targeted by this record *)
    toolchain : string;
        (** The toolchain prefix for using this architecture with
           GNU binutils like [objdump] and [as]. *)
    isla : Isla.t;  (** The isla configuration for this architecture *)
  }
end

module Z3 : sig
  type t = {
    timeout : int option;  (** Timeout for individual requests in milliseconds *)
    memory : int option;  (** Maximum memory, in MegaBytes *)
    simplify_opts : (string * bool) list;  (** List of option for the simplify command *)
  }
end

(** Each architecture specific configuration is specified by a TOML section
    represented by a {!Arch.t} record.
    They are stored in this type *)
type archs_type = (ArchType.t, ArchConf.t) Hashtbl.t

type t = {
  arch : ArchType.t option;
      (** The default architecture to be choosen when no ELF file is specified on the CLI.
          This is optional, but if supplied, it will override {!Config.default_arch} *)
  archs : archs_type;  (** All the architecture specific configurations *)
  z3 : Z3.t;  (** The Z3 configuration *)
}

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 File loading }

    This section is about loading the configuration from a TOML file *)

(** Load the configuration file from the specified file name *)
val load : string -> unit

(** If the configuration file is not already loaded, load it from the specified file.*)
val ensure_loaded : string -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:acc Accessors }

    The section provide various accessor to access directly part of the configuration

    The config must be loaded otherwise {!UnloadedConfig} will be thrown *)

(** This exception will be raised if any of the accessor is called
    while the config is not loaded. *)
exception UnloadedConfig

(** Get all the configuration information *)
val get_config : unit -> t

(** Get the default architecture name to be used if no architecture is implicitly
    or explicitly specified on the CLI *)
val get_arch_name : unit -> ArchType.t

(** Get the architecture configuration for a specific architecture.
    To get the architecture configuration of the currently enabled architecture,
    Call {!Arch.get_config} *)
val get_arch_config : ArchType.t -> ArchConf.t

(** Get the isla configuration for a specific architecture.
    To get the isla configuration of the currently enabled architecture,
    Call {!Arch.get_isla_config} *)
val get_isla_config : ArchType.t -> ArchConf.Isla.t

(** Get the Z3 configuration *)
val get_z3_config : unit -> Z3.t
